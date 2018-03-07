//===-- godumpspec.cpp - C->Go helper utility for llvm --------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This program is a helper for the libgo build. Given an object file
// and macros file derived from a given C source file, emit Go translations
// for the types/constants/macros in the C file.
//
// Expected usage mode looks something like this:
//
//   % cc -E -dM -o somefile-macros.txt somefile.c
//   % cc -g -c -o somefile.o somefile.c
//   % llvm-godumpspec -object somefile.o \
//         -macrotmp somefile-macros.txt \
//         -output somefile-types-and-macros.go
//
// The tool reads DWARF from 'somefile.o' and combines the type/var/constant
// info from the DWARF with macro definitions from 'somefile-macros.txt'
// to produce Go equivalents for the type/var/constant info in the original
// C source file.
//
//===----------------------------------------------------------------------===//

#include "llvm/DebugInfo/DIContext.h"
#include "llvm/DebugInfo/DWARF/DWARFContext.h"
#include "llvm/Object/Binary.h"
#include "llvm/Object/ObjectFile.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/Format.h"
#include "llvm/Support/ManagedStatic.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/PrettyStackTrace.h"
#include "llvm/Support/Signals.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/ToolOutputFile.h"
#include "llvm/Support/raw_ostream.h"

#include "macro-parser.h"

#include <unordered_set>
#include <unordered_map>
#include <iostream>
#include <fstream>
#include <sstream>
#include <string>

using namespace llvm;
using namespace object;

namespace {

static cl::opt<std::string>
InputObjectFile("object", cl::desc("Object file for *.c file"));

static cl::opt<std::string>
InputMacrosFile("macrotmp", cl::desc("Macros file for *.c file"));

static cl::opt<std::string>
OutputFilename("output", cl::desc("Output file to write."));

static cl::opt<unsigned>
PointerSize("pointersize", cl::desc("Size of a pointer in bytes for "
                                    "the target architecture of interest. "
                                    "Defaults to host pointer size."),
            cl::init(sizeof(void*)));

static cl::opt<bool>
Trace("trace", cl::desc("Enable debug trace output."));

} // namespace

// At various points we have to decide whether to use the previously
// established DWARF name for a type, or emit it inline.
typedef enum {
  TN_SelectDefault,
  TN_PreferName,
  TN_AvoidName
} TypeNameDisp;

// This helper / mix-in class provides helper routines for capturing
// intermediate results via a buffer.

class DumpManager {
 public:

  // Create a new dump manager, passing it an output ostream.
  explicit DumpManager(raw_ostream &os);

 protected:

  // Output stream
  raw_ostream &os() { return os_; }

  // Set up a temporary string buffer (accessed via 'buf()' for
  // building up intermediate results).
  void initBuf() {
    if (buf_.get())
      buf_->flush();
    str_.reset(new std::string);
    buf_.reset(new llvm::raw_string_ostream(*str_.get()));
  }

  // Return a reference to the current intermediate results buffer.
  llvm::raw_string_ostream &buf() {
    assert(buf_.get() != nullptr);
    return *buf_.get();
  }

  // Save/restore the current intermediate results buffer.
  // Occasionally when processing a given type there is a need to
  // pause emission while visiting a sub-type or field, which these
  // methods enable. Here 'pauseBuf' returns the current intermediate
  // buffer state (which the caller can then cache away somewhere) and
  // re-initializes things with a call to initBuf(); restoreBuf takes
  // the specified string/buf and resets the buffer using those
  // objects (any current contents of the intermediate results buffer
  // are lost).
  std::pair<raw_string_ostream *, std::string *> pauseBuf();
  void restoreBuf(raw_string_ostream *stream, std::string *str);

  // Determines whether a given token is a Go language keyword.
  bool isGoKeyWord(const char *str) {
    return keywords_.find(str) != keywords_.end();
  }

 private:
  std::unordered_set<std::string> keywords_;
  std::unique_ptr<std::string> str_;
  std::unique_ptr<llvm::raw_string_ostream> buf_;
  raw_ostream &os_;  // output file we're writing
};

DumpManager::DumpManager(raw_ostream &os)
    : keywords_({"break", "default", "func", "interface", "select",
            "case", "defer", "go", "map", "struct", "chan", "else",
            "goto", "package", "switch", "const", "fallthrough", "if",
            "range", "type", "continue", "for", "import", "return", "var"}),
      os_(os)
{
}

// Pause output buffering, saving off current state to 'saveTo'. Returns
// the state of the current buffer (which the client can presumbably
// stash away and later pass to restoreBuf).

std::pair<raw_string_ostream *, std::string *> DumpManager::pauseBuf()
{
  raw_string_ostream *r1 = buf_.release();
  std::string *r2 = str_.release();
  initBuf();
  return std::make_pair(r1, r2);
}

// Restore buffer using previously capturede state from pauseBuf.

void DumpManager::restoreBuf(raw_string_ostream *stream, std::string *str)
{
  buf_.reset(stream);
  str_.reset(str);
}

// This class manages the overall process of generating Go code from
// DWARF and macro info derived from a C compilation. It walks the
// DWARF DIE chain from an object file we're looking at, and manages
// the process of combining DWARF type info with definitions from a
// macro temp file. Expected use here is to construct a helper, then
// call the readDwarf() method, then read + process any macro
// definitions, and finally called the emit() method.

class GoDumpHelper : public MacroParser, public DumpManager {
 public:
  explicit GoDumpHelper(raw_ostream &os);
  void readDwarf(DWARFCompileUnit *cu);
  void emit();

 private:
  // Visit a type. Each type should be visited twice, first as part of
  // a discovery/analysis phase (with emit_ == false) and then as part
  // of an output phase (eith emit_ == true).
  void visitType(const DWARFDie &die);

  // Emit a variable.
  void emitVariable(const DWARFDie &die);

  // Record a given DWARF DIE for additional processing.
  void enqueueType(const DWARFDie &die);
  void enqueueVariable(const DWARFDie &die);

  // Visit the specified DWARF type DIE, generateing a Go version
  // of the type into the intermediate results buffer.
  bool generateType(const DWARFDie &die, TypeNameDisp disp = TN_SelectDefault);

  // Similar to the above, but returns the Go code as a string without
  // appending anything to the current buffer.
  std::pair<bool, std::string> generateTypeToString(const DWARFDie &die);

  // Helpers to take care of specific type flavors.
  bool generateBaseType(const DWARFDie &die);
  bool generateStructType(const DWARFDie &die);
  bool generateUnionType(const DWARFDie &die);
  bool generateFcnType(const DWARFDie &die);
  bool generateArrayType(const DWARFDie &die);
  bool generateEnumType(const DWARFDie &die);

  // Postprocess a struct member.
  bool generateMember(const DWARFDie &die);

  // Assorted helpers.
  bool useTypeName(const DWARFDie &die, TypeNameDisp disp);
  bool isPtrToFunctionType(const DWARFDie &die);
  bool isSuitableArrayDimTyp(const DWARFDie &die);
  bool isSpuriousTypedef(const DWARFDie &die);
  bool isBitField(const DWARFDie &die);
  bool isAggregate(const DWARFDie &die);
  const char *dieName(DWARFDie die);
  DWARFDie forwardedType(DWARFDie die);
  std::string enumLitString(DWARFFormValue &fvalue);

  bool isInvalidType(const DWARFDie &die) {
    return invalidTypes_.find(die.getOffset()) != invalidTypes_.end();
  }
  bool isBaseType(const DWARFDie &die) {
    return die.getTag() == dwarf::DW_TAG_base_type;
  }

  bool typeSizeKnown(const DWARFDie &die) {
    return typeSize_.find(die.getOffset()) != typeSize_.end();
  }

  uint64_t typeSize(const DWARFDie &die) {
    auto it = typeSize_.find(die.getOffset());
    assert(it != typeSize_.end());
    return it->second;
  }
  uint64_t typeOfSize(const DWARFDie &die) {
    DWARFDie typ = die.getAttributeValueAsReferencedDie(dwarf::DW_AT_type);
    assert(typ.isValid());
    auto it = typeSize_.find(typ.getOffset());
    assert(it != typeSize_.end());
    return it->second;
  }

  void setTypeSize(const DWARFDie &die, uint64_t siz) {
    auto it = typeSize_.find(die.getOffset());
    if (it != typeSize_.end()) {
      assert(siz == it->second);
    } else {
      typeSize_[die.getOffset()] = siz;
    }
  }

  bool typeAlignKnown(const DWARFDie &die) {
    return typeAlign_.find(die.getOffset()) != typeAlign_.end();
  }

  uint64_t typeAlign(const DWARFDie &die) {
    auto it = typeAlign_.find(die.getOffset());
    if (it != typeAlign_.end())
      return it->second;
    assert(isInvalidType(die));
    return 1;
  }

  void setTypeAlign(const DWARFDie &die, uint64_t aln) {
    auto it = typeAlign_.find(die.getOffset());
    if (it != typeAlign_.end()) {
      assert(aln == it->second);
    } else {
      typeAlign_[die.getOffset()] = aln;
    }
  }

 private:
  // Names of types emitted. To avoid clases between macros + types.
  std::unordered_set<std::string> emittedTypeNames_;

  // To detect cycles in a type graph. Indexed by DIE offset.
  std::unordered_set<uint32_t> visited_;

  // Records types unrepresentable in Go. Indexed by DIE offset.
  std::unordered_set<uint32_t> invalidTypes_;

  // Referenced structs with no defined body (eg: "struct X;").
  // Indexed by DIE offset.
  std::unordered_set<uint32_t> externalStructs_;

  // Anonymous sub-structure types within unions. Indexed by DIE offset.
  std::unordered_set<uint32_t> anonSubstructure_;

  // Enumerated type literals. Indexed by enum literal name.
  std::unordered_map<std::string, std::string> enumLiterals_;

  // Type size and alignment requirement. Indexed by DIE offset.
  std::unordered_map<uint32_t, uint64_t> typeSize_;
  std::unordered_map<uint32_t, uint32_t> typeAlign_;

  // Queue of interesting DIEs to examine.
  std::vector<uint32_t> queue_;

  // Current DWARF compilation unit.
  DWARFCompileUnit *cu_; // current compilation unit

  // DWARF die offset of top-level type DIE being visited.
  uint32_t curDieOffset_;

  // Count of pad bytes used while processing bitfields.
  uint32_t padcount_;

  // Pointer size in bytes.
  uint32_t ptrSize_;

  // Set initially to false while we examine all type info, then set to
  // true for a second pass through to emit types.
  bool emit_;
};

constexpr uint32_t invalidOffset = ((unsigned)-1);

GoDumpHelper::GoDumpHelper(raw_ostream &os)
    : DumpManager(os),
      curDieOffset_(invalidOffset),
      padcount_(0),
      ptrSize_(PointerSize),
      emit_(false)
{
}

const char *GoDumpHelper::dieName(DWARFDie die)
{
  auto formval = die.find(dwarf::DW_AT_name);
  if (!formval)
    return nullptr;
  auto cstr = formval->getAsCString();
  if (!cstr)
    return nullptr;
  return *cstr;
}

void GoDumpHelper::enqueueType(const DWARFDie &die)
{
  queue_.push_back(cu_->getDIEIndex(die));
  visitType(die);
}

void GoDumpHelper::enqueueVariable(const DWARFDie &die)
{
  queue_.push_back(cu_->getDIEIndex(die));
  DWARFDie typ = die.getAttributeValueAsReferencedDie(dwarf::DW_AT_type);
  assert(typ.isValid());
  visitType(typ);
}

// Walk the DWARF DIE chain for the specified compilation unit,
// queuing up interesting DIEs for later post-processing. As each type
// or variable is enqueued we'll visit the type associated with it, so
// as to discover invalid types and establish the size/alignment of
// all interesting types.

void GoDumpHelper::readDwarf(DWARFCompileUnit *cu)
{
  assert(cu);
  cu_ = cu;
  for (const auto &entry : cu_->dies()) {
    DWARFDie die(cu_, &entry);
    if (isType(die.getTag()) &&
        (dieName(die) != nullptr ||
         die.getTag() == dwarf::DW_TAG_enumeration_type))
      enqueueType(die);
    else if (die.getTag() == dwarf::DW_TAG_variable)
      enqueueVariable(die);
    if (Trace)
      die.dump();
  }
}

void GoDumpHelper::visitType(const DWARFDie &die)
{
  // Skip base types at the top level (they will be emitted inline
  // where needed).
  if (isBaseType(die))
    return;

  // Skip spurious typedefs ("type X X"), which crop up a fair
  // amount with structs (ex: "typedef struct A { ... } A;").
  if (isSpuriousTypedef(die))
    return;

  if (Trace) {
    std::cerr << "visit offset " << std::hex << die.getOffset();
    if (dieName(die))
      std::cerr << " " << dieName(die);
    std::cerr << "\n";
  }

  initBuf();
  const char *cname = dieName(die);
  curDieOffset_ = die.getOffset();
  padcount_ = 0;
  if (emit_)
    visited_.clear();
  bool ok = generateType(die);
  curDieOffset_ = invalidOffset;

  if (emit_ && cname != nullptr) {
    if (! ok)
      os() << "// ";
    else
      emittedTypeNames_.insert(cname);
    os() << "type _" << cname << " " << buf().str();
    os() << "\n";

    if (ok) {
      // For struct and union types, emit a size constant
      DWARFDie fwd(forwardedType(die));
      if (fwd.getTag() == dwarf::DW_TAG_structure_type ||
          fwd.getTag() == dwarf::DW_TAG_union_type) {
        assert(typeSizeKnown(fwd));
        os() << "const _sizeof_" << cname << " = " << typeSize(fwd) << "\n";
      }
    }
  }
}

static const char *bitsTag(unsigned byteSize) {
  switch(byteSize) {
    case 1: return "8";
    case 2: return "16";
    case 4: return "32";
    case 8: return "64";
    case 16: return "128";
  }
  return nullptr;
}

std::string GoDumpHelper::enumLitString(DWARFFormValue &fvalue)
{
  std::stringstream ss;
  auto uval = fvalue.getAsUnsignedConstant();
  auto sval = fvalue.getAsSignedConstant();
  if (uval) {
    ss << *uval;
  } else if (sval) {
    ss << *sval;
  }
  return ss.str();
}

bool GoDumpHelper::generateEnumType(const DWARFDie &die)
{
  // Enumerated types wind up as simple uint's in Go.
  auto byteSize = dwarf::toUnsigned(die.find(dwarf::DW_AT_byte_size));
  assert(byteSize);
  const char *bits = bitsTag(*byteSize);
  if (!bits)
    return false;
  setTypeAlign(die, *byteSize);
  buf() << "uint" << bits;

  // Our overall goal is to have enumeration types trump macro
  // definitions; to enable this, macros and enum literals are
  // buffered up and then combined/reconciled as part of the
  // emit process.
  bool rval = true;
  DWARFDie child = die.getFirstChild();
  while (child && !child.isNULL()) {
    if (child.getTag() == dwarf::DW_TAG_enumerator) {
      const char *name = dieName(child);
      // FIXME: avoid clash with Go keywords here?
      auto val = child.find(dwarf::DW_AT_const_value);
      assert(val);
      std::string s = enumLitString(*val);
      if (s.empty())
        rval = false;
      else {
        std::string n(name);
        if (enumLiterals_.find(n) == enumLiterals_.end()) {
          enumLiterals_[n] = s;
          addEnumLiteralPseudoMacro(n, s);
        }
      }
    }
    child = child.getSibling();
  }

  return rval;
}

bool GoDumpHelper::isSpuriousTypedef(const DWARFDie &die)
{
  if (die.getTag() != dwarf::DW_TAG_typedef)
    return false;

  // For C constructs such as "typedef struct X { ... } X;" in the
  // DWARF we'll see first a struct type with named type X, followed
  // by a typedef type with name X, which would result in "type X X",
  // which is not what we want.
  DWARFDie tgtDie = die.getAttributeValueAsReferencedDie(dwarf::DW_AT_type);
  if (!tgtDie.isValid())
    return false;
  const char *toname = dieName(tgtDie);
  if (toname) {
    const char *fromname = dieName(die);
    if (fromname && !strcmp(fromname, toname))
      return true;
  }
  return false;
}

bool GoDumpHelper::isPtrToFunctionType(const DWARFDie &die)
{
  if (die.getTag() != dwarf::DW_TAG_pointer_type)
    return false;
  DWARFDie toDie = die.getAttributeValueAsReferencedDie(dwarf::DW_AT_type);
  if (! toDie.isValid())
    return false;
  return toDie.getTag() == dwarf::DW_TAG_subroutine_type;
}

bool GoDumpHelper::isSuitableArrayDimTyp(const DWARFDie &die)
{
  // FIXME: no support yet for enumerated type as array dim.
  if (!isBaseType(die))
    return false;
  auto byteSize = dwarf::toUnsigned(die.find(dwarf::DW_AT_byte_size));
  assert(byteSize);
  if (*byteSize < 1 || *byteSize > 8)
    return false;
  auto encoding = dwarf::toUnsigned(die.find(dwarf::DW_AT_encoding));
  assert(encoding);
  if (*encoding != dwarf::DW_ATE_signed &&
      *encoding != dwarf::DW_ATE_unsigned_char &&
      *encoding != dwarf::DW_ATE_signed_char &&
      *encoding != dwarf::DW_ATE_unsigned)
    return false;
  return true;
}

bool GoDumpHelper::generateArrayType(const DWARFDie &die)
{
  bool rval = true;

  DWARFDie eltyp = die.getAttributeValueAsReferencedDie(dwarf::DW_AT_type);
  assert(eltyp.isValid());
  std::pair<bool, std::string> eresult =
      GoDumpHelper::generateTypeToString(eltyp);
  if (! eresult.first)
    rval = false;
  std::string etgen(eresult.second);
  setTypeAlign(die, typeAlign(eltyp));

  DWARFDie child = die.getFirstChild();
  uint64_t totElements = 0;
  bool zeroDim = false;
  while (child && !child.isNULL()) {
    if (child.getTag() == dwarf::DW_TAG_subrange_type) {
      DWARFDie ctyp = child.getAttributeValueAsReferencedDie(dwarf::DW_AT_type);
      if (!ctyp.isValid()) {
        // This corresponds to "[0]"
        buf() << "[0]";
        zeroDim = true;
      } else {
        if (! isSuitableArrayDimTyp(ctyp))
          rval = false;
        // NB: don't expect to see a lower bound here or non-constant
        // upper bound.
        auto ubval = child.find(dwarf::DW_AT_upper_bound);
        auto count = child.find(dwarf::DW_AT_count);
        if (ubval) {
          auto cval = ubval->getAsUnsignedConstant();
          assert(cval);
          buf() << "[" << *cval << "+1]";
          totElements = (totElements ? (*cval+1) * totElements : (*cval+1));
        } else if (count) {
          auto cval = count->getAsUnsignedConstant();
          assert(cval);
          buf() << "[" << *cval << "]";
          totElements = (totElements ? *cval * totElements : *cval);
        } else {
          // This corresponds to "[0]"
          buf() << "[0]";
          zeroDim = true;
        }
      }
    }
    child = child.getSibling();
  }
  if (zeroDim)
    totElements = 0;

  buf() << etgen;

  // NB: array types may be lacking a byte size attribute. If so, set
  // size manually.
  auto byteSize = dwarf::toUnsigned(die.find(dwarf::DW_AT_byte_size));
  if (!byteSize)
    setTypeSize(die, totElements * typeSize(eltyp));

  return rval;
}

bool GoDumpHelper::generateFcnType(const DWARFDie &die)
{
  bool rval = true;

  // Params
  buf() << "func(";
  bool com = false;
  for (DWARFDie child : die.children()) {
    if (com)
      buf() << ", ";
    DWARFDie ctyp = child.getAttributeValueAsReferencedDie(dwarf::DW_AT_type);
    assert(ctyp.isValid());
    if (!generateType(ctyp))
      rval = false;
    com = true;
  }
  buf() << ") ";

  // Return type
  DWARFDie rtyp = die.getAttributeValueAsReferencedDie(dwarf::DW_AT_type);
  if (rtyp.isValid()) {
    if (!generateType(rtyp))
      rval = false;
  }

  // From a practical perspective the only function types of interest
  // for us are pointer-to-function types, so here we create fictional
  // values for type and alignment of this type (this makes things easier
  // if this DIE is the target of a typedef).
  setTypeSize(die, 0);
  setTypeAlign(die, 0);

  return rval;
}

bool GoDumpHelper::generateMember(const DWARFDie &die)
{
  bool rval = true;

  const char *name = dieName(die);
  bool anonSub = false;
  if (!name) {
    // This corresponds to an anonymous sub-union, e.g. something like
    //
    //  struct x {
    //    union { int q; double z; };
    //    ...
    //  }
    //
    // From the compiler's point of view, "q" and "z" are effectively
    // fields within x, which has to be reflected in the generated Go code.
    // Note: to make matters more complicated, anonymous structures are
    // also allowed. Example:
    //
    //     union { struct { int x; double z; int y; };
    //             struct { char c4[4];
    //                      struct { double quix; char kkk; }; double k; }; };
    //
    // For the oddball above, each of the nested fields (ex: kkk) is
    // considered by the compiler to be a child of the top-level union (in
    // terms of how a user would reference it).
    DWARFDie ctyp = die.getAttributeValueAsReferencedDie(dwarf::DW_AT_type);
    assert(ctyp.isValid());
    assert(ctyp.getTag() == dwarf::DW_TAG_union_type ||
           ctyp.getTag() == dwarf::DW_TAG_structure_type);
    anonSubstructure_.insert(ctyp.getOffset());
    anonSub = true;
  } else {
    assert(name);
    if (isGoKeyWord(name))
      buf() << "_";
    buf() << name << " ";
  }
  auto bitSize = die.find(dwarf::DW_AT_bit_size);
  if (bitSize) {
    // This corresponds to the case of a bitfield whose size/alignment
    // happens to make it appear to be an integral field, e.g.
    //
    //   struct {
    //     unsigned x:16;
    //   }
    //
    // Here we want to treat 'x' as if it were a simple "unsigned
    // short" and not a bitfield.
    //
    auto bsval = bitSize->getAsUnsignedConstant();
    assert(bsval);
    buf() << "uint" << *bsval;
  } else {
    DWARFDie ctyp = die.getAttributeValueAsReferencedDie(dwarf::DW_AT_type);
    assert(ctyp.isValid());
    if (!generateType(ctyp))
      rval = false;
  }
  if (! anonSub)
    buf() << "; ";
  return rval;
}

bool GoDumpHelper::generateUnionType(const DWARFDie &die)
{
  bool rval = true;
  if (anonSubstructure_.find(die.getOffset()) == anonSubstructure_.end())
    buf() << "struct { ";
  std::pair<raw_string_ostream *, std::string *> pauseState;

  // Walk the union members. We want to emit only the first field
  // (since Go has no unions), so pause buffering after the first
  // field and resume after we are done.
  DWARFDie child = die.getFirstChild();
  uint64_t csiz = 0;
  uint64_t calign = 0;
  uint64_t maxalign = 0;
  bool firstchild = true;
  auto padcountsave = 0;
  while (child && !child.isNULL()) {
    if (child.getTag() == dwarf::DW_TAG_member) {
      // Replace bitfields with padding.
      auto bitsize = child.find(dwarf::DW_AT_bit_size);
      if (bitsize)
        continue;

      rval &= generateMember(child);
      DWARFDie ctyp = child.getAttributeValueAsReferencedDie(dwarf::DW_AT_type);
      if (firstchild) {
        calign = typeAlign(ctyp);
        csiz = typeSize(ctyp);
        pauseState = pauseBuf();
        padcountsave = padcount_;
        firstchild = false;
      }
      maxalign = std::max(typeAlign(ctyp), maxalign);
    }
    child = child.getSibling();
  }
  if (pauseState.first != nullptr) {
    padcount_ = padcountsave;
    restoreBuf(pauseState.first, pauseState.second);
  }

  // Pad out to the required size
  auto byteSize = dwarf::toUnsigned(die.find(dwarf::DW_AT_byte_size));
  assert(byteSize);
  if (csiz < *byteSize) {
    unsigned padAmt = *byteSize - csiz;
    buf() << "Godump_" << padcount_++ << "_pad [" << padAmt << "]byte; ";
  }

  // Enforce alignment
  if (maxalign > calign && maxalign > 1) {
    buf() << "Godump_" << padcount_++ << "_align [0]int"
          << bitsTag(maxalign) << "; ";
  }
  setTypeAlign(die, maxalign);

  if (anonSubstructure_.find(die.getOffset()) == anonSubstructure_.end())
    buf() << "}";
  return rval;
}

bool GoDumpHelper::isBitField(const DWARFDie &die)
{
  auto bitSize = die.find(dwarf::DW_AT_bit_size);
  if (!bitSize)
    return false;
  auto byteSize = die.find(dwarf::DW_AT_byte_size);
  auto bitOffset = die.find(dwarf::DW_AT_bit_offset);
  assert(bitSize && bitOffset);
  auto byval = byteSize->getAsUnsignedConstant();
  auto bsval = bitSize->getAsUnsignedConstant();
  auto boval = bitOffset->getAsUnsignedConstant();
  assert(byval && bsval && boval);
  if (*boval % *bsval == 0 &&
      *bsval % *byval == 0 &&
      (*bsval == 8 || *bsval == 16 || *bsval == 32 || *bsval == 64))
    return false;
  return true;
}

bool GoDumpHelper::generateStructType(const DWARFDie &die)
{
  if (anonSubstructure_.find(die.getOffset()) == anonSubstructure_.end())
    buf() << "struct { ";

  // Collect members. Note that DWARF allows the producer to include
  // other things (such as other types) as direct children of the
  // struct type DIE, so we have to allow for that possibility here.
  std::vector<DWARFDie> members;
  DWARFDie child = die.getFirstChild();
  while (child && !child.isNULL()) {
    if (child.getTag() == dwarf::DW_TAG_member) {
      members.push_back(child);
    }
    child = child.getSibling();
  }

  // Walk the members.
  uint64_t accumSize = 0;
  uint64_t maxAlign = 0;
  bool rval = true;
  bool prevBitField = false;
  for (unsigned idx = 0; idx < members.size(); ++idx) {
    auto &member = members[idx];

    // Replace bitfields with padding.
    if (isBitField(member)) {
      prevBitField = true;
      continue;
    }

    // Padding if needed
    if (idx != 0) {
      auto dml = member.find(dwarf::DW_AT_data_member_location);
      assert(dml);
      auto dmlval = dml->getAsUnsignedConstant();
      assert(dmlval);
      if (accumSize < dmlval) {
        unsigned padAmt = *dmlval - accumSize;
        if (prevBitField)
          buf() << "Godump_" << padcount_++ << "_pad [" << padAmt << "]byte; ";
        accumSize += padAmt;
      }
      prevBitField = false;
    }

    rval &= generateMember(member);
    DWARFDie mtyp = member.getAttributeValueAsReferencedDie(dwarf::DW_AT_type);
    maxAlign = std::max(maxAlign, typeAlign(mtyp));
    accumSize += typeSize(mtyp);
  }
  setTypeAlign(die, maxAlign);

  // Handle the "external" struct case, e.g. something like
  //
  //  typedef struct definedSomewhereElse btyp;
  //  typedef btyp *pbtyp;
  //  extern pbytp *p;
  //
  // There isn't a direct Go equivalent here, so emit a dummy
  // in such cases and make a record of what's happened.
  auto isdecl = die.find(dwarf::DW_AT_declaration);
  if (isdecl) {
    auto ival = isdecl->getAsUnsignedConstant();
    assert(ival);
    if (*ival) {
      externalStructs_.insert(die.getOffset());
      setTypeSize(die, 0);
      setTypeAlign(die, 0);
    }
  }

  // Padding if needed
  auto byteSize = typeSize(die);
  if (accumSize < byteSize) {
    unsigned padAmt = byteSize - accumSize;
    buf() << "Godump_" << padcount_++ << "_pad [" << padAmt << "]byte; ";
  }

  if (anonSubstructure_.find(die.getOffset()) == anonSubstructure_.end())
    buf() << "}";

  return rval;
}

bool GoDumpHelper::generateBaseType(const DWARFDie &die)
{
  auto byteSize = dwarf::toUnsigned(die.find(dwarf::DW_AT_byte_size));
  assert(byteSize);
  unsigned bytes = *byteSize;
  const char *bits = bitsTag(bytes);
  if (!bits)
    return false;

  auto encoding = dwarf::toUnsigned(die.find(dwarf::DW_AT_encoding));
  assert(encoding);
  switch(*encoding) {
    case dwarf::DW_ATE_boolean:
      setTypeAlign(die, 1);
      buf() << "bool";
      return true;
    case dwarf::DW_ATE_unsigned_char: {
      setTypeAlign(die, 1);
      assert(bytes == 1);
      buf() << "uint8";
      return true;
    }
    case dwarf::DW_ATE_signed_char: {
      setTypeAlign(die, 1);
      assert(bytes == 1);
      buf() << "int8";
      return true;
    }
    case dwarf::DW_ATE_unsigned: {
      setTypeAlign(die, bytes);
      buf() << "uint" << bits;
      return true;
    }
    case dwarf::DW_ATE_signed: {
      setTypeAlign(die, bytes);
      buf() << "int" << bits;
      return true;
    }
    case dwarf::DW_ATE_float: {
      setTypeAlign(die, bytes);
      // Go does not support float128 / long double
      if (bytes > 8)
        return false;
      buf() << "float" << bits;
      return true;
    }
    case dwarf::DW_ATE_complex_float: {
      setTypeAlign(die, bytes/2);
      buf() << "complex" << bits;
      return true;
    }
    default: {
      return false;
    }
  }
  return false;
}

DWARFDie GoDumpHelper::forwardedType(DWARFDie die)
{
  while (die.getTag() == dwarf::DW_TAG_typedef ||
         die.getTag() == dwarf::DW_TAG_volatile_type ||
         die.getTag() == dwarf::DW_TAG_const_type) {
    die = die.getAttributeValueAsReferencedDie(dwarf::DW_AT_type);
    assert(die.isValid());
  }
  return die;
}

bool GoDumpHelper::isAggregate(const DWARFDie &die)
{
  return (die.getTag() == dwarf::DW_TAG_structure_type ||
          die.getTag() == dwarf::DW_TAG_union_type ||
          die.getTag() == dwarf::DW_TAG_array_type);
}

// When generating a Go representation for a given DWARF type T
// that refers to a set of other types { T1, T2, ... TN }, at
// various points we have to decide whether to refer to a given child type
// TK via TK's names (if it has a name) or whether to emit concrete
// definition for TK. This routine helps with making that decision.

bool GoDumpHelper::useTypeName(const DWARFDie &die, TypeNameDisp disp)
{
  // Type has to have a name for us to use it.
  const char *name = dieName(die);
  if (!name)
    return false;

  // Top-level die that we're in the process of emitting?
  if (die.getOffset() == curDieOffset_)
    return false;

  // If we're in the process of visiting this type, we have to
  // use the emitted name (to avoid infinite recursion).
  if (visited_.find(die.getOffset()) != visited_.end()) {
    assert(name);
    return true;
  }

  // Don't try to use the name stored within a base type
  // (among other things, they are allowed to have spaces)
  if (isBaseType(die))
    return false;

  // On the first pass (prior to emit) walk as many types as possible.
  if (!emit_)
    return false;

  // Take into account preferences here.
  if (disp == TN_AvoidName)
    return false;
  if (disp == TN_PreferName)
    return true;

  // Here we try to mimic the GCC -fgo-dump-spec implementation, which
  // has specific preferences about whether/where to use a previously
  // emitted name.
  DWARFDie fwd(forwardedType(die));
  if (!isAggregate(fwd) && !isPtrToFunctionType(fwd))
    return false;

  return true;
}

bool GoDumpHelper::generateType(const DWARFDie &die, TypeNameDisp disp)
{
  // Invalid?
  if (isInvalidType(die))
    return false;

  // Record size for posterity.
  auto byteSize = dwarf::toUnsigned(die.find(dwarf::DW_AT_byte_size));
  if (byteSize)
    setTypeSize(die, *byteSize);

  // Use a reference to a previously emitted type name if appropriate.
  if (useTypeName(die, disp)) {
    const char *name = dieName(die);
    assert(name);
    buf() << "_" << name;
    return true;
  }

  // Reset top-level DIE offset.
  curDieOffset_ = ((unsigned)-1);

  // Look to see what we're dealing with.
  bool rval = true;
  dwarf::Tag tag = die.getTag();
  switch(tag) {
    case dwarf::DW_TAG_base_type: {
      rval = generateBaseType(die);
      break;
    }
    case dwarf::DW_TAG_pointer_type: {
      // NB: for "void *" we may see no target type.
      DWARFDie toDie = die.getAttributeValueAsReferencedDie(dwarf::DW_AT_type);
      if (! toDie.isValid()) {
        // Treat this case as "*byte"
        buf() << "*byte";
      } else {
        if (toDie.getTag() != dwarf::DW_TAG_subroutine_type)
          buf() << "*";
        bool toDieValid = generateType(toDie);
        if (!toDieValid) {
          buf() << "byte";
        }
      }
      setTypeSize(die, ptrSize_);
      setTypeAlign(die, ptrSize_);
      break;
    }
    case dwarf::DW_TAG_typedef: {
      DWARFDie tgtDie = die.getAttributeValueAsReferencedDie(dwarf::DW_AT_type);
      // Interestingly, for a construct like:
      //
      //    typedef void MyOpaque;
      //    typedef MyOpaque *MyOpaquePointer;
      //
      // the DIE corresponding to "MyOpaque" will be a typedef with no
      // type reference attribute; handle this case accordingly.
      if (!tgtDie.isValid()) {
        rval = false;
      } else {
        rval = generateType(tgtDie, TN_AvoidName);
        setTypeAlign(die, typeAlign(tgtDie));
        setTypeSize(die, typeSize(tgtDie));
      }
      break;
    }
    case dwarf::DW_TAG_structure_type: {
      visited_.insert(die.getOffset());
      rval = generateStructType(die);
      break;
    }
    case dwarf::DW_TAG_union_type: {
      rval = generateUnionType(die);
      break;
    }
    case dwarf::DW_TAG_enumeration_type: {
      rval = generateEnumType(die);
      break;
    }
    case dwarf::DW_TAG_subroutine_type: {
      rval = generateFcnType(die);
      break;
    }
    case dwarf::DW_TAG_array_type: {
      rval = generateArrayType(die);
      break;
    }
    case dwarf::DW_TAG_const_type:
    case dwarf::DW_TAG_restrict_type:
    case dwarf::DW_TAG_volatile_type: {
      // Throw away these qualifiers.
      DWARFDie qtyp = die.getAttributeValueAsReferencedDie(dwarf::DW_AT_type);
      assert(qtyp.isValid());
      rval = generateType(qtyp);
      setTypeAlign(die, typeAlign(qtyp));
      setTypeSize(die, typeSize(qtyp));
      break;
    }
    default:
      assert(false);
  }

  if (!rval)
    invalidTypes_.insert(die.getOffset());

  return rval;
}

std::pair<bool, std::string>
GoDumpHelper::generateTypeToString(const DWARFDie &die)
{
  auto pauseState = pauseBuf();
  bool ok = generateType(die);
  std::string str(buf().str());
  restoreBuf(pauseState.first, pauseState.second);
  return std::make_pair(ok, str);
}

void GoDumpHelper::emitVariable(const DWARFDie &die)
{
  initBuf();

  DWARFDie typ = die.getAttributeValueAsReferencedDie(dwarf::DW_AT_type);
  assert(typ.isValid());
  bool ok = generateType(typ, TN_PreferName);

  // In cases where there is a clash between a named type and a variable,
  // we choose the type and skip the variable.
  const char *name = dieName(die);
  if (emittedTypeNames_.find(name) != emittedTypeNames_.end())
    ok = false;

  if (! ok)
    os() << "// ";
  assert(name);
  os() << "var _" << name << " " << buf().str() << "\n";
}

void GoDumpHelper::emit()
{
  // Tell the visit routines below to emit Go code.
  emit_ = true;

  for (auto idx : queue_) {
    DWARFDie die = cu_->getDIEAtIndex(idx);
    if (isType(die.getTag()))
      visitType(die);
    else if (die.getTag() == dwarf::DW_TAG_variable)
      emitVariable(die);
  }

  // Emit macros once we've finished with types.
  emitMacros(os(), emittedTypeNames_);
}

static void error(StringRef Prefix, std::error_code EC) {
  if (!EC)
    return;
  errs() << Prefix << ": " << EC.message() << "\n";
  exit(1);
}

static int visitMacrosFile(const std::string &infile,
                           GoDumpHelper &state,
                           raw_ostream &os)
{
  std::string line;
  std::ifstream macfile(infile);
  unsigned lno = 0;
  if (macfile.is_open()) {
    while (std::getline(macfile, line))
    {
      lno += 1;
      state.visitMacroLine(line, lno);
    }
    macfile.close();
    state.postProcessMacros();
  } else {
    errs() << "error: unable to open macro file " << infile << "\n";
    return 1;
  }

  return 0;
}

struct ObjectState {
  std::unique_ptr<MemoryBuffer> mbuf_;
  std::unique_ptr<Binary> binary_;
  std::unique_ptr<DWARFContext> dwctxt_;
};

static int visitObjectFile(const std::string &infile,
                           GoDumpHelper &state,
                           ObjectState &ostate,
                           raw_ostream &os)
{
  ErrorOr<std::unique_ptr<MemoryBuffer>> buffOrErr =
      MemoryBuffer::getFile(infile);
  error(infile, buffOrErr.getError());
  std::unique_ptr<MemoryBuffer> buffer = std::move(buffOrErr.get());
  ostate.mbuf_.reset(buffer.release());

  Expected<std::unique_ptr<Binary>> binOrErr =
      object::createBinary(*ostate.mbuf_);
  error(infile, errorToErrorCode(binOrErr.takeError()));
  std::unique_ptr<Binary> binary = std::move(binOrErr.get());
  ostate.binary_.reset(binary.release());

  // NB: no MachO support at the moment
  auto *obj = dyn_cast<ObjectFile>(ostate.binary_.get());
  if (obj == nullptr) {
    errs() << "error: problems opening object file " << infile << "\n";
    return 1;
  }
  ostate.dwctxt_.reset(DWARFContext::create(*obj).release());

  // Expect to see exactly one DWARF CU.
  if (ostate.dwctxt_->getNumCompileUnits() < 1) {
    errs() << "error: no DWARF compilation units found in " << infile << "\n";
    return 1;
  } else if (ostate.dwctxt_->getNumCompileUnits() > 1) {
    errs() << "error: unexpected multiple DWARF compilation "
           << "units found in " << infile << "\n";
    return 1;
  }

  DWARFCompileUnit *cu = ostate.dwctxt_->getCompileUnitAtIndex(0);
  state.readDwarf(cu);

  return 0;
}

int main(int argc, char **argv) {

  // Print a stack trace if we signal out.
  sys::PrintStackTraceOnErrorSignal(argv[0]);
  PrettyStackTraceProgram X(argc, argv);
  llvm_shutdown_obj Y;  // Call llvm_shutdown() on exit.

  llvm::InitializeAllTargetInfos();
  llvm::InitializeAllTargetMCs();

  cl::ParseCommandLineOptions(
      argc, argv,
      "Emit Go translation for type/const/macro information derived "
      "from compilation of a C file.\n");

  if (InputObjectFile.empty()) {
    errs() << "error: supply input object file using -object option.\n";
    return 1;
  }

  std::unique_ptr<ToolOutputFile> OutputFile;
  if (!OutputFilename.empty()) {
    std::error_code EC;
    OutputFile = llvm::make_unique<ToolOutputFile>(OutputFilename, EC,
                                                     sys::fs::F_None);
    // Don't remove output file if we exit with an error.
    OutputFile->keep();
    error("Unable to open output file" + OutputFilename, EC);
  }

  raw_ostream &OS = OutputFile ? OutputFile->os() : outs();
  GoDumpHelper state(OS);
  ObjectState ostate;

  int rc = 0;
  if (! InputObjectFile.empty()) {
    rc |= visitObjectFile(InputObjectFile, state, ostate, OS);
  }
  if (! InputMacrosFile.empty()) {
    rc |= visitMacrosFile(InputMacrosFile, state, OS);
  }
  state.emit();

  return rc;
}
