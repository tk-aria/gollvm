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
//===----------------------------------------------------------------------===//

#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/StringSet.h"
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
#include "llvm/Support/Regex.h"
#include "llvm/Support/Signals.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/ToolOutputFile.h"
#include "llvm/Support/raw_ostream.h"

#include <unordered_set>
#include <unordered_map>
#include <iostream>
#include <sstream>

using namespace llvm;
using namespace object;

namespace {
using namespace cl;

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

typedef enum {
  SelectDefaultName,
  PreferEmittedName,
  AvoidEmittedName
} EmitNameDisp;

//
// Helper class for performing a visition/walk of the DIE chain read
// from the DWARF in the object file we're examining.
//
class GoDumpHelper {
 public:
  explicit GoDumpHelper(raw_ostream &os, DWARFCompileUnit *cu);
  void walkDIEChain();
  void emit();

 private:
  bool isGoKeyWord(const char *str) {
    return keywords_.find(str) != keywords_.end();
  }

  void visitType(DWARFDie &die);
  void visitVariable(DWARFDie &die);

  // Generate a Go version for the specified type DIE. This appends
  // Go source code directly to the current buffer.
  bool generateType(DWARFDie &die, EmitNameDisp disp = SelectDefaultName);

  // Similar to the above, but returns the Go code as a string without
  // appending anything to the current buffer.
  std::pair<bool, std::string> generateTypeToString(DWARFDie &die);

  bool generateBaseType(DWARFDie &die);
  bool generateStructType(DWARFDie &die);
  bool generateUnionType(DWARFDie &die);
  bool generateFcnType(DWARFDie &die);
  bool generateArrayType(DWARFDie &die);
  bool generateEnumType(DWARFDie &die);

  bool generateMember(DWARFDie &die);

  bool useEmittedName(DWARFDie &die, EmitNameDisp disp);
  bool isPtrToFunctionType(DWARFDie &die);
  bool isSuitableArrayDimTyp(DWARFDie &die);
  bool isSpuriousTypedef(DWARFDie &die);
  bool isBitField(DWARFDie &die);

  std::string enumLitString(DWARFFormValue &fvalue);

  void initBuf() {
    str_.reset(new std::string);
    buf_.reset(new llvm::raw_string_ostream(*str_.get()));
  }
  llvm::raw_string_ostream &buf() {
    assert(buf_.get() != nullptr);
    return *buf_.get();
  }

  std::pair<raw_string_ostream *, std::string *> pauseBuf();
  void restoreBuf(raw_string_ostream *stream, std::string *str);

  bool isInvalidType(DWARFDie &die) {
    return invalidTypes_.find(die.getOffset()) != invalidTypes_.end();
  }
  bool isEmitted(DWARFDie &die) {
    return typesEmitted_.find(die.getOffset()) != typesEmitted_.end();
  }
  bool isBaseType(DWARFDie &die) {
    return die.getTag() == dwarf::DW_TAG_base_type;
  }

  bool typeSizeKnown(DWARFDie &die) {
    return typeSize_.find(die.getOffset()) != typeSize_.end();
  }

  uint64_t typeSize(DWARFDie &die) {
    auto it = typeSize_.find(die.getOffset());
    assert(it != typeSize_.end());
    return it->second;
  }
  uint64_t typeOfSize(DWARFDie &die) {
    DWARFDie typ = die.getAttributeValueAsReferencedDie(dwarf::DW_AT_type);
    assert(typ.isValid());
    auto it = typeSize_.find(typ.getOffset());
    assert(it != typeSize_.end());
    return it->second;
  }

  void setTypeSize(DWARFDie &die, uint64_t siz) {
    auto it = typeSize_.find(die.getOffset());
    if (it != typeSize_.end()) {
      assert(siz == it->second);
    } else {
      typeSize_[die.getOffset()] = siz;
    }
  }

  bool typeAlignKnown(DWARFDie &die) {
    return typeAlign_.find(die.getOffset()) != typeAlign_.end();
  }

  uint64_t typeAlign(DWARFDie &die) {
    auto it = typeAlign_.find(die.getOffset());
    assert(it != typeAlign_.end());
    return it->second;
  }

  void setTypeAlign(DWARFDie &die, uint64_t aln) {
    auto it = typeAlign_.find(die.getOffset());
    if (it != typeAlign_.end()) {
      assert(aln == it->second);
    } else {
      typeAlign_[die.getOffset()] = aln;
    }
  }

  void dump();

 private:
  raw_ostream &os_;  // output file we're writing
  DWARFCompileUnit *cu_; // current compilation unit
  uint32_t curDieOffset_; // DWARF offset of current top-level type DIE
  uint32_t padcount_;
  uint32_t ptrSize_;
  std::unique_ptr<std::string> str_;
  std::unique_ptr<llvm::raw_string_ostream> buf_;
  std::vector<uint32_t> queue_;
  std::unordered_set<std::string> keywords_;
  std::unordered_set<uint32_t> typesEmitted_;
  std::unordered_set<uint32_t> invalidTypes_;
  std::unordered_map<std::string, std::string> enumLiterals_;
  std::unordered_map<uint32_t, uint64_t> typeSize_;
  std::unordered_map<uint32_t, uint32_t> typeAlign_;
};

constexpr uint32_t invalidOffset = ((unsigned)-1);

GoDumpHelper::GoDumpHelper(raw_ostream &os, DWARFCompileUnit *cu)
    : os_(os),
      cu_(cu),
      curDieOffset_(invalidOffset),
      padcount_(0),
      ptrSize_(PointerSize),
      keywords_({"break", "default", "func", "interface", "select",
              "case", "defer", "go", "map", "struct", "chan", "else",
              "goto", "package", "switch", "const", "fallthrough", "if",
              "range", "type", "continue", "for", "import", "return", "var"})
{
}

void GoDumpHelper::dump() {
  std::cerr << "Buf: " << buf_->str() << "\n";
  std::cerr << "curDieOffset_ " << curDieOffset_ << "\n";
}

// Pause output buffering, saving off current state to 'saveTo'. Returns
//
std::pair<raw_string_ostream *, std::string *> GoDumpHelper::pauseBuf()
{
  raw_string_ostream *r1 = buf_.release();
  std::string *r2 = str_.release();
  initBuf();
  return std::make_pair(r1, r2);
}

void GoDumpHelper::restoreBuf(raw_string_ostream *stream, std::string *str)
{
  buf_.reset(stream);
  str_.reset(str);
}

void GoDumpHelper::walkDIEChain()
{
  for (const auto &entry : cu_->dies()) {
    DWARFDie die(cu_, &entry);
    if (isType(die.getTag())) {
      // queue only named types
      if (dwarf::toString(die.find(dwarf::DW_AT_name)))
        queue_.push_back(cu_->getDIEIndex(die));
    } else if (die.getTag() == dwarf::DW_TAG_variable)
      queue_.push_back(cu_->getDIEIndex(die));

    if (Trace)
      die.dump();
  }
}

void GoDumpHelper::visitType(DWARFDie &die)
{
  // Skip base types at the top level (they will be emitted inline
  // where needed).
  if (isBaseType(die))
    return;

  // Skip spurious typedefs ("type X X"), which crop up a fair
  // amount with structs (ex: "typedef struct A { ... } A;").
  if (isSpuriousTypedef(die))
    return;

  initBuf();
  typesEmitted_.insert(die.getOffset());
  curDieOffset_ = die.getOffset();
  padcount_ = 0;
  bool ok = generateType(die);
  curDieOffset_ = invalidOffset;

  if (! ok)
    os_ << "// ";
  auto name = dwarf::toString(die.find(dwarf::DW_AT_name));
  assert(name);
  os_ << "type _" << *name << " " << buf_->str();
  os_ << "\n";

  if (ok && enumLiterals_.size()) {
    for (auto lit : enumLiterals_) {
      os_ << "const _" << lit.first << " = " << lit.second << "\n";
    }
    enumLiterals_.clear();
  }
}

const char *bitsTag(unsigned byteSize) {
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

bool GoDumpHelper::generateEnumType(DWARFDie &die)
{
  // In the GCC implemetation, enumeration types trump macro definitions;
  // this is not yet implemented here.
  auto byteSize = dwarf::toUnsigned(die.find(dwarf::DW_AT_byte_size));
  assert(byteSize);
  const char *bits = bitsTag(*byteSize);
  if (!bits)
    return false;
  setTypeAlign(die, *byteSize);
  buf() << "uint" << bits;

  // Cache away literals in this enum
  bool rval = true;
  DWARFDie child = die.getFirstChild();
  while (child && !child.isNULL()) {
    if (child.getTag() == dwarf::DW_TAG_enumerator) {
      auto name = dwarf::toString(child.find(dwarf::DW_AT_name));
      assert(name);
      // FIXME: avoid clash with Go keywords here?
      auto val = child.find(dwarf::DW_AT_const_value);
      assert(val);
      std::string s = enumLitString(*val);
      if (s.empty())
        rval = false;
      else
        enumLiterals_[*name] = s;
    }
    child = child.getSibling();
  }

  return rval;
}

bool GoDumpHelper::isSpuriousTypedef(DWARFDie &die)
{
  if (die.getTag() != dwarf::DW_TAG_typedef)
    return false;

  // For C constructs such as "typedef struct X { ... } X;" in the
  // DWARF we'll see first a struct type with named type X, followed
  // by a typedef type with name X, which would result in "type X X",
  // which is no what we want.
  DWARFDie tgtDie = die.getAttributeValueAsReferencedDie(dwarf::DW_AT_type);
  assert(tgtDie.isValid());
  auto toname = dwarf::toString(tgtDie.find(dwarf::DW_AT_name));
  if (toname) {
    auto fromname = dwarf::toString(die.find(dwarf::DW_AT_name));
    if (fromname && !strcmp(*fromname, *toname))
      return true;
  }
  return false;
}

bool GoDumpHelper::isPtrToFunctionType(DWARFDie &die)
{
  if (die.getTag() != dwarf::DW_TAG_pointer_type)
    return false;
  DWARFDie toDie = die.getAttributeValueAsReferencedDie(dwarf::DW_AT_type);
  if (! toDie.isValid())
    return false;
  return toDie.getTag() == dwarf::DW_TAG_subroutine_type;
}

bool GoDumpHelper::isSuitableArrayDimTyp(DWARFDie &die)
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
      *encoding != dwarf::DW_ATE_signed_char &&
      *encoding != dwarf::DW_ATE_unsigned)
    return false;
  return true;
}

bool GoDumpHelper::generateArrayType(DWARFDie &die)
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
      assert(ctyp.isValid());
      if (! isSuitableArrayDimTyp(ctyp))
        rval = false;
      // NB: don't expect to see a lower bound here or non-constant upper bound
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

bool GoDumpHelper::generateFcnType(DWARFDie &die)
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

  return rval;
}

bool GoDumpHelper::generateMember(DWARFDie &die)
{
  bool rval = true;
  auto name = dwarf::toString(die.find(dwarf::DW_AT_name));
  assert(name);
  if (isGoKeyWord(*name))
    buf() << "_";
  buf() << *name << " ";
  auto bitSize = die.find(dwarf::DW_AT_bit_size);
  if (bitSize) {
    // This corresponds to the case of a bitfield whose size/alignment
    // happens to make it appear to be an integral field, e.g.
    // struct {
    //   unsigned x:16;
    //   unsigned y:16;
    // }
    auto bsval = bitSize->getAsUnsignedConstant();
    assert(bsval);
    buf() << "uint" << *bsval;
  } else {
    DWARFDie ctyp = die.getAttributeValueAsReferencedDie(dwarf::DW_AT_type);
    assert(ctyp.isValid());
    if (!generateType(ctyp))
      rval = false;
  }
  buf() << "; ";
  return rval;
}

bool GoDumpHelper::generateUnionType(DWARFDie &die)
{
  bool rval = true;
  buf() << "struct { ";
  std::pair<raw_string_ostream *, std::string *> pauseState;

  // Walk the union members. We want to emit only the first
  // field (since Go has no unions), so pause buffering after the first
  // field and resume after we are done.
  DWARFDie child = die.getFirstChild();
  uint64_t csiz = 0;
  uint64_t calign = 0;
  uint64_t maxalign = 0;
  bool firstchild = true;
  auto padcountsave = 0;
  while (child && !child.isNULL()) {
    if (child.getTag() == dwarf::DW_TAG_member) {
      // No bitfields please -- we replace them with padding.
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

  buf() << "}";
  return rval;
}

bool GoDumpHelper::isBitField(DWARFDie &die)
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

bool GoDumpHelper::generateStructType(DWARFDie &die)
{
  buf() << "struct { ";

  // Collect members. Note that DWARF allows the producer to include
  // other things (like types) as direct children of the struct type.
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

  // Padding if needed
  auto byteSize = typeSize(die);
  if (accumSize < byteSize) {
    unsigned padAmt = byteSize - accumSize;
    buf() << "Godump_" << padcount_++ << "_pad [" << padAmt << "]byte; ";
  }

  buf() << "}";

  // FIXME: handle alignment and padding
  return rval;
}

bool GoDumpHelper::generateBaseType(DWARFDie &die)
{
  auto byteSize = dwarf::toUnsigned(die.find(dwarf::DW_AT_byte_size));
  assert(byteSize);
  const char *bits = bitsTag(*byteSize);
  if (!bits)
    return false;

  auto encoding = dwarf::toUnsigned(die.find(dwarf::DW_AT_encoding));
  assert(encoding);
  switch(*encoding) {
    case dwarf::DW_ATE_boolean:
      setTypeAlign(die, 1);
      buf() << "bool";
      return true;
    case dwarf::DW_ATE_signed_char: {
      setTypeAlign(die, 1);
      assert(*byteSize == 1);
      buf() << "int8";
      return true;
    }
    case dwarf::DW_ATE_unsigned: {
      setTypeAlign(die, *byteSize);
      buf() << "uint" << bits;
      return true;
    }
    case dwarf::DW_ATE_signed: {
      setTypeAlign(die, *byteSize);
      buf() << "int" << bits;
      return true;
    }
    case dwarf::DW_ATE_float: {
      setTypeAlign(die, *byteSize);
      buf() << "float" << bits;
      return true;
    }
    case dwarf::DW_ATE_complex_float: {
      setTypeAlign(die, *byteSize/2);
      buf() << "complex" << bits;
      return true;
    }
    default: {
      return false;
    }
  }
  return false;
}

bool GoDumpHelper::useEmittedName(DWARFDie &die, EmitNameDisp disp)
{
  // Not yet emitted?
  if (!isEmitted(die))
    return false;

  // Top-level die that we're in the process of emitting?
  if (die.getOffset() == curDieOffset_)
    return false;

  // No name?
  auto name = dwarf::toString(die.find(dwarf::DW_AT_name));
  if (!name)
    return false;

  if (disp == AvoidEmittedName)
    return false;

  // Here we try to mimix the gcc implementation, which seems to have
  // specific preferences about whether/where to use a previously
  // emitted name.
  if (disp != PreferEmittedName) {
    DWARFDie fwd(die);
    while (fwd.getTag() == dwarf::DW_TAG_typedef ||
           fwd.getTag() == dwarf::DW_TAG_const_type) {
      fwd = fwd.getAttributeValueAsReferencedDie(dwarf::DW_AT_type);
      assert(fwd.isValid());
    }
    if (fwd.getTag() != dwarf::DW_TAG_structure_type &&
        fwd.getTag() != dwarf::DW_TAG_union_type &&
        fwd.getTag() != dwarf::DW_TAG_array_type &&
        !isPtrToFunctionType(fwd))
      return false;
  }

  // Don't try to use the name stored within a base type
  // (among other things, they are allowed to have spaces)
  if (isBaseType(die))
    return false;

  return true;
}

bool GoDumpHelper::generateType(DWARFDie &die, EmitNameDisp disp)
{
  // Invalid?
  if (isInvalidType(die))
    return false;

  // Record size for posterity.
  auto byteSize = dwarf::toUnsigned(die.find(dwarf::DW_AT_byte_size));
  if (byteSize)
    setTypeSize(die, *byteSize);

  // Use a reference to a previously emitted type name if possible.
  if (useEmittedName(die, disp)) {
    auto name = dwarf::toString(die.find(dwarf::DW_AT_name));
    buf() << "_" << *name;
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
        rval = false;
      } else {
        if (toDie.getTag() != dwarf::DW_TAG_subroutine_type)
          buf() << "*";
        rval = generateType(toDie);
      }
      setTypeSize(die, ptrSize_);
      setTypeAlign(die, typeSize(die));
      break;
    }
    case dwarf::DW_TAG_typedef: {
      DWARFDie tgtDie = die.getAttributeValueAsReferencedDie(dwarf::DW_AT_type);
      assert(tgtDie.isValid());
      rval = generateType(tgtDie, AvoidEmittedName);
      setTypeAlign(die, typeAlign(tgtDie));
      setTypeSize(die, typeSize(tgtDie));
      break;
    }
    case dwarf::DW_TAG_structure_type: {
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
    case dwarf::DW_TAG_const_type: {
      // Throw away 'const' qualifiers
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

std::pair<bool, std::string> GoDumpHelper::generateTypeToString(DWARFDie &die)
{
  auto pauseState = pauseBuf();
  bool ok = generateType(die);
  std::string str(buf_->str());
  restoreBuf(pauseState.first, pauseState.second);
  return std::make_pair(ok, str);
}

void GoDumpHelper::visitVariable(DWARFDie &die)
{
  initBuf();

  DWARFDie typ = die.getAttributeValueAsReferencedDie(dwarf::DW_AT_type);
  assert(typ.isValid());
  bool ok = generateType(typ, PreferEmittedName);
  if (! ok)
    os_ << "// ";

  auto name = dwarf::toString(die.find(dwarf::DW_AT_name));
  assert(name);

  os_ << "var _" << *name << " " << buf_->str() << "\n";
}

void GoDumpHelper::emit()
{
  for (auto idx : queue_) {
    DWARFDie die = cu_->getDIEAtIndex(idx);
    if (isType(die.getTag()))
      visitType(die);
    else if (die.getTag() == dwarf::DW_TAG_variable)
      visitVariable(die);
  }
}

static void error(StringRef Prefix, std::error_code EC) {
  if (!EC)
    return;
  errs() << Prefix << ": " << EC.message() << "\n";
  exit(1);
}

static int visitMacrosFile(const std::string &infile, raw_ostream &os)
{
  return 0;
}

static int visitObjectFile(const std::string &infile, raw_ostream &os)
{
  ErrorOr<std::unique_ptr<MemoryBuffer>> buffOrErr =
      MemoryBuffer::getFile(infile);
  error(infile, buffOrErr.getError());
  std::unique_ptr<MemoryBuffer> buffer = std::move(buffOrErr.get());
  Expected<std::unique_ptr<Binary>> binOrErr = object::createBinary(*buffer);
  error(infile, errorToErrorCode(binOrErr.takeError()));
  // NB: no MachO support at the moment
  auto *obj = dyn_cast<ObjectFile>(binOrErr->get());
  if (obj == nullptr) {
    errs() << "error: problems opening object file " << infile << "\n";
    return 1;
  }
  std::unique_ptr<DWARFContext> dwctxt = DWARFContext::create(*obj);

  // Expect to see exactly one DWARF CU.
  if (dwctxt->getNumCompileUnits() < 1) {
    errs() << "error: no DWARF compilation units found in " << infile << "\n";
    return 1;
  } else if (dwctxt->getNumCompileUnits() > 1) {
    errs() << "error: unexpected multiple DWARF compilation units found in " << infile << "\n";
    return 1;
  }

  DWARFCompileUnit *cu = dwctxt->getCompileUnitAtIndex(0);
  GoDumpHelper state(os, cu);
  state.walkDIEChain();
  state.emit();

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

  if (InputObjectFile.empty() && InputMacrosFile.empty() ) {
    errs() << "error: supply input object file using -object option.\n";
    return 1;
  }

  std::unique_ptr<ToolOutputFile> OutputFile;
  if (!OutputFilename.empty()) {
    std::error_code EC;
    OutputFile = llvm::make_unique<ToolOutputFile>(OutputFilename, EC,
                                                     sys::fs::F_None);
    error("Unable to open output file" + OutputFilename, EC);
    // Don't remove output file if we exit with an error.
    OutputFile->keep();
  }

  raw_ostream &OS = OutputFile ? OutputFile->os() : outs();
  int rc = 0;
  if (! InputMacrosFile.empty()) {
    rc |= visitMacrosFile(InputMacrosFile, OS);
  }
  if (! InputObjectFile.empty()) {
    rc |= visitObjectFile(InputObjectFile, OS);
  }

  return rc;
}
