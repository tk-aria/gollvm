//===-- go-llvm-cabi-oracle.cpp - implementation of CABIOracle ------------===//
//
// Copyright 2018 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
//
//===----------------------------------------------------------------------===//
//
// Methods for CABIOracle class.
//
//===----------------------------------------------------------------------===//

#include "go-llvm-cabi-oracle.h"
#include "go-llvm-typemanager.h"

#include "llvm/IR/DataLayout.h"
#include "llvm/Support/raw_ostream.h"

//......................................................................

// Given an LLVM type, classify it according to whether it would
// need to be passed in an integer or SSE register (or if it is
// some combination of entirely empty structs/arrays).

enum TypDisp { FlavSSE, FlavSIMDFP, FlavInt, FlavEmpty };

// Arm64 ABI AAPCS64 defines HFA as follows:
// An Homogeneous Floating-point Aggregate (HFA) is an Homogeneous Aggregate with
// a Fundamental Data Type that is a Floating-Point type and at most four uniquely
// addressable members.
struct HFAInfo {
  unsigned number;
  llvm::Type *type;
};

// Here "meet" is in the dataflow anlysis sense (meet operator on lattice
// values).

static TypDisp dispMeet(TypDisp d1, TypDisp d2) {
  if (d1 == d2)
    return d1;
  if (d1 == FlavEmpty)
    return d2;
  if (d2 == FlavEmpty)
    return d1;
  if (d1 == FlavSSE && d2 == FlavSSE)
    return FlavSSE;
  return FlavInt;
}

static TypDisp getTypDisp(llvm::Type *typ) {
  if (typ->isFloatTy() || typ->isDoubleTy())
    return FlavSSE;
  return FlavInt;
}

//......................................................................

// Q: split off this class into a separate file, so as to unit test it?

// The AMD64 ABI classification scheme for aggregates (section 3.2.3
// third page) talks about dividing up the contents of an array or
// struct info 8-byte chunks or regions; this container class holds
// info on a region.  A given 8-byte region may contain something
// simple like a field of type "double", a pair of floats, or there
// may be a mix of floating point and integer fields within a struct.
// The "types" and "offsets" hold the LLVM types and offsets of
// elements relative to the start of the object; "abiDirectType" holds
// the type used to pass the elements if we're passing the entire
// object directly (in registers) as opposed in on the stack.
//
// Implementation of Arm64 ABI AAPCS64 reused this struct.

struct EightByteRegion {
  EightByteRegion() : abiDirectType(nullptr), attr(AttrNone) { }
  TypDisp getRegionTypDisp() const;

  std::vector<llvm::Type*> types;
  std::vector<uint64_t> offsets;
  llvm::Type *abiDirectType;
  CABIParamAttr attr;

  void dump();
  void osdump(llvm::raw_ostream &os);
};

TypDisp EightByteRegion::getRegionTypDisp() const {
  TypDisp disp = FlavEmpty;
  for (auto &t : types)
    disp = dispMeet(getTypDisp(t), disp);
  return disp;
}

void EightByteRegion::dump()
{
  std::string s;
  llvm::raw_string_ostream os(s);
  osdump(os);
  std::cerr << os.str();
}

void EightByteRegion::osdump(llvm::raw_ostream &os)
{
  os << "types:\n";
  for (auto &t : types) {
    os << "  ";
    t->print(os);
    os << "\n";
  }
  os << "offsets:\n";
  for (auto &o : offsets) {
    os << "  " << o << "\n";
  }
}

// This class is a container for zero or more EightByteRegion objects
// that correspond to the contents of an object of some type that
// we're going to be passing to or returning from a function.

class EightByteInfo {
 public:
  EightByteInfo(Btype *bt, TypeManager *tm);

  std::vector<EightByteRegion> &regions() { return ebrs_; }
  HFAInfo &getHFA() { return hfa_; }
  void getRegisterRequirements(unsigned *numInt, unsigned *numSSE);

 private:
  std::vector<EightByteRegion> ebrs_;
  HFAInfo hfa_;
  TypeManager *typeManager_;

  typedef std::pair<Btype *, unsigned> typAndOffset;
  void addLeafTypes(Btype *bt, unsigned off, std::vector<typAndOffset> *leaves);
  void explode(Btype *bt);
  void setHFA();
  void explodeStruct(Btype *bst);
  void explodeArray(BArrayType *bat);
  void incorporateScalar(Btype *bt);
  void determineABITypesForARM_AAPCS();
  void determineABITypesForX86_64_SysV();
  TypeManager *tm() const { return typeManager_; }
};

EightByteInfo::EightByteInfo(Btype *bt, TypeManager *tmgr)
    : typeManager_(tmgr)
{
  explode(bt);
  llvm::CallingConv::ID cconv = tmgr->callingConv();
  switch (cconv) {
  case llvm::CallingConv::X86_64_SysV:
    determineABITypesForX86_64_SysV();
    break;
  case llvm::CallingConv::ARM_AAPCS:
    setHFA();
    if (getHFA().number == 0 && tmgr->typeSize(bt) <= 16) {
      // For HFA and indirect cases, we don't need do this.
      determineABITypesForARM_AAPCS();
    }
    break;
  default:
    llvm::errs() << "unsupported llvm::CallingConv::ID " << cconv << "\n";
    break;
  }
}

// Perform a pre-order walk of a type, collecting the various leaf
// elements within the type. The general idea is to flatten out any
// nested structs/arrays and expose each of the underlying scalar
// elements of the type along with their offsets. Return value is a
// list of type/offset pairs, where the offset represents the location
// of the scalar type within the larger aggregate type. Example:
//
//      struct {
//        f1 int8
//        f2 [0]uint64
//        struct {
//          f1 int8
//        }
//      }
//
// The return for the struct above in "preserve" mode would be:
//
//     { int8, 0 }, { int8, 8 }
//
void EightByteInfo::addLeafTypes(Btype *bt,
                                 unsigned offset,
                                 std::vector<typAndOffset> *leaves)
{
  assert(bt && leaves);
  BStructType *bst = bt->castToBStructType();
  if (bst) {
    unsigned numFields = bst->fields().size();
    for (unsigned fidx = 0; fidx < numFields; ++fidx) {
      unsigned foff = tm()->typeFieldOffset(bst, fidx);
      addLeafTypes(bst->fieldType(fidx), offset + foff, leaves);
    }
    return;
  }
  BArrayType *bat = bt->castToBArrayType();
  if (bat) {
    Btype *et = bat->elemType();
    for (unsigned elidx = 0; elidx < bat->nelSize(); ++elidx) {
      unsigned eloff = elidx * tm()->typeSize(et);
      addLeafTypes(et, offset + eloff, leaves);
    }
    return;
  }
  BComplexType *bct = bt->castToBComplexType();
  if (bct) {
    unsigned bits = bct->bits() / 2;
    for (unsigned fidx = 0; fidx < 2; ++fidx) {
      Btype *leaf = tm()->floatType(bits);
      unsigned foff = tm()->typeFieldOffset(bct, fidx);
      leaves->push_back(std::make_pair(leaf, foff));
    }
    return;
  }

  assert(bt->flavor() != Btype::AuxT && bt->flavor() != Btype::FunctionT);
  leaves->push_back(std::make_pair(bt, offset));
}

// Given a struct type, explode it into zero, one or multiple EightByteRegion
// descriptors. Examples of the contents of EightByteInfo structs
// for various Go types follow. The first type (empty struct) results
// in a single EightByteRegion struct with empty vectors. The second
// type results in a single EightByteRegion, and the third yields two
// EightByteRegion.
//
//    Go struct type:              Computed EightByteRegions:
//                                 C types:           offsets:
//
//    type empty struct { }  [0]   <no types>         <no offsets>
//
//    type foo struct {
//      f1 uint8;            [0]   unsigned char      0
//      f2 uint16;                 unsigned short     16
//      f4 float32;                float              32
//    }
//
//    type bar struct {
//      f1 double;            [0]  double             0
//      f2 uint8;             [1]  unsigned char      64
//      f3 int16;                  short              80
//    }
//

void EightByteInfo::explodeStruct(Btype *bst)
{
  std::vector<typAndOffset> leafTypes;
  addLeafTypes(bst, 0, &leafTypes);

  // collect offsets and field types
  EightByteRegion *cur8 = nullptr;
  unsigned curOffset = 0;
  for (auto &pair : leafTypes) {
    Btype *lt = pair.first;
    unsigned offset = pair.second;
    if (cur8 == nullptr || (offset - curOffset >= 8)) {
      ebrs_.push_back(EightByteRegion());
      cur8 = &ebrs_.back();
      curOffset = offset;
    }
    cur8->types.push_back(lt->type());
    cur8->offsets.push_back(offset);
  }
}

// Given an array type, explode it into zero, one or multiple EightByteInfo
// descriptors. Examples appear below; the first array type results
// in a single EightByteInfo with empty type/offset vectors, then the second
// array type results in a single EightByteInfo, and the thrd array
// type results in two EightByteInfo structs:
//
//    Go type:                     Computed EightByteInfo:
//                                 C types:           offsets:
//
//    [0]float32              [0]  <no types>         <no offsets>
//
//    [3]uint8                [0]  unsigned char      0
//                                 unsigned char      8
//                                 unsigned char      16
//
//    [6]uint16               [0]  unsigned short     0
//                                 unsigned short     16
//                                 unsigned short     32
//                                 unsigned short     48
//                            [1]  unsigned short     64
//                                 unsigned short     80

void EightByteInfo::explodeArray(BArrayType *bat)
{
  EightByteRegion *cur8 = nullptr;
  unsigned curOffset = 0;
  unsigned elSize = tm()->typeSize(bat->elemType());
  for (unsigned elidx = 0; elidx < bat->nelSize(); ++elidx) {
    unsigned offset = elidx * elSize;
    if (cur8 == nullptr || (offset - curOffset >= 8)) {
      ebrs_.push_back(EightByteRegion());
      cur8 = &ebrs_.back();
      curOffset = offset;
    }
    // note that elem type here may be composite
    cur8->types.push_back(bat->elemType()->type());
    cur8->offsets.push_back(offset);
  }
}

void EightByteInfo::incorporateScalar(Btype *bt)
{
  assert(tm()->typeSize(bt) <= 8);
  EightByteRegion ebr;
  ebr.types.push_back(bt->type());
  ebr.offsets.push_back(0u);
  ebr.abiDirectType = bt->type();
  BIntegerType *bit = bt->castToBIntegerType();
  if (bit && tm()->typeSize(bit) < 4)
    ebr.attr = (bit->isUnsigned() ? AttrZext : AttrSext);
  ebrs_.push_back(ebr);
}

// Convert a Btype into EightByteRegions.
void EightByteInfo::explode(Btype *bt)
{
  BStructType *bst = bt->castToBStructType();
  BComplexType *bct = bt->castToBComplexType();
  BArrayType *bat = bt->castToBArrayType();
  if (bst || bct || bat) {
    // Flatten all composite types.
    explodeStruct(bt);
  } else {
    incorporateScalar(bt);
  }
}

void EightByteInfo::getRegisterRequirements(unsigned *numInt, unsigned *numSSE)
{
  *numInt = 0;
  *numSSE = 0;
  for (auto &ebr : ebrs_)
    if (ebr.getRegionTypDisp() == FlavSSE)
      *numSSE += 1;
    else
      *numInt += 1;
}

// Check if the parameter is an Homogeneous Floating-point Aggregates (HFA),
// and set hfa_ according to the result.
void EightByteInfo::setHFA() {
  if (ebrs_.empty()) {
    hfa_ = HFAInfo{0, nullptr};
    return;
  }
  unsigned num = 0;
  llvm::Type *typ = ebrs_[0].types[0];
  if (typ != tm()->llvmDoubleType() && typ != tm()->llvmFloatType()) {
    hfa_ = HFAInfo{0, nullptr};
    return;
  }
  for (auto &ebr : ebrs_) {
    for (auto &t : ebr.types) {
      if (t != typ || num > 3) {
        hfa_ = HFAInfo{0, nullptr};
        return;
      }
      ++num;
    }
  }
  hfa_ = HFAInfo{num, typ};
}

// Select the appropriate abi type for each eight-byte region within
// an EightByteInfo. HFA and arguments larger than 16 bytes have been
// processed, so the arguments processed here can only be integer types,
// pointer types or a mix of integer and non-integer, mapped it onto
// the pointer type or the appropriately sized integer type.
//
// Problems arise in the code below when dealing with structures with
// constructs that inject additional padding. For example, consider
// the following struct passed by value:
//
//      struct {
//        f1 int8
//        f2 [0]uint64
//        f3 int8
//      }
//
// Without taking into account the over-alignment of field f3, we would
// wind up with two regions, each with type int8. This in itself is not so
// bad, but creating a struct from these two types (via ::computeABIStructType)
// would give us { int8, int8 }, in which the second field doesn't have
// the correct alignment. Work around this by checking for such situations
// and promoting the type of the first EBR to 64 bits.
//
void EightByteInfo::determineABITypesForARM_AAPCS() {
  assert(ebrs_.size() <= 2);
  for (auto &ebr : ebrs_) {
    if (ebr.abiDirectType != nullptr)
      continue;
    // Preserve pointerness for the use of GC.
    // TODO: this assumes pointer is 8 byte, so we never pack pointer
    // and other stuff together.
    if (ebr.types[0]->isPointerTy()) {
      ebr.abiDirectType = tm()->llvmPtrType();
      continue;
    }
    unsigned nel = ebr.offsets.size();
    unsigned bytes = ebr.offsets[nel - 1] - ebr.offsets[0] +
                     tm()->llvmTypeSize(ebr.types[nel - 1]);
    assert(bytes && bytes <= 8);
    ebr.abiDirectType = tm()->llvmArbitraryIntegerType(bytes);
  }

  // See the example above for more on why this is needed.
  if (ebrs_.size() == 2 && ebrs_[0].abiDirectType->isIntegerTy()) {
    ebrs_[0].abiDirectType = tm()->llvmArbitraryIntegerType(8);
  }
}

// Select the appropriate abi type for each eight-byte region within
// an EightByteInfo. Pure floating point types are mapped onto float,
// double, or <2 x float> (a vector type), integer types (or something
// that is a mix of integer and non-integer) are mapped onto the
// appropriately sized integer type.
//
// Problems arise in the code below when dealing with structures with
// constructs that inject additional padding. For example, consider
// the following struct passed by value:
//
//      struct {
//        f1 int8
//        f2 [0]uint64
//        f3 int8
//      }
//
// Without taking into account the over-alignment of field f3, we would
// wind up with two regions, each with type int8. This in itself is not so
// bad, but creating a struct from these two types (via ::computeABIStructType)
// would give us { int8, int8 }, in which the second field doesn't have
// the correct alignment. Work around this by checking for such situations
// and promoting the type of the first EBR to 64 bits.
//
void EightByteInfo::determineABITypesForX86_64_SysV()
{
  // In the direct case, ebrs_.size() cannot be greater than 2 because parameters
  // larger than 16 bytes are passed indirectly.
  assert(ebrs_.size() <= 2);
  unsigned intRegions = 0;
  unsigned floatRegions = 0;
  for (auto &ebr : ebrs_) {
    if (ebr.abiDirectType != nullptr)
      continue;
    TypDisp regionDisp = ebr.getRegionTypDisp();
    if (regionDisp == FlavSSE) {
      // Case 1: two floats -> vector
      if (ebr.types.size() == 2)
        ebr.abiDirectType = tm()->llvmTwoFloatVecType();
      else if (ebr.types.size() == 1) {
        assert(ebr.types[0] == tm()->llvmDoubleType() ||
               ebr.types[0] == tm()->llvmFloatType());
        ebr.abiDirectType = ebr.types[0];
      } else {
        assert(false && "this should never happen");
      }
      floatRegions += 1;
    } else {
      unsigned nel = ebr.offsets.size();
      unsigned bytes = ebr.offsets[nel-1] - ebr.offsets[0] +
          tm()->llvmTypeSize(ebr.types[nel-1]);
      assert(bytes && bytes <= 8);
      // Preserve pointerness for the use of GC.
      // TODO: this assumes pointer is 8 byte, so we never pack pointer
      // and other stuff together.
      if (ebr.types[0]->isPointerTy())
        ebr.abiDirectType = tm()->llvmPtrType();
      else
        ebr.abiDirectType = tm()->llvmArbitraryIntegerType(bytes);
      intRegions += 1;
    }
  }

  // See the example above for more on why this is needed.
  if (intRegions == 2 &&
      ebrs_[0].abiDirectType->isIntegerTy())
    ebrs_[0].abiDirectType = tm()->llvmArbitraryIntegerType(8);
  else if (floatRegions == 2 &&
           ebrs_[0].abiDirectType == tm()->llvmFloatType())
    ebrs_[0].abiDirectType = tm()->llvmDoubleType();
}

//......................................................................

llvm::Type *CABIParamInfo::computeABIStructType(TypeManager *tm) const
{
  assert(tm);
  if (abiTypes_.size() == 1) {
    assert(abiTypes_[0]->isStructTy());
    return abiTypes_[0];
  }

  assert(abiTypes_.size() <= CABIParamInfo::ABI_TYPES_MAX_SIZE);
  llvm::Type *llst = tm->makeLLVMStructType(abiTypes_);
  return llst;
}

void CABIParamInfo::dump()
{
  std::string s;
  llvm::raw_string_ostream os(s);
  osdump(os);
  std::cerr << os.str();
}

void CABIParamInfo::osdump(llvm::raw_ostream &os)
{
  os << (disp() == ParmDirect ? "Direct" :
         (disp() == ParmIgnore ? "Ignore" :
          (disp() == ParmIndirect ? "Indirect" : "<unknown>")));
  if (attr() != AttrNone)
    os << (attr() == AttrStructReturn ? " AttrStructReturn" :
           (attr() == AttrByVal ? " AttrByVal" :
            (attr() == AttrNest ? " AttrNest" :
             (attr() == AttrZext ? " AttrZext" :
              (attr() == AttrSext ? " AttrSext" :
               (attr() == AttrDoCopy ? " AttrDoCopy" : " <unknown>"))))));
  os << " { ";
  unsigned idx = 0;
  for (auto &abit : abiTypes_) {
    os << (idx++ != 0 ? ", " : "");
    abit->print(os);
  }
  os << " }";
  os << " sigOffset: " << sigOffset() << "\n";
}

//......................................................................

// Helper struct to track state information during ABI param
// classification. Keeps track of arg count (args in final ABI-cooked
// signature) along with available int/sse regs.

class ABIState {
public:
  ABIState(TypeManager *typm) : argCount_(0) {
    assert(typm != nullptr);
    llvm::CallingConv::ID cconv = typm->callingConv();
    switch (cconv) {
    case llvm::CallingConv::X86_64_SysV:
      availIntRegs_ = 6;
      availSSERegs_ = 8;
      break;
    case llvm::CallingConv::ARM_AAPCS:
      availIntRegs_ = 8;
      availSIMDFPRegs_ = 8;
      break;
    default:
      llvm::errs() << "unsupported llvm::CallingConv::ID " << cconv << "\n";
      break;
    }
  }
  void addDirectIntArg() {
    if (availIntRegs_)
      availIntRegs_ -= 1;
    argCount_ += 1;
  }
  void addDirectSSEArg() {
    if (availSSERegs_)
      availSSERegs_ -= 1;
    argCount_ += 1;
  }
  // For ARM_AAPCS HFA, one argument may takes multiple registers.
  void addDirectSIMDFPArg(unsigned sr = 1) {
    unsigned t = availSIMDFPRegs_ - sr;
    if (availSIMDFPRegs_ > t)
      availSIMDFPRegs_ = t;
    argCount_ += 1;
  }
  void addIndirectArg() { argCount_ += 1; }
  void addIndirectReturn() {
    if (availIntRegs_)
      availIntRegs_ -= 1;
    argCount_ += 1;
  }
  // ARM_AAPCS uses separate x8 to store return address.
  void addIndirectReturnForARM_AAPCS() { argCount_ += 1; }
  void addChainArg() { argCount_ += 1; }
  unsigned argCount() const { return argCount_; }
  unsigned availIntRegs() const { return availIntRegs_; }
  unsigned availSSERegs() const { return availSSERegs_; }
  unsigned availSIMDFPRegs() const { return availSIMDFPRegs_; }
  void clearAvailIntRegs() { availIntRegs_ = 0; }
  void clearAvailSIMDFPRegs() { availSIMDFPRegs_ = 0; }

private:
  unsigned availIntRegs_;
  unsigned availSSERegs_;
  unsigned availSIMDFPRegs_;
  unsigned argCount_;
};

//......................................................................

CABIOracle::CABIOracle(const std::vector<Btype *> &fcnParamTypes,
                       Btype *fcnResultType,
                       bool followsCabi,
                       TypeManager *typeManager)
    : fcnParamTypes_(fcnParamTypes)
    , fcnResultType_(fcnResultType)
    , fcnTypeForABI_(nullptr)
    , typeManager_(typeManager)
    , followsCabi_(followsCabi)
    , ccID_(llvm::CallingConv::MaxID)
    , cc_(nullptr)
{
  setCC();
  analyze();
}

CABIOracle::CABIOracle(BFunctionType *ft,
                       TypeManager *typeManager)
    : fcnParamTypes_(ft->paramTypes())
    , fcnResultType_(ft->resultType())
    , fcnTypeForABI_(nullptr)
    , typeManager_(typeManager)
    , followsCabi_(ft->followsCabi())
    , ccID_(llvm::CallingConv::MaxID)
    , cc_(nullptr)
{
  setCC();
  analyze();
}

void CABIOracle::setCC()
{
  assert(typeManager_ != nullptr);
  ccID_ = typeManager_->callingConv();
  // Supported architectures at present.
  assert(ccID_ == llvm::CallingConv::X86_64_SysV ||
         ccID_ == llvm::CallingConv::ARM_AAPCS);

  if (cc_ != nullptr) {
    return;
  }
  switch (ccID_) {
  case llvm::CallingConv::X86_64_SysV:
    cc_ = std::unique_ptr<CABIOracleArgumentAnalyzer>(new CABIOracleX86_64_SysV(typeManager_));
    break;
  case llvm::CallingConv::ARM_AAPCS:
    cc_ = std::unique_ptr<CABIOracleArgumentAnalyzer>(new CABIOracleARM_AAPCS(typeManager_));
    break;
  default:
    llvm::errs() << "unsupported llvm::CallingConv::ID " << ccID_ << "\n";
    break;
  }
}

TypeManager *CABIOracle::tm() const
{
  return typeManager_;
}

llvm::FunctionType *CABIOracle::getFunctionTypeForABI()
{
  assert(cc_ != nullptr);
  return fcnTypeForABI_;
}

const CABIParamInfo &CABIOracle::paramInfo(unsigned idx)
{
  assert(cc_ != nullptr);
  // Slot 0: return info
  // Slot 1: static chain param
  // Slot 2: first argument / parameter
  unsigned pidx = idx + 2;
  assert(pidx < infov_.size());
  return infov_[pidx];
}

const CABIParamInfo &CABIOracle::returnInfo()
{
  assert(cc_ != nullptr);
  unsigned ridx = 0;
  assert(ridx < infov_.size());
  return infov_[ridx];
}

const CABIParamInfo &CABIOracle::chainInfo()
{
  assert(cc_ != nullptr);
  unsigned ridx = 1;
  assert(ridx < infov_.size());
  return infov_[ridx];
}

void CABIOracle::dump()
{
  std::cerr << toString();
}

std::string CABIOracle::toString()
{
  std::string s;
  llvm::raw_string_ostream os(s);
  osdump(os);
  return os.str();
}

void CABIOracle::osdump(llvm::raw_ostream &os)
{
  os << "Return: ";
  infov_[0].osdump(os);
  for (unsigned pidx = 1; pidx < infov_.size(); pidx++) {
    os << "Param " << pidx << ": ";
    infov_[pidx].osdump(os);
  }
}

// Fill in parameter / return / type information for a builtin function,
// e.g. all values passed + returned directly, no static chain param.

void CABIOracle::analyzeRaw()
{
  //if (fcnTypeForABI_)
  //return;

  // First slot in the info vector will be for the return.
  llvm::Type *rtyp = fcnResultType_->type();
  CABIParamInfo rinfo(rtyp, ParmDirect, AttrNone, -1);
  infov_.push_back(rinfo);

  // No static chain, but we'll create an entry for the chain marked
  // as ignored.
  CABIParamInfo cinfo(tm()->llvmPtrType(), ParmIgnore, AttrNest, -1);
  infov_.push_back(cinfo);

  // Now process the params.
  llvm::SmallVector<llvm::Type *, 8> elems(0);
  for (unsigned idx = 0; idx < fcnParamTypes_.size(); ++idx) {
    Btype *pType = fcnParamTypes_[idx];
    CABIParamInfo pinfo(pType->type(), ParmDirect, AttrNone, idx);
    infov_.push_back(pinfo);
    elems.push_back(pType->type());
  }

  // Build the proper LLVM function type
  const bool isVarargs = false;
  fcnTypeForABI_ = llvm::FunctionType::get(rtyp, elems, isVarargs);
}

void CABIOracle::analyze()
{
  if (fcnTypeForABI_)
    return;
  if (! followsCabi_) {
    analyzeRaw();
    return;
  }

  ABIState state(tm());
  assert(cc_ != nullptr);

  // First slot in the info vector will be for the return.
  infov_.push_back(cc_->analyzeABIReturn(fcnResultType_, state));

  // Static chain parameter
  int sigOff = state.argCount();
  state.addChainArg();
  CABIParamInfo cparm(tm()->llvmPtrType(), ParmDirect, AttrNest, sigOff);
  infov_.push_back(cparm);

  // Now process the params.
  for (unsigned idx = 0; idx < fcnParamTypes_.size(); ++idx) {
    Btype *pType = fcnParamTypes_[idx];
    auto d = cc_->analyzeABIParam(pType, state);
    infov_.push_back(d);
  }

  llvm::SmallVector<llvm::Type *, 8> elems(0);
  llvm::Type *rtyp = nullptr;
  if (infov_[0].disp() == ParmIndirect) {
    rtyp = tm()->llvmVoidType();
    elems.push_back(infov_[0].abiType());
  } else {
    rtyp = infov_[0].abiType();
  }
  for (unsigned pidx = 1; pidx < infov_.size(); pidx++) {
    if (infov_[pidx].disp() == ParmIgnore)
      continue;
    for (auto &abit : infov_[pidx].abiTypes())
      elems.push_back(abit);
  }
  const bool isVarargs = false;
  fcnTypeForABI_ = llvm::FunctionType::get(rtyp, elems, isVarargs);
}

//......................................................................

CABIOracleX86_64_SysV::CABIOracleX86_64_SysV(TypeManager *typeManager)
    : CABIOracleArgumentAnalyzer(typeManager) {}

// For full C++ (with long double, unions, vector types) the
// rules here are a good deal more complicated, but for Go
// it all boils down to the size of the type.

CABIParamDisp CABIOracleX86_64_SysV::classifyArgType(Btype *btype)
{
  int64_t sz = tm_->typeSize(btype);
  return (sz == 0 ? ParmIgnore : ((sz <= 16) ? ParmDirect : ParmIndirect));
}

// Given the number of registers that we think a param is going to consume, and
// a state object storing the registers used so far, canPassDirectly() makes a
// decision as to whether a given param can be passed directly in registers vs
// in memory.
//
// Note the first clause, "if (regsInt + regsSSE == 1) return true". This may
// seem counter-intuitive (why no check against the state object?), but this way
// of doing things is the convention used by other front ends (e.g. clang). What
// is happening here is that for larger aggregate/array params (things that
// don't fit into a single register), we'll make the pass-through-memory
// semantics explicit in the function signature and generate the explict code to
// copy things into memory. For params that do fit into a single register,
// however, we just leave them all as by-value parameters and then assume that
// the back end will do the right thing (e.g. pass the first few in registers
// and then the remaining ones in memory).
//
// Doing things this way has performance advantages in that the middle-end
// (all of the machine-independent LLVM optimization passes) won't have
// to deal with the additional chunks of stack memory and code to copy
// things onto and off of the stack (not to mention the aliasing concerns
// when a local variable's address is taken and then passed in a function
// call).

bool CABIOracleX86_64_SysV::canPassDirectly(unsigned regsInt,
                                 unsigned regsSSE,
                                 ABIState &state)
{
  if (regsInt + regsSSE == 1) // see comment above
    return true;
  if (regsInt <= state.availIntRegs() && regsSSE <= state.availSSERegs())
    return true;
  return false;
}

CABIParamInfo CABIOracleX86_64_SysV::analyzeABIParam(Btype *paramType,
                                                     ABIState &state)
{
  llvm::Type *ptyp = paramType->type();

  // The only situations in which we should be seeing AuxT types here is
  // in cases where we're analyzing the signatures of builtin functions,
  // meaning that there should be no structures or arrays.
  assert(paramType->flavor() != Btype::AuxT || ptyp->isVoidTy() ||
         !(ptyp->isStructTy() || ptyp->isArrayTy() || ptyp->isVectorTy() ||
           ptyp->isEmptyTy() || ptyp->isIntegerTy(8) || ptyp->isIntegerTy(16)));

  CABIParamDisp pdisp = classifyArgType(paramType);

  if (pdisp == ParmIgnore) {
    // Empty struct or array
    llvm::Type *voidType = tm_->llvmVoidType();
    return CABIParamInfo(voidType, ParmIgnore, AttrNone, -1);
  }

  int sigOff = state.argCount();

  if (pdisp == ParmIndirect) {
    // Value will be passed in memory on stack.
    // Stack is always in address space 0.
    llvm::Type *ptrTyp = llvm::PointerType::get(ptyp, 0);
    state.addIndirectArg();
    return CABIParamInfo(ptrTyp, ParmIndirect, AttrByVal, sigOff);
  }

  // Figure out what to do in the direct case
  assert(pdisp == ParmDirect);
  EightByteInfo ebi(paramType, tm_);

  // Figure out how many registers it would take to pass this parm directly
  unsigned regsInt = 0, regsSSE = 0;
  ebi.getRegisterRequirements(&regsInt, &regsSSE);

  // Make direct/indirect decision
  CABIParamAttr attr = AttrNone;
  if (canPassDirectly(regsInt, regsSSE, state)) {
    std::vector<llvm::Type *> abiTypes;
    for (auto &ebr : ebi.regions()) {
      abiTypes.push_back(ebr.abiDirectType);
      if (ebr.attr != AttrNone) {
        assert(attr == AttrNone || attr == ebr.attr);
        attr = ebr.attr;
      }
      if (ebr.getRegionTypDisp() == FlavSSE)
        state.addDirectSSEArg();
      else
        state.addDirectIntArg();
    }
    return CABIParamInfo(abiTypes, ParmDirect, attr, sigOff);
  } else {
    state.addIndirectArg();
    llvm::Type *ptrTyp = llvm::PointerType::get(ptyp, 0);
    return CABIParamInfo(ptrTyp, ParmIndirect, AttrByVal, sigOff);
  }
}

CABIParamInfo CABIOracleX86_64_SysV::analyzeABIReturn(Btype *resultType,
                                                      ABIState &state) {
  llvm::Type *rtyp = resultType->type();
  CABIParamDisp rdisp =
      (rtyp == tm_->llvmVoidType() ? ParmIgnore
                                    : classifyArgType(resultType));

  if (rdisp == ParmIgnore) {
    // This corresponds to a function with no returns or
    // returning an empty composite.
    llvm::Type *voidType = tm_->llvmVoidType();
    return CABIParamInfo(voidType, ParmIgnore, AttrNone, -1);
  }

  if (rdisp == ParmIndirect) {
    // Return value will be passed in memory, via a hidden
    // struct return param.
    // It is on stack, therefore address space 0.
    llvm::Type *ptrTyp = llvm::PointerType::get(rtyp, 0);
    state.addIndirectReturn();
    return CABIParamInfo(ptrTyp, ParmIndirect, AttrStructReturn, 0);
  }

  // Figure out what to do in the direct case
  assert(rdisp == ParmDirect);
  EightByteInfo ebi(resultType, tm_);
  auto &regions = ebi.regions();
  if (regions.size() == 1) {
    // Single value
    return CABIParamInfo(regions[0].abiDirectType,
                        ParmDirect, regions[0].attr, -1);
  }

  // Two-element struct
  assert(regions.size() == 2);
  llvm::Type *abiTyp =
      tm_->makeLLVMTwoElementStructType(regions[0].abiDirectType,
                                         regions[1].abiDirectType);
  return CABIParamInfo(abiTyp, ParmDirect, AttrNone, -1);
}

//......................................................................

CABIOracleARM_AAPCS::CABIOracleARM_AAPCS(TypeManager *typeManager)
    : CABIOracleArgumentAnalyzer(typeManager) {}

// Given the number of registers that we think a param is going to consume, and
// a state object storing the registers used so far, canPassDirectly() makes a
// decision as to whether a given param can be passed directly in registers vs
// in memory.
//
// Note the first clause, "if (regsInt + regsSSE == 1) return true". This may
// seem counter-intuitive (why no check against the state object?), but this way
// of doing things is the convention used by other front ends (e.g. clang). What
// is happening here is that for larger aggregate/array params (things that
// don't fit into a single register), we'll make the pass-through-memory
// semantics explicit in the function signature and generate the explict code to
// copy things into memory. For params that do fit into a single register,
// however, we just leave them all as by-value parameters and then assume that
// the back end will do the right thing (e.g. pass the first few in registers
// and then the remaining ones in memory).
//
// Doing things this way has performance advantages in that the middle-end
// (all of the machine-independent LLVM optimization passes) won't have
// to deal with the additional chunks of stack memory and code to copy
// things onto and off of the stack (not to mention the aliasing concerns
// when a local variable's address is taken and then passed in a function
// call).

bool CABIOracleARM_AAPCS::canPassDirectly(unsigned regsInt,
                                          unsigned regsSIMDFP,
                                          ABIState &state)
{
  if (regsInt + regsSIMDFP == 1) // see comment above
    return true;
  if (regsInt <= state.availIntRegs() && regsSIMDFP <= state.availSIMDFPRegs())
    return true;
  return false;
}

CABIParamInfo CABIOracleARM_AAPCS::analyzeABIParam(Btype *paramType, ABIState &state)
{
  llvm::Type *ptyp = paramType->type();

  // The only situations in which we should be seeing AuxT types here is
  // in cases where we're analyzing the signatures of builtin functions,
  // meaning that there should be no structures or arrays.
  assert(paramType->flavor() != Btype::AuxT || ptyp->isVoidTy() ||
         !(ptyp->isStructTy() || ptyp->isArrayTy() || ptyp->isVectorTy() ||
           ptyp->isEmptyTy() || ptyp->isIntegerTy(8) || ptyp->isIntegerTy(16)));

  if (ptyp == tm_->llvmVoidType()) {
    // Empty struct or array
    llvm::Type *voidType = tm_->llvmVoidType();
    return CABIParamInfo(voidType, ParmIgnore, AttrNone, -1);
  }

  // If ptyp is llvmVoidType, we may not able to get the size of it,
  // so we can't combine the following if statement with the above one.
  int64_t sz = tm_->typeSize(paramType);
  if (sz == 0) {
    // Empty struct or array
    llvm::Type *voidType = tm_->llvmVoidType();
    return CABIParamInfo(voidType, ParmIgnore, AttrNone, -1);
  }

  int sigOff = state.argCount();

  //  Go has only two floating point types: float32 and float64, so the size of
  //  an HFA does not exceed 32 bytes.
  if (sz > 32) {
    // Value will be passed in memory on stack.
    // Stack is always in address space 0.
    llvm::Type *ptrTyp = llvm::PointerType::get(ptyp, 0);
    state.addIndirectArg();
    return CABIParamInfo(ptrTyp, ParmIndirect, AttrDoCopy, sigOff);
  }

  EightByteInfo ebi(paramType, tm_);
  auto &hfa = ebi.getHFA();
  if (hfa.number != 0) {
    // Is HFA.
    llvm::Type * abiType = hfa.type;
    if (hfa.number > 1) {
      // If it contains multiple elements, make the param as an Array
      // type. This ensures that an HFA is passed as a whole.
      abiType = llvm::ArrayType::get(hfa.type, hfa.number);
    }
    if (canPassDirectly(0, hfa.number, state)) {
      state.addDirectSIMDFPArg(hfa.number);
    } else {
      state.clearAvailSIMDFPRegs();
      state.addIndirectArg();
    }
    // Whether or not an HFA can be passed in registers, we use
    // ParmDirect. This is because HFA is passed by value on stack
    // in indirect cases, and we happen to be able to reuse the
    // processing logic of the direct cases.
    return CABIParamInfo(abiType, ParmDirect, AttrNone, sigOff);
  }
  if (sz > 16) {
    // Not an HFA,value will be passed in memory on stack.
    // Stack is always in address space 0.
    llvm::Type *ptrTyp = llvm::PointerType::get(ptyp, 0);
    state.addIndirectArg();
    return CABIParamInfo(ptrTyp, ParmIndirect, AttrDoCopy, sigOff);
  }

  // Direct case.
  auto &regions = ebi.regions();
  // Make direct/indirect decision
  CABIParamAttr attr = AttrNone;
  if (canPassDirectly(regions.size(), 0, state)) {
    std::vector<llvm::Type *> abiTypes;
    for (auto &ebr : regions) {
      abiTypes.push_back(ebr.abiDirectType);
      if (ebr.attr != AttrNone) {
        assert(attr == AttrNone || attr == ebr.attr);
        attr = ebr.attr;
      }
      state.addDirectIntArg();
    }
    return CABIParamInfo(abiTypes, ParmDirect, attr, sigOff);
  } else {
    state.clearAvailIntRegs();
    state.addIndirectArg();
    llvm::Type *abiType = regions[0].abiDirectType;
    if (regions.size() > 1) {
      // Convert the argument to an array type so that the backend considers it as a
      // whole whether it can be passed through registers.
      abiType = llvm::ArrayType::get(tm_->llvmArbitraryIntegerType(8), regions.size());
    }
    // Pass by value on stack, so use ParmDirect.
    return CABIParamInfo(abiType, ParmDirect, AttrNone, sigOff);
  }
}

CABIParamInfo CABIOracleARM_AAPCS::analyzeABIReturn(Btype *resultType,
                                                    ABIState &state) {
  llvm::Type *rtyp = resultType->type();

  if (rtyp == tm_->llvmVoidType()) {
    // This corresponds to a function with no returns or
    // returning an empty composite.
    llvm::Type *voidType = tm_->llvmVoidType();
    return CABIParamInfo(voidType, ParmIgnore, AttrNone, -1);
  }

  // If rtyp is llvmVoidType, we may not able to get the size of it,
  // so we can't combine the following if statement with the above one.
  int64_t sz = tm_->typeSize(resultType);
  if (sz == 0) {
    // This corresponds to a function with no returns or
    // returning an empty composite.
    llvm::Type *voidType = tm_->llvmVoidType();
    return CABIParamInfo(voidType, ParmIgnore, AttrNone, -1);
  }

  //  Go has only two floating point types: float32 and float64, so the size of
  //  an HFA does not exceed 32 bytes.
  if (sz > 32) {
    // Return value will be passed in memory, via a hidden
    // struct return param.
    // It is on stack, therefore address space 0.
    llvm::Type *ptrTyp = llvm::PointerType::get(rtyp, 0);
    // Indirect return value is passed by register R8, so doesn't occupy any int
    // register.
    state.addIndirectReturnForARM_AAPCS();
    return CABIParamInfo(ptrTyp, ParmIndirect, AttrStructReturn, 0);
  }

  EightByteInfo ebi(resultType, tm_);
  auto &hfa = ebi.getHFA();
  if (hfa.number != 0) {
    // Is HFA.
    // If only one element, don't bother to make a llvm struct type.
    if (hfa.number == 1) {
      return CABIParamInfo(hfa.type, ParmDirect, AttrNone, -1);
    }
    std::vector<llvm::Type *> fields;
    for (unsigned i = 0; i < hfa.number; ++i) {
      fields.push_back(hfa.type);
    }
    llvm::Type *abiTyp = tm_->makeLLVMStructType(fields);
    return CABIParamInfo(abiTyp, ParmDirect, AttrNone, -1);
  }

  // The return value is not an HFA and its size exceeds 16 bytes,
  // be passed in memory, via a hidden struct return param.
  if (sz > 16) {
    llvm::Type *ptrTyp = llvm::PointerType::get(rtyp, 0);
    state.addIndirectReturnForARM_AAPCS();
    return CABIParamInfo(ptrTyp, ParmIndirect, AttrStructReturn, 0);
  }

  // Direct case
  auto &regions = ebi.regions();
  if (regions.size() == 1) {
    // Single value
    return CABIParamInfo(regions[0].abiDirectType, ParmDirect, regions[0].attr,
                         -1);
  }

  // Two-element struct
  assert(regions.size() == 2);
  llvm::Type *abiTyp = tm_->makeLLVMTwoElementStructType(
      regions[0].abiDirectType, regions[1].abiDirectType);
  return CABIParamInfo(abiTyp, ParmDirect, AttrNone, -1);
}

//......................................................................
