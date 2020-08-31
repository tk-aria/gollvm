//===-- typemanger.cpp - implementation of TypeManager class --------------===//
//
// Copyright 2018 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
//
//===----------------------------------------------------------------------===//
//
// Methods for TypeManager class.
//
//===----------------------------------------------------------------------===//

#include "go-llvm-typemanager.h"
#include "go-llvm-dibuildhelper.h"
#include "go-llvm-bexpression.h"
#include "go-llvm-cabi-oracle.h"

#include "llvm/Support/raw_ostream.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/DIBuilder.h"
#include "llvm/IR/Type.h"

TypeManager::TypeManager(llvm::LLVMContext &context,
                         llvm::CallingConv::ID conv,
                         unsigned addrspace)
    : context_(context)
    , datalayout_(nullptr)
    , cconv_(conv)
    , addressSpace_(addrspace)
    , traceLevel_(0)
    , nametags_(nullptr)
    , errorExpression_(nullptr)
    , errorType_(nullptr)
    , stringType_(nullptr)
    , uintPtrType_(nullptr)
    , llvmVoidType_(nullptr)
    , llvmBoolType_(nullptr)
    , llvmPtrType_(nullptr)
    , llvmPtr0Type_(nullptr)
    , llvmSizeType_(nullptr)
    , llvmIntegerType_(nullptr)
    , llvmInt8Type_(nullptr)
    , llvmInt32Type_(nullptr)
    , llvmInt64Type_(nullptr)
    , llvmFloatType_(nullptr)
    , llvmDoubleType_(nullptr)
    , llvmLongDoubleType_(nullptr)
    , llvmTwoFloatVecType_(nullptr)
{
  // LLVM doesn't have anything that corresponds directly to the
  // gofrontend notion of an error type. For now we create a so-called
  // 'identified' anonymous struct type and have that act as a
  // stand-in. See http://llvm.org/docs/LangRef.html#structure-type
  errorType_ = makeAuxType(llvm::StructType::create(context_, "$Error$"));

  // For builtin creation
  llvmPtrType_ =
      llvm::PointerType::get(llvm::IntegerType::get(context_, 8),
                             addressSpace_);
  llvmPtr0Type_ =
      llvm::PointerType::get(llvm::IntegerType::get(context_, 8), 0);

  // Assorted pre-computer types for use in builtin function creation
  llvmVoidType_ = llvm::Type::getVoidTy(context_);
  llvmBoolType_ = llvm::IntegerType::get(context_, 1);
  llvmInt8Type_ = llvm::IntegerType::get(context_, 8);
  llvmInt32Type_ = llvm::IntegerType::get(context_, 32);
  llvmInt64Type_ = llvm::IntegerType::get(context_, 64);
  llvmFloatType_ = llvm::Type::getFloatTy(context_);
  llvmDoubleType_ = llvm::Type::getDoubleTy(context_);
  llvmLongDoubleType_ = llvm::Type::getFP128Ty(context_);
  llvmTwoFloatVecType_ = llvm::FixedVectorType::get(llvmFloatType_, 2);

  // Predefined C string type
  stringType_ = pointerType(integerType(true, 8));
}

TypeManager::~TypeManager()
{
  for (auto &t : anonTypes_)
    delete t;
  for (auto &t : placeholders_)
    delete t;
  for (auto &t : duplicates_)
    delete t;
  for (auto &kv : auxTypeMap_)
    delete kv.second;
  for (auto &t : namedTypes_)
    delete t;
}

void TypeManager::initializeTypeManager(Bexpression *errorExpression,
                                        const llvm::DataLayout *datalayout,
                                        NameGen *nt)
{
  assert(errorExpression);
  assert(nt);
  assert(datalayout);
  datalayout_ = datalayout;
  errorExpression_ = errorExpression;
  nametags_ = nt;

  llvmIntegerType_ =
      llvm::IntegerType::get(context_, datalayout_->getPointerSizeInBits());
  llvmSizeType_ = llvmIntegerType_;
  uintPtrType_ = makeAuxType(llvmIntegerType_);
}

// Used for creation of intermediate ABI types, not used by FE

llvm::Type *TypeManager::llvmArbitraryIntegerType(unsigned bytes)
{
  assert(bytes);
  assert(bytes <= 8);
  return llvm::IntegerType::get(context_, bytes*8);
}

bool TypeManager::removeAnonType(Btype *typ)
{
  auto it = anonTypes_.find(typ);
  if (it != anonTypes_.end()) {
    anonTypes_.erase(it);
    return true;
  }
  return false;
}

void TypeManager::reinstallAnonType(Btype *typ)
{
  auto it = anonTypes_.find(typ);
  if (it != anonTypes_.end()) {
    // type already exists -- take the type we were intending to
    // install in anonTypes_ and record as duplicate.
    duplicates_.insert(typ);
    return;
  }
  anonTypes_.insert(typ);
}

// We have two flavors of placeholders: those created using the
// ::placeholder_*_type methods, and then concrete types that have
// placeholder children (for example when ::pointer_type is invoked
// on a placeholder type). This routine deals only with the first
// category.

void TypeManager::updatePlaceholderUnderlyingType(Btype *pht,
                                                  Btype *newtyp)
{
  assert(placeholders_.find(pht) != placeholders_.end());
  assert(anonTypes_.find(pht) == anonTypes_.end());
  assert(pht->isPlaceholder());

  // make changes
  pht->setType(newtyp->type());
  if (! newtyp->isPlaceholder())
    pht->setPlaceholder(false);

  // Now that this is fully concrete, this may allow us to make
  // other types concrete if they referred to this one.
  if (!pht->isPlaceholder())
    postProcessResolvedPlaceholder(pht);
}

BFunctionType *TypeManager::makeAuxFcnType(llvm::FunctionType *ft)
{
  assert(ft);

  auto it = auxTypeMap_.find(ft);
  if (it != auxTypeMap_.end())
    return it->second->castToBFunctionType();

  Btype *recvTyp = nullptr;
  std::vector<Btype *> params;
  std::vector<Btype *> results;
  Btype *rbtype = nullptr;
  if (ft->getReturnType() != llvmVoidType_) {
    rbtype = makeAuxType(ft->getReturnType());
    results.push_back(rbtype);
  } else {
    rbtype = makeAuxType(llvm::Type::getVoidTy(context_));
  }
  for (unsigned ii = 0; ii < ft->getNumParams(); ++ii)
    params.push_back(makeAuxType(ft->getParamType(ii)));
  Location loc;
  bool followsCabi = false;
  BFunctionType *rval =
      new BFunctionType(recvTyp, params, results, rbtype, ft,
                        followsCabi, loc);
  auxTypeMap_[ft] = rval;
  return rval;
}

Btype *TypeManager::makeAuxType(llvm::Type *lt) {
  assert(lt);

  auto it = auxTypeMap_.find(lt);
  if (it != auxTypeMap_.end())
    return it->second;
  Location loc;
  Btype *rval = new Btype(Btype::AuxT, lt, loc);
  auxTypeMap_[lt] = rval;
  return rval;
}

Btype *TypeManager::errorType() { return errorType_; }

Btype *TypeManager::voidType() { return makeAuxType(llvmVoidType_); }

bool TypeManager::isBooleanType(Btype *bt) {
  BIntegerType *bit = bt->castToBIntegerType();
  return (bit && bit->isUnsigned() &&
          bit->type() == llvm::IntegerType::get(context_, 8));
}

Btype *TypeManager::boolType() {
  // LLVM has no predefined boolean type. Use int8 for this purpose.
  return integerType(true, 8);
}

llvm::Type *TypeManager::makeLLVMFloatType(int bits)
{
  llvm::Type *llft = nullptr;
  if (bits == 32)
    llft = llvmFloatType_;
  else if (bits == 64)
    llft = llvmDoubleType_;
  else if (bits == 128)
    llft = llvmLongDoubleType_;
  else
    assert(false && "unsupported float width");
  return llft;
}

// Get an unnamed float type.

Btype *TypeManager::floatType(int bits)
{
  llvm::Type *llft = makeLLVMFloatType(bits);

  // Consult cache
  Location loc;
  BFloatType cand(bits, llft, loc);
  auto it = anonTypes_.find(&cand);
  if (it != anonTypes_.end()) {
    Btype *existing = *it;
    BFloatType *bft = existing->castToBFloatType();
    assert(bft);
    return bft;
  }

  // Install in cache
  BFloatType *rval = new BFloatType(bits, llft, loc);
  anonTypes_.insert(rval);
  return rval;
}

// Get an unnamed integer type.
//
// Note that in the LLVM world, we don't have signed/unsigned types,
// we only have signed/unsigned operations (e.g. signed addition of
// two integers).
//
// Many frontends for C-like languages have squishyness when it comes
// to signed/unsigned arithmetic. Example: for the C code
//
//       double abc(unsigned x, int y) { return (double) x + y; }
//
// What typically happens under the hood is that a C compiler constructs
// a parse tree that looks like
//
//                  op: ADDITION
//                 /          \.
//                /            \.
//            var_ref(x)      var_ref(y)
//            typ: unsigned   type: signed
//
// where the ADD op is generic/polymorphic, and the real nature of the
// add (signed/unsigned) only becomes apparent during lowering, when
// the C rules about type conversions are enforced.
//
// To account for any potential hazards here, we record whether the
// frontend has announced that a specific type is unsigned in a side
// table.  We can then use that table later on to enforce the rules
// (for example, to insure that we didn't forget to insert a type
// conversion, or to derive the correct flavor of an integer ADD based
// on its arguments).

Btype *TypeManager::integerType(bool is_unsigned, int bits)
{
  llvm::Type *llit = llvm::IntegerType::get(context_, bits);

  // Check for and return existing anon type if we have one already
  Location loc;
  BIntegerType cand(is_unsigned, bits, llit, loc);
  auto it = anonTypes_.find(&cand);
  if (it != anonTypes_.end()) {
    Btype *existing = *it;
    BIntegerType *bit = existing->castToBIntegerType();
    assert(bit);
    return bit;
  }

  // Install in cache
  BIntegerType *rval = new BIntegerType(is_unsigned, bits, llit, loc);
  anonTypes_.insert(rval);
  return rval;
}

llvm::Type *TypeManager::makeLLVMPointerType(llvm::Type *toTy)
{
  return llvm::PointerType::get(toTy, addressSpace_);
}

llvm::Type *
TypeManager::makeLLVMTwoElementStructType(llvm::Type *f1, llvm::Type *f2)
{
  llvm::SmallVector<llvm::Type *, 2> elems(2);
  elems[0] = f1;
  elems[1] = f2;
  return llvm::StructType::get(context_, elems);
}

llvm::Type *
TypeManager::makeLLVMStructType(const std::vector<Btyped_identifier> &fields) {
  llvm::SmallVector<llvm::Type *, 64> elems(fields.size());
  for (unsigned i = 0; i < fields.size(); ++i)
    elems[i] = fields[i].btype->type();
  llvm::Type *lst = llvm::StructType::get(context_, elems);
  return lst;
}

llvm::Type *
TypeManager::makeLLVMStructType(const std::vector<llvm::Type *> &fields) {
  llvm::Type *lst = llvm::StructType::get(context_, fields);
  return lst;
}

bool TypeManager::addPlaceholderRefs(Btype *btype)
{
  bool rval = false;
  switch(btype->flavor()) {
    case Btype::ArrayT: {
      BArrayType *bat = btype->castToBArrayType();
      if (bat->elemType()->isUnresolvedPlaceholder()) {
        placeholderRefs_[bat->elemType()].insert(btype);
        rval = true;
      }
      break;
    }
    case Btype::PointerT: {
      BPointerType *bpt = btype->castToBPointerType();
      if (bpt->toType()->isUnresolvedPlaceholder()) {
        placeholderRefs_[bpt->toType()].insert(btype);
        rval = true;
      }
      break;
    }
    case Btype::StructT: {
      BStructType *bst = btype->castToBStructType();
      const std::vector<Backend::Btyped_identifier> &fields = bst->fields();
      for (unsigned i = 0; i < fields.size(); ++i) {
        if (fields[i].btype->isUnresolvedPlaceholder()) {
          placeholderRefs_[fields[i].btype].insert(bst);
          rval = true;
        }
      }
      break;
    }
    default: {
      // nothing to do here
    }
  }
  return rval;
}

// The front end creates structures with fields that are
// function-type as opposed to pointer-to-function type -- this
// doesn't play well with LLVM, so rewrite the field types
// in this instance.

std::vector<Btyped_identifier>
TypeManager::sanitizeFields(const std::vector<Btyped_identifier> &fields)
{
  std::vector<Btyped_identifier> pfields(fields);
  for (unsigned i = 0; i < fields.size(); ++i) {
    if (fields[i].btype->type()->isFunctionTy())
      pfields[i].btype = pointerType(fields[i].btype);
  }
  return pfields;
}

// Make a struct type.

Btype *TypeManager::structType(const std::vector<Btyped_identifier> &rawfields)
{
  // Return error type if any field has error type; look for placeholders
  bool hasPlaceField = false;
  for (unsigned i = 0; i < rawfields.size(); ++i) {
    if (rawfields[i].btype == errorType_)
      return errorType_;
    if (rawfields[i].btype->isUnresolvedPlaceholder())
      hasPlaceField = true;
  }

  Location loc;
  BStructType *rval = nullptr;
  llvm::Type *llst = nullptr;
  std::vector<Btyped_identifier> fields = sanitizeFields(rawfields);
  if (hasPlaceField) {
    // If any of the fields have placeholder type, then we can't
    // create a concrete LLVM type, so manufacture a placeholder
    // instead.
    llst = makeOpaqueLlvmType("IPST");
    rval = new BStructType(fields, llst, loc);
    rval->setPlaceholder(addPlaceholderRefs(rval));
  } else {
    // No placeholder fields -- manufacture the concrete LLVM, then
    // check for and return existing anon type if we have one already.
    llst = makeLLVMStructType(fields);
    BStructType cand(fields, llst, loc);
    auto it = anonTypes_.find(&cand);
    if (it != anonTypes_.end()) {
      Btype *existing = *it;
      assert(existing->castToBStructType());
      return existing;
    }
    rval = new BStructType(fields, llst, loc);
  }

  if (traceLevel() > 1) {
    std::cerr << "\n^ struct type "
              << ((void*)rval) << " [llvm type "
              << ((void*)llst) << "] fields:\n";
    for (unsigned i = 0; i < fields.size(); ++i)
      std::cerr << i << ": " << ((void*)fields[i].btype) << "\n";
    rval->dump();
  }

  // Done
  anonTypes_.insert(rval);
  return rval;
}

llvm::Type *TypeManager::makeOpaqueLlvmType(const char *tag) {
  std::string tname(tnamegen(tag));
  return llvm::StructType::create(context_, tname);
}

// Create a placeholder for a struct type.
Btype *TypeManager::placeholderStructType(const std::string &name,
                                          Location location)
{
  BStructType *pst = new BStructType(name, location);
  llvm::Type *opaque = makeOpaqueLlvmType(name.c_str());
  pst->setType(opaque);
  placeholders_.insert(pst);
  return pst;
}

// LLVM has no such thing as a complex type -- it expects the front
// end to lower all complex operations from the get-go, meaning that
// the back end will see only two-element structs. In order to create
// the proper debug information, however, we have to keep track of
// which two-float / two-double structs are complex objects and which
// are simply regular structs.

Btype *TypeManager::complexType(int bits)
{
  assert(bits == 64 || bits == 128);
  llvm::Type *elemTy = (bits == 64 ? llvm::Type::getFloatTy(context_)
                                   : llvm::Type::getDoubleTy(context_));
  llvm::Type *llct = makeLLVMTwoElementStructType(elemTy, elemTy);

  // Check for and return existing anon type if we have one already
  Location loc;
  BComplexType cand(bits, llct, loc);
  auto it = anonTypes_.find(&cand);
  if (it != anonTypes_.end()) {
    Btype *existing = *it;
    BComplexType *bct = existing->castToBComplexType();
    assert(bct);
    return bct;
  }

  // Install in cache
  BComplexType *rval = new BComplexType(bits, llct, loc);
  anonTypes_.insert(rval);
  return rval;
}

// Get a pointer type.

Btype *TypeManager::pointerType(Btype *toType)
{
  return addrSpacePointerType(toType, addressSpace_);
}

Btype *TypeManager::addrSpacePointerType(Btype *toType, unsigned addressSpace)
{
  if (toType == errorType_)
    return errorType_;

  // Manufacture corresponding LLVM type. Note that LLVM does not
  // allow creation of a "pointer to void" type -- model this instead
  // as pointer to char.
  llvm::Type *lltot =
      (toType->type() == llvmVoidType_ ? llvmInt8Type_ : toType->type());
  llvm::Type *llpt = llvm::PointerType::get(lltot, addressSpace);

  // Check for and return existing anon type if we have one already
  Location loc;
  BPointerType cand(toType, llpt, loc);
  auto it = anonTypes_.find(&cand);
  if (it != anonTypes_.end()) {
    Btype *existing = *it;
    BPointerType *bpt = existing->castToBPointerType();
    assert(bpt);
    return bpt;
  }

  // Create new type
  BPointerType *rval = new BPointerType(toType, llpt, loc);
  rval->setPlaceholder(addPlaceholderRefs(rval));
  if (traceLevel() > 1) {
    std::cerr << "\n^ pointer type "
              << ((void*)rval) << " [llvm type "
              << ((void*) llpt) << "]\n";
    rval->dump();
  }

  // Install in cache
  anonTypes_.insert(rval);
  return rval;
}

// LLVM doesn't directly support placeholder types other than opaque
// structs, so the general strategy for placeholders is to create an
// opaque struct (corresponding to the thing being pointed to) and
// then make a pointer to it. Since LLVM allows only a single opaque
// struct type with a given name within a given context, we capture
// the provided name but we don't try to hand that specific name to
// LLVM to use for the identified struct (so as to avoid collisions).

// Create a placeholder for a pointer type.

Btype *TypeManager::placeholderPointerType(const std::string &name,
                                           Location location,
                                           bool forfunc)
{
  Btype *ppt = new BPointerType(name, location);
  llvm::Type *opaque = makeOpaqueLlvmType("PPT");
  llvm::PointerType *pto = llvm::PointerType::get(opaque, addressSpace_);
  ppt->setType(pto);
  placeholders_.insert(ppt);

  if (traceLevel() > 1) {
    std::cerr << "\n^ placeholder pointer type "
              << ((void*)ppt) << " [llvm type "
              << ((void*) pto) << "]\n";
    ppt->dump();
  }

  return ppt;
}

llvm::Type *
TypeManager::makeLLVMFunctionType(const std::vector<Btype *> &paramTypes,
                                  Btype *rbtype,
                                  bool followsCabi)
{
  // Construct an ABI oracle helper and ask the oracle for the
  // correct ABI-adjusted type.
  CABIOracle abiOracle(paramTypes, rbtype, followsCabi, this);
  llvm::FunctionType *llvmft = abiOracle.getFunctionTypeForABI();

  // https://gcc.gnu.org/PR72814 handling. From the go-gcc.cc
  // equivalent, here is an explanatory comment:
  //
  // The libffi library can not represent a zero-sized object.  To
  // avoid causing confusion on 32-bit SPARC, we treat a function that
  // returns a zero-sized value as returning void.  That should do no
  // harm since there is no actual value to be returned.
  llvm::Type *rtyp = rbtype->type();
  assert(!rtyp->isSized() || datalayout_->getTypeSizeInBits(rtyp) != 0 ||
         llvmft->getReturnType() == llvm::Type::getVoidTy(context_));

  return llvmft;
}

// Make a function type.

Btype *
TypeManager::functionType(const Btyped_identifier &receiver,
                          const std::vector<Btyped_identifier> &parameters,
                          const std::vector<Btyped_identifier> &results,
                          Btype *result_struct,
                          bool followsCabi,
                          Location location)
{
  std::vector<Btype *> paramTypes;
  if (receiver.btype) {
    if (receiver.btype == errorType_)
      return errorType_;
    paramTypes.push_back(receiver.btype);
  }

  // Vett the parameters and results. As with structure fields, convert
  // parameters of raw function type to pointer-to-function type.
  for (auto p : parameters) {
    if (p.btype == errorType_)
      return errorType_;
    Btype *ptype = p.btype;
    if (ptype->type()->isFunctionTy())
      ptype = pointerType(ptype);
    paramTypes.push_back(ptype);
  }
  std::vector<Btype *> resultTypes;
  for (auto r : results) {
    if (r.btype == errorType_)
      return errorType_;
    resultTypes.push_back(r.btype);
  }
  if (result_struct && result_struct == errorType_)
    return errorType_;

  // Determine result Btype
  Btype *rbtype = nullptr;
  if (results.empty())
    rbtype = makeAuxType(llvm::Type::getVoidTy(context_));
  else if (results.size() == 1) {
    rbtype = results.front().btype;
  } else {
    assert(result_struct != nullptr);
    rbtype = result_struct;
  }
  assert(rbtype != nullptr);

  llvm::Type *llft = makeLLVMFunctionType(paramTypes, rbtype, followsCabi);

  // Consult cache
  BFunctionType cand(receiver.btype, paramTypes, resultTypes, rbtype,
                     llft, followsCabi, location);
  auto it = anonTypes_.find(&cand);
  if (it != anonTypes_.end()) {
    Btype *existing = *it;
    BFunctionType *bft = existing->castToBFunctionType();
    assert(bft);
    return bft;
  }

  // Manufacture new BFunctionType to return
  BFunctionType *rval =
      new BFunctionType(receiver.btype, paramTypes, resultTypes,
                        rbtype, llft, followsCabi, location);

  // Do some book-keeping
  bool isPlace = false;
  if (result_struct && result_struct->isUnresolvedPlaceholder()) {
    placeholderRefs_[result_struct].insert(rval);
    isPlace = true;
  }
  if (receiver.btype && receiver.btype->isUnresolvedPlaceholder()) {
    placeholderRefs_[receiver.btype].insert(rval);
    isPlace = true;
  }
  for (auto p : paramTypes) {
    if (p->isUnresolvedPlaceholder()) {
      placeholderRefs_[p].insert(rval);
      isPlace = true;
    }
  }
  for (auto r : resultTypes) {
    if (r->isUnresolvedPlaceholder()) {
      placeholderRefs_[r].insert(rval);
      isPlace = true;
    }
  }
  if (isPlace)
    rval->setPlaceholder(true);

  assert(!isPlace || !followsCabi);

  if (traceLevel() > 1) {
    std::cerr << "\n^ function type "
              << ((void*)rval) << " [llvm type "
              << ((void*) llft) << "]\n";
    rval->dump();
  }

  // Install in cache
  anonTypes_.insert(rval);

  return rval;
}

Btype *TypeManager::arrayType(Btype *elemType, Bexpression *length)
{
  if (length == errorExpression_ || elemType == errorType_)
    return errorType_;

  // The length expression provided needs to be immediately available
  assert(length->value());
  assert(llvm::isa<llvm::ConstantInt>(length->value()));

  // Manufacture corresponding LLVM type
  llvm::ConstantInt *lc = llvm::cast<llvm::ConstantInt>(length->value());
  uint64_t asize = lc->getValue().getZExtValue();
  llvm::Type *llat = llvm::ArrayType::get(elemType->type(), asize);

  // Check for and return existing anon type if we have one already
  Location loc;
  BArrayType cand(elemType, length, llat, loc);
  auto it = anonTypes_.find(&cand);
  if (it != anonTypes_.end()) {
    Btype *existing = *it;
    BArrayType *bat = existing->castToBArrayType();
    assert(bat);
    return bat;
  }

  // Create appropriate Btype
  BArrayType *rval = new BArrayType(elemType, length, llat, loc);
  rval->setPlaceholder(addPlaceholderRefs(rval));

  if (traceLevel() > 1) {
    std::cerr << "\n^ array type "
              << ((void*)rval) << " [llvm type "
              << ((void*) llat) << "] sz="
              << asize << " elementType:";
    std::cerr << ((void*) elemType) << "\n";
    rval->dump();
  }

  // Install in cache and return
  anonTypes_.insert(rval);
  return rval;
}

// Create a placeholder for an array type.

Btype *TypeManager::placeholderArrayType(const std::string &name,
                                         Location location)
{
  Btype *pat = new BArrayType(name, location);
  llvm::Type *opaque = makeOpaqueLlvmType("PAT");
  pat->setType(opaque);
  placeholders_.insert(pat);
  return pat;
}

// Here "typ" is assumed to be the Btype of the "function" expression
// feeding into a call, which can either be raw pointer-to-function or
// pointer-to-function-descriptor. This helper picks out and returns
// the underlying BFunctionType in either of those two cases.

BFunctionType *TypeManager::unpackFunctionType(Btype *typ)
{
  assert(typ);
  assert(typ != errorType_);
  BPointerType *pt = typ->castToBPointerType();
  if (pt)
    typ = pt->toType();
  BFunctionType *ft = typ->castToBFunctionType();
  if (ft)
    return ft;
  BStructType *st = typ->castToBStructType();
  assert(st);
  Btype *sft = st->fieldType(0);
  pt = sft->castToBPointerType();
  assert(pt);
  ft = pt->toType()->castToBFunctionType();
  assert(ft);
  return ft;
}

Btype *TypeManager::elementTypeByIndex(Btype *btype, unsigned fieldIndex)
{
  assert(btype);
  assert(btype != errorType_);
  BStructType *st = btype->castToBStructType();
  if (st)
    return st->fieldType(fieldIndex);
  BComplexType *ct = btype->castToBComplexType();
  if (ct)
    return floatType(ct->bits()/2);
  BArrayType *at = btype->castToBArrayType();
  assert(at);
  return at->elemType();
}

void TypeManager::postProcessResolvedPointerPlaceholder(BPointerType *bpt,
                                                        Btype *btype)
{
  assert(bpt);
  assert(bpt->isPlaceholder());
  llvm::Type *newllpt = llvm::PointerType::get(btype->type(), addressSpace_);

  // pluck out of anonTypes if stored there
  bool wasHashed = removeAnonType(bpt);

  bpt->setType(newllpt);
  if (traceLevel() > 1) {
    std::cerr << "\n^ resolving placeholder pointer type "
              << ((void*)bpt) << " to concrete target type; "
              << "new LLVM type "
              << ((void*) newllpt) << ", resolved type now:\n";
    bpt->dump();
  }
  bpt->setPlaceholder(false);

  // put back into anonTypes if it was there previously
  if (wasHashed)
    reinstallAnonType(bpt);

  postProcessResolvedPlaceholder(bpt);
}

void TypeManager::postProcessResolvedStructPlaceholder(BStructType *bst,
                                                       Btype *btype)
{
  assert(bst);
  assert(bst->isPlaceholder());

  std::vector<Btyped_identifier> fields(bst->fields());
  bool hasPl = false;
  for (unsigned i = 0; i < fields.size(); ++i) {
    const Btype *ft = fields[i].btype;
    if (btype == ft) {
      if (traceLevel() > 1) {
        std::cerr << "\n^ resolving field " << i << " of "
                  << "placeholder struct type "
                  << ((void*)bst) << " to concrete field type "
                  << ((void*)btype) << "\n";
      }
    } else if (fields[i].btype->isUnresolvedPlaceholder()) {
      hasPl = true;
    }
  }
  if (! hasPl) {

    bst->setPlaceholder(false);

    // If 'bst' corresponds to a named type that was cloned from an
    // anonymous type that (at the time) was a placeholder, then
    // it's possible that the LLVM type in question has already been
    // updated (if the anon type was processed first).
    llvm::StructType *llst = llvm::cast<llvm::StructType>(bst->type());
    if (llst->isOpaque()) {

      // No need to unhash the type -- we can simply update it in place.
      llvm::SmallVector<llvm::Type *, 64> elems(fields.size());
      for (unsigned i = 0; i < fields.size(); ++i)
        elems[i] = fields[i].btype->type();
      llst->setBody(elems);
    }

    if (traceLevel() > 1) {
      std::cerr << "\n^ resolving placeholder struct type "
                << ((void*)bst) << " to concrete struct type:\n";
      bst->dump();
    }

    postProcessResolvedPlaceholder(bst);
  }
}

void TypeManager::postProcessResolvedArrayPlaceholder(BArrayType *bat,
                                                       Btype *btype)
{
  assert(bat);
  assert(bat->isPlaceholder());

  // Create new resolved LLVM array type.
  uint64_t asize = bat->nelSize();
  llvm::Type *newllat = llvm::ArrayType::get(btype->type(), asize);

  // pluck out of anonTypes if stored there
  bool wasHashed = removeAnonType(bat);

  // update
  bat->setType(newllat);
  if (traceLevel() > 1) {
    std::cerr << "\n^ resolving placeholder array type "
              << ((void*)bat) << " elem type: "
              << "new LLVM type "
              << ((void*) newllat) << ", resolved type now:\n";
    bat->dump();
  }
  bat->setPlaceholder(false);

  // put back into anonTypes if it was there previously
  if (wasHashed)
    reinstallAnonType(bat);

  postProcessResolvedPlaceholder(bat);
}

void TypeManager::postProcessResolvedFunctionPlaceholder(BFunctionType *bft,
                                                         Btype *btype)
{
  assert(bft);
  assert(bft->isPlaceholder());

  // Create a new resolved LLVM function type.
  CABIOracle abiOracle(bft, this);
  llvm::FunctionType *newllft = abiOracle.getFunctionTypeForABI();

  // pluck out of anonTypes if stored there
  bool wasHashed = removeAnonType(bft);

  // update
  bft->setType(newllft);
  if (traceLevel() > 1) {
    std::cerr << "\n^ resolving placeholder function type "
              << ((void*)bft) << "new LLVM type "
              << ((void*) newllft) << ", resolved type now:\n";
    bft->dump();
  }
  bft->setPlaceholder(false);

  // put back into anonTypes if it was there previously
  if (wasHashed)
    reinstallAnonType(bft);

  postProcessResolvedPlaceholder(bft);
}

// When one of the "set_placeholder_*_type()" methods is called to
// resolve a placeholder type PT to a concrete type CT, we then need
// to chase down other types that refer to PT. For example, there
// might be a separate struct type ST that has a field with type ST --
// if this is ST's only placeholder field, then when PT is
// "concretized" we can also "concretize" ST. This helper manages this
// process.  It is worth noting that the types that we're updating may
// be installed in the anonTypes_ table, meaning that to make any
// changes we need to unhash it (remove it from the table), then apply
// the changes, then add it back into the table. During the addition,
// the new type may collide with some other existing type in the
// table. If this happens, we record the type in the separate
// placeholders_ set, so as to make sure we can delete it at the end
// of the compilation (this is managed by reinstallAnonType).
//
void TypeManager::postProcessResolvedPlaceholder(Btype *btype)
{
  auto it = placeholderRefs_.find(btype);
  if (it == placeholderRefs_.end())
    return;

  for (auto refType : it->second) {

    if (!refType->isPlaceholder())
      continue;

    BPointerType *bpt = refType->castToBPointerType();
    if (bpt) {
      postProcessResolvedPointerPlaceholder(bpt, btype);
      continue;
    }

    BArrayType *bat = refType->castToBArrayType();
    if (bat) {
      postProcessResolvedArrayPlaceholder(bat, btype);
      continue;
    }

    BStructType *bst = refType->castToBStructType();
    if (bst) {
      postProcessResolvedStructPlaceholder(bst, btype);
      continue;
    }

    BFunctionType *bft = refType->castToBFunctionType();
    if (bft) {
      postProcessResolvedFunctionPlaceholder(bft, btype);
      continue;
    }

    // Should never get here
    assert(false);
  }
}

// Set the real target type for a placeholder pointer type.

bool TypeManager::setPlaceholderPointerType(Btype *placeholder,
                                            Btype *to_type)
{
  assert(placeholder);
  assert(to_type);
  if (placeholder == errorType_ || to_type == errorType_)
    return false;

  // The frontend may pass a C function type as a function pointer
  // type (e.g. in the interface mtable struct). Make a function
  // pointer type here.
  if (to_type->castToBFunctionType())
    to_type = pointerType(to_type);

  assert(to_type->type()->isPointerTy());
  assert(placeholders_.find(placeholder) != placeholders_.end());

  // This function may be called by the frontend multiple times on the same
  // type. At the second time, the type is no longer marked as placeholder,
  // and the types referencing this type are already finalized. Don't update
  // it in this case.
  // Circular function/pointer types have isPlaceholder false, but they do
  // need to resolve, so special-case them.
  if (!placeholder->isPlaceholder() &&
      circularPointerTypes_.find(placeholder->type()) == circularPointerTypes_.end() &&
      circularFunctionTypes_.find(placeholder->type()) == circularFunctionTypes_.end())
    return true;

  assert(anonTypes_.find(placeholder) == anonTypes_.end());

  // For circular types, intercept the cycle with the marker type, so we
  // won't generate a self-referential type.
  auto cpit = circularPointerTypeMap_.find(placeholder);
  if (cpit != circularPointerTypeMap_.end()) {
    Btype *cpt = cpit->second;
    if (to_type != cpt) {
      // Link the marker type with the concrete type.
      BPointerType *bpt = cpt->castToBPointerType();
      BPointerType *ttpt = to_type->castToBPointerType();
      Btype *elt = ttpt->toType();
      bpt->setToType(elt);

      if (traceLevel() > 1) {
        std::cerr << "\n^ link circular pointer type "
                  << ((void*)cpt) << " [llvm type "
                  << ((void*)cpt->type()) << "]"
                  << " to concrete type " << ((void*) to_type)
                  << " [llvm type " << ((void*) to_type->type())
                  << "]\n";
        std::cerr << "marker: "; cpt->dump();
        std::cerr << "redir: "; to_type->dump();
      }

      // Circular pointer type handling
      circularConversionLoadMap_[cpt->type()] = elt;
      circularConversionAddrMap_[elt->type()] = cpt;

      return setPlaceholderPointerType(placeholder, cpt);
    }
  }

  if (traceLevel() > 1) {
    std::cerr << "\n^ placeholder pointer "
              << ((void*)placeholder) << " [llvm type "
              << ((void*) placeholder->type()) << "]"
              << " redirected to " << ((void*) to_type)
              << " [llvm type " << ((void*) to_type->type())
              << "]\n";
    std::cerr << "placeholder: "; placeholder->dump();
    std::cerr << "redir: "; to_type->dump();
  }

  auto it = circularFunctionTypes_.find(placeholder->type());
  if (it != circularFunctionTypes_.end())
    circularFunctionTypes_.insert(to_type->type());

  // Update the target type for the pointer
  BPointerType *bpt = placeholder->castToBPointerType();
  BPointerType *ttpt = to_type->castToBPointerType();
  bpt->setToType(ttpt->toType());
  bpt->setType(to_type->type());

  // Decide what to do next.
  if (to_type->isUnresolvedPlaceholder()) {
    // We're redirecting this type to another placeholder -- delay the
    // creation of the final LLVM type and record the reference.
    placeholderRefs_[to_type].insert(placeholder);
  } else {
    // The target is a concrete type. Reset the placeholder flag on
    // this type, then call a helper to update any other types that
    // might refer to this one.
    placeholder->setPlaceholder(false);
    postProcessResolvedPlaceholder(placeholder);
  }

  return true;
}

// Set the real values for a placeholder function type.

bool TypeManager::setPlaceholderFunctionType(Btype *placeholder,
                                             Btype *ft)
{
  return setPlaceholderPointerType(placeholder, ft);
}

// Fill in the fields of a placeholder struct type.

bool
TypeManager::setPlaceholderStructType(Btype *placeholder,
                             const std::vector<Btyped_identifier> &rawfields)
{
  if (placeholder == errorType_)
    return false;
  for (unsigned i = 0; i < rawfields.size(); ++i) {
    if (rawfields[i].btype == errorType_)
      return false;
  }
  std::vector<Btyped_identifier> fields = sanitizeFields(rawfields);
  assert(anonTypes_.find(placeholder) == anonTypes_.end());
  assert(placeholders_.find(placeholder) != placeholders_.end());
  BStructType *phst = placeholder->castToBStructType();
  assert(phst);
  phst->setFields(fields);

  // If we still have fields with placeholder types, then we still can't
  // manufacture a concrete LLVM type. If no placeholders, then we can
  // materialize the final LLVM type using a setBody call.
  bool isplace = addPlaceholderRefs(phst);
  if (! isplace) {
    llvm::StructType *llst = llvm::cast<llvm::StructType>(phst->type());
    llvm::SmallVector<llvm::Type *, 8> fieldtypes(fields.size());
    for (unsigned idx = 0; idx < fields.size(); ++idx)
      fieldtypes[idx] = fields[idx].btype->type();
    llst->setBody(fieldtypes);
    phst->setPlaceholder(false);
  }

  if (traceLevel() > 1) {
    std::cerr << "\n^ placeholder struct "
              << ((void*)placeholder) << " [llvm type "
              << ((void*) placeholder->type())
              << "] body redirected:\n";
    std::cerr << "placeholder: "; placeholder->dump();
    std::cerr << "fields:\n";
    for (unsigned i = 0; i < fields.size(); ++i) {
      std::cerr << i << ": ";
      fields[i].btype->dump();
    }
  }

  if (! isplace)
    postProcessResolvedPlaceholder(phst);

  return true;
}

// Fill in the components of a placeholder array type.

bool TypeManager::setPlaceholderArrayType(Btype *placeholder,
                                           Btype *element_btype,
                                           Bexpression *length) {
  if (placeholder == errorType_ || element_btype == errorType_ ||
      length == errorExpression_)
    return false;

  assert(anonTypes_.find(placeholder) == anonTypes_.end());
  assert(placeholders_.find(placeholder) != placeholders_.end());

  BArrayType *phat = placeholder->castToBArrayType();
  assert(phat);
  phat->setElemType(element_btype);
  phat->setNelements(length);
  bool isplace = addPlaceholderRefs(phat);

  uint64_t asize = phat->nelSize();
  llvm::Type *newllat = llvm::ArrayType::get(element_btype->type(), asize);
  Btype *atype = makeAuxType(newllat);
  atype->setPlaceholder(isplace);

  if (traceLevel() > 1) {
    std::string ls;
    llvm::raw_string_ostream os(ls);
    length->osdump(os);
    std::cerr << "\n^ placeholder array "
              << ((void*)placeholder) << " [llvm type "
              << ((void*) placeholder->type())
              << "] element type updated to " << ((void*) element_btype)
              << " [llvm type " << ((void*) element_btype->type())
              << "] length: " << ls
              << "\n";
    std::cerr << "placeholder: "; placeholder->dump();
    std::cerr << "redir: "; atype->dump();
  }

  updatePlaceholderUnderlyingType(placeholder, atype);

  return true;
}

// Return a named version of a type.

Btype *TypeManager::namedType(const std::string &name,
                              Btype *btype,
                              Location location)
{
  // TODO: add support for debug metadata

  Btype *rval = btype->clone();
  rval->setLocation(location);
  addPlaceholderRefs(rval);
  rval->setName(name);
  namedTypes_.insert(rval);
  revNames_[btype] = rval;

  if (traceLevel() > 1) {
    std::cerr << "\n^ named type '" << name << "' "
              << ((void*)rval) << " created from "
              << ((void*)btype) << " [llvm type "
              << ((void*) rval->type()) << "]\n";
    rval->dump();
  }

  return rval;
}

// Return a pointer type used as a marker for a circular type.

// Consider the following Go code:
//
//      type s *p
//      type p *q
//      type q *r
//      type r *p
//
// Here we have a cycle involving p/q/r, plus another type "p" that
// points into the cycle. When the front end detects the cycle it will
// flag "p" as circular, meaning that circular_pointer_type will
// wind up being invoked twice (once for real due to the cycle and
// once due to the jump into the cycle). We use a map to detect
// the second instance (e.g. "s") so that we can just return whatever
// we created before.

Btype *TypeManager::circularPointerType(Btype *placeholder, bool isfunc) {
  assert(placeholder);
  if (placeholder == errorType_)
    return errorType_;

  // Front end call this multiple times on the same placeholder, so we
  // cache markers and return the cached value on successive lookups.
  auto it = circularPointerTypeMap_.find(placeholder);
  if (it != circularPointerTypeMap_.end())
    return it->second;

  // Create a marker type.
  Btype *rval = new BPointerType("", placeholder->location());
  llvm::Type *opaque = makeOpaqueLlvmType(isfunc ? "CFT" : "CPT");
  llvm::Type *circ_typ = llvm::PointerType::get(opaque, addressSpace_);
  placeholders_.insert(rval);
  rval->setType(circ_typ);
  rval->setPlaceholder(false);
  circularPointerTypeMap_[placeholder] = rval;

  if (isfunc) {
    // Push marker and placeholder onto a stack so that we can
    // update them when the appropriate function type is created.
    circularFunctionPlaceholderTypes_.insert(placeholder);
    circularFunctionTypes_.insert(rval->type());
  } else {
    // Set up to start tracking the types that will make up the
    // loop involved in the cycle.
    circularPointerTypes_.insert(circ_typ);
  }

  if (traceLevel() > 1) {
    std::cerr << "\n^ circular_pointer_type "
              << "for placeholder " << ((void *)placeholder)
              << " [llvm type " << ((void *)placeholder->type())
              << "] returned type is " << ((void *)rval) << " [llvm type "
              << ((void *)rval->type()) << "]\n";
    placeholder->dump();
    rval->dump();
  }

  return rval;
}

// Return whether we might be looking at a circular function type.

bool TypeManager::isCircularFunctionType(Btype *btype) {
  assert(btype);
  auto it = circularFunctionPlaceholderTypes_.find(btype);
  if (it != circularFunctionPlaceholderTypes_.end())
    return true;
  return isCircularFunctionType(btype->type());
}

bool TypeManager::isCircularFunctionType(llvm::Type *typ) {
  assert(typ);
  auto it = circularFunctionTypes_.find(typ);
  return it != circularFunctionTypes_.end();
}

// Return whether we might be looking at a circular pointer type.

bool TypeManager::isCircularPointerType(Btype *btype) {
  assert(btype);
  return isCircularPointerType(btype->type());
}

bool TypeManager::isCircularPointerType(llvm::Type *typ) {
  assert(typ);
  auto it = circularPointerTypes_.find(typ);
  return it != circularPointerTypes_.end();
}

Btype *TypeManager::circularTypeLoadConversion(Btype *typ) {
  auto it = circularConversionLoadMap_.find(typ->type());
  return it != circularConversionLoadMap_.end()  ? it->second : nullptr;
}

Btype *TypeManager::circularTypeAddrConversion(Btype *typ) {
  auto it = circularConversionAddrMap_.find(typ->type());
  if (it != circularConversionAddrMap_.end())
    return it->second;
  return nullptr;
}

// Given a zero-sized type, create a similarly-structured type that
// has non-zero size. This is hacky, and there aren't really any firm
// rules here, but the general idea is to manufacture something that is
// as close to the original type as possible. For example, if the original
// type is a struct type with three fields A, B, and C, then we want the
// synthesized type to also be a struct type with three similarly named
// fields. For arrays we simply increase the number of elements from zero
// to one -- this is a risky strategy in that someone could define a global
// variable that looks like
//
//     type huge struct {
//       x [1<<30]char
//     }
//     type emptyar [0]huge
//
// however this scenario does not seem especially likely (most global
// zero-sized types are likely due to the use of interfaces).

Btype *TypeManager::synthesizeNonZeroSizeType(Btype *typ, Bexpression *one)
{
  if (typeSize(typ) != 0)
    return typ;
  switch(typ->flavor()) {
    case Btype::ArrayT: {
      BArrayType *bat = typ->castToBArrayType();
      Btype *elemtyp = synthesizeNonZeroSizeType(bat->elemType(), one);
      return arrayType(elemtyp, one);
    }
    case Btype::StructT: {
      BStructType *bst = typ->castToBStructType();
      const std::vector<Backend::Btyped_identifier> &fields = bst->fields();
      if (fields.size()) {
        std::vector<Backend::Btyped_identifier> nzfields = fields;
        nzfields[0].btype = synthesizeNonZeroSizeType(fields[0].btype, one);
        return structType(nzfields);
      }
      std::vector<Backend::Btyped_identifier> dummyfields(1);
      dummyfields[0].btype = boolType();
      dummyfields[0].name = "dummy";
      return structType(dummyfields);
    }
    default: {
      assert(false);
    }
  }
  return nullptr;
}

llvm::Type *TypeManager::landingPadExceptionType()
{
  llvm::SmallVector<llvm::Type *, 2> elems(2);
  elems[0] = llvmPtr0Type();
  elems[1] = llvmInt32Type();
  return llvm::StructType::get(context_, elems);
}

llvm::FunctionType *TypeManager::personalityFunctionType()
{
  const bool isVarargs = false;
  llvm::SmallVector<llvm::Type *, 5> elems(5);
  elems[0] = llvmInt32Type();
  elems[1] = llvmInt32Type();
  elems[2] = llvmInt64Type();
  elems[3] = llvmPtrType();
  elems[4] = llvmPtrType();
  llvm::FunctionType *llft =
      llvm::FunctionType::get(llvmInt32Type(), elems, isVarargs);
  return llft;
}

bool TypeManager::isLlvmCompositeType(llvm::Type *t)
{
  return llvm::isa<llvm::StructType>(t) || llvm::isa<llvm::ArrayType>(t) || llvm::isa<llvm::VectorType>(t);
}

llvm::Type *TypeManager::getLlvmTypeAtIndex(llvm::Type *t, unsigned i)
{
  if (llvm::isa<llvm::StructType>(t)) {
    llvm::StructType *st = llvm::cast<llvm::StructType>(t);
    return st->getTypeAtIndex(i);
  } else if (llvm::isa<llvm::ArrayType>(t)) {
    llvm::ArrayType *at = llvm::cast<llvm::ArrayType>(t);
    return at->getElementType();
  } else {
    assert(llvm::isa<llvm::VectorType>(t));
    return t->getScalarType();
  }
}

llvm::Type *TypeManager::placeholderProxyType(Btype *typ,
                                              pproxymap *pmap)
{
  llvm::SmallPtrSet<llvm::Type *, 32> vis;
  if (typ->type()->isSized(&vis))
    return typ->type();
  auto it = pmap->find(typ);
  if (it != pmap->end())
    return it->second;
  switch(typ->flavor()) {
    case Btype::ArrayT: {
      BArrayType *bat = typ->castToBArrayType();
      Btype *elt = bat->elemType();
      llvm::Type *elprox = placeholderProxyType(elt, pmap);
      if (!elprox)
        return nullptr;
      llvm::Type *llat = llvm::ArrayType::get(elprox, bat->nelSize());
      (*pmap)[bat] = llat;
      return llat;
    }
    case Btype::StructT: {
      BStructType *bst = typ->castToBStructType();
      const std::vector<Backend::Btyped_identifier> &fields = bst->fields();
      llvm::SmallVector<llvm::Type *, 64> elems(fields.size());
      for (unsigned i = 0; i < fields.size(); ++i) {
        llvm::Type *ft = placeholderProxyType(fields[i].btype, pmap);
        if (!ft)
          return nullptr;
        elems[i] = ft;
      }
      llvm::Type *llst = llvm::StructType::get(context_, elems);
      (*pmap)[bst] = llst;
      return llst;
    }
    case Btype::PointerT: {
      // All pointers should be sized the same
      return llvmPtrType_;
    }
    case Btype::FunctionT: {
      // This is hacky, but it turns out we do need this
      // because of the way FE handles function types.
      // Treat function type as pointer-to-function type.
      return llvmPtrType_;
    }
    case Btype::AuxT: {
      assert(false && "not expecting aux type here");
    }
    default: {
      assert(false && "not expecting scalar type here");
    }
  }
  return nullptr;
}

// Helper for use with typeSize() and typeFieldOffset methods.
// Handles situations where we're asking about size/offset for
// types that still incorporate placeholders.
llvm::Type *TypeManager::getPlaceholderProxyIfNeeded(Btype *btype)
{
  llvm::Type *toget = btype->type();
  llvm::SmallPtrSet<llvm::Type *, 32> vis;
  if (!btype->type()->isSized(&vis)) {
    pproxymap pmap;
    toget = placeholderProxyType(btype, &pmap);
    assert(toget);
  }
  return toget;
}

// Return the size of a type.

// Note: frontend sometimes asks for the size of a placeholder
// type that has not been finalized -- this is a bit tricky since
// at that point we don't have an LLVM type. If this happens, call
// a helper to fake it (since in many cases we'll know the size
// of the type even if placeholder pointer types have not been resolved).

int64_t TypeManager::typeSize(Btype *btype) {
  if (btype == errorType_)
    return 1;
  llvm::Type *toget = getPlaceholderProxyIfNeeded(btype);
  uint64_t uvalbytes = datalayout_->getTypeAllocSize(toget);
  return static_cast<int64_t>(uvalbytes);
}

uint64_t TypeManager::llvmTypeAllocSize(llvm::Type *t)
{
  return datalayout()->getTypeAllocSize(t);
}

uint64_t TypeManager::llvmTypeSize(llvm::Type *t)
{
  unsigned bits = datalayout()->getTypeSizeInBits(t);
  if (bits == 1) // special case for 1-bit int
    return 1;
  assert((bits & 7) == 0);
  return bits / 8;
}

// Return the alignment of a type.

int64_t TypeManager::typeAlignment(Btype *btype) {
  if (btype == errorType_)
    return 1;
  llvm::Type *toget = getPlaceholderProxyIfNeeded(btype);
  unsigned uval = datalayout_->getABITypeAlignment(toget);
  return static_cast<int64_t>(uval);
}

// Return the alignment of a struct field of type BTYPE.
//
// One case where type_field_align(X) != type_align(X) is
// for type 'double' on x86 32-bit, where for compatibility
// a double field is 4-byte aligned but will be 8-byte aligned
// otherwise.

int64_t TypeManager::typeFieldAlignment(Btype *btype) {
  if (btype == errorType_)
    return -1;

  llvm::Type *toget = getPlaceholderProxyIfNeeded(btype);

  // Corner cases.
  if (!toget->isSized())
    return -1;

  // Create a new anonymous struct with two fields: first field is a
  // single byte, second field is of type btype. Then use
  // getElementOffset to find out where the second one has been
  // placed. Finally, return min of alignof(btype) and that value.

  llvm::SmallVector<llvm::Type *, 2> elems(2);
  elems[0] = llvm::Type::getInt1Ty(context_);
  elems[1] = toget;
  llvm::StructType *dummyst = llvm::StructType::get(context_, elems);
  const llvm::StructLayout *sl = datalayout_->getStructLayout(dummyst);
  uint64_t uoff = sl->getElementOffset(1);
  unsigned talign = datalayout_->getABITypeAlignment(toget);
  int64_t rval = (uoff < talign ? uoff : talign);
  return rval;
}

// Return the offset of a field in a struct.

int64_t TypeManager::typeFieldOffset(Btype *btype, size_t index) {
  if (btype == errorType_)
    return 0;

  llvm::Type *toget = getPlaceholderProxyIfNeeded(btype);
  assert(toget->isStructTy());
  llvm::StructType *llvm_st = llvm::cast<llvm::StructType>(toget);
  return llvmTypeFieldOffset(llvm_st, index);
}

// Return the offset of a field in an LLVM struct type.

int64_t TypeManager::llvmTypeFieldOffset(llvm::StructType *llst, size_t fidx)
{
  const llvm::StructLayout *sl = datalayout_->getStructLayout(llst);
  uint64_t uoff = sl->getElementOffset(fidx);
  return static_cast<int64_t>(uoff);
}

bool TypeManager::isPtrToIfaceStructType(llvm::Type *typ)
{
  if (! typ->isPointerTy())
    return false;
  llvm::PointerType *pt = llvm::cast<llvm::PointerType>(typ);
  llvm::Type *elt = pt->getElementType();
  if (! elt->isStructTy())
    return false;
  llvm::StructType *st = llvm::cast<llvm::StructType>(elt);
  if (st->getNumElements() != 2)
    return false;
  llvm::SmallPtrSet<llvm::Type *, 32> vis;
  // expect { ptr, ptr } or { ptr, uintptr }
  return (st->getElementType(0)->isPointerTy() &&
          (st->getElementType(1)->isPointerTy() ||
           st->getElementType(1) == llvmIntegerType()));
}

bool TypeManager::isFuncDescriptorType(llvm::Type *typ)
{
  if (! typ->isStructTy())
    return false;
  llvm::StructType *st = llvm::cast<llvm::StructType>(typ);
  if (st->getNumElements() != 1)
    return false;
  llvm::Type *f0t = st->getElementType(0);
  llvm::PointerType *f0tpt = nullptr;
  if (f0t->isPointerTy())
    f0tpt = llvm::cast<llvm::PointerType>(f0t);
  if (f0t != llvmIntegerType_ &&
      !f0t->isFunctionTy() &&
      !(f0tpt && f0tpt->getElementType()->isFunctionTy()))
    return false;
  return true;
}

bool TypeManager::isPtrToFuncDescriptorType(llvm::Type *typ)
{
  if (! typ->isPointerTy())
    return false;
  llvm::PointerType *pt = llvm::cast<llvm::PointerType>(typ);
  return isFuncDescriptorType(pt->getElementType());
}

bool TypeManager::isPtrToFuncType(llvm::Type *typ)
{
  if (! typ->isPointerTy())
    return false;
  llvm::PointerType *pt = llvm::cast<llvm::PointerType>(typ);
  return pt->getElementType()->isFunctionTy();
}

bool TypeManager::isPtrToVoidType(llvm::Type *typ)
{
  if (! typ->isPointerTy())
    return false;
  llvm::PointerType *pt = llvm::cast<llvm::PointerType>(typ);
  return pt->getElementType() == llvmInt8Type_;
}

bool TypeManager::isPtrToArrayOf(llvm::Type *typ, llvm::Type *arElmTyp)
{
  if (! typ->isPointerTy())
    return false;
  llvm::PointerType *pt = llvm::cast<llvm::PointerType>(typ);
  llvm::Type *elt = pt->getElementType();
  if (! elt->isArrayTy())
    return false;
  llvm::ArrayType *llat = llvm::cast<llvm::ArrayType>(elt);
  llvm::Type *aelt = llat->getElementType();
  if (aelt == arElmTyp)
    return true;
  if (isCircularFunctionType(aelt) && isCircularFunctionType(arElmTyp))
    return true; // TODO: check they are same circular function type?
  return false;
}

bool TypeManager::fcnPointerCompatible(llvm::Type *left,
                                       llvm::Type *right,
                                       std::set<llvm::Type *> &visited)
{
  // Allow for pointer-to-fp and func-desc matching
  bool leftFPD = isPtrToFuncType(left) || isPtrToVoidType(left);
  bool rightFPD = isPtrToFuncType(right) || isPtrToVoidType(right);
  if (leftFPD && rightFPD)
    return true;

  bool visleft = (visited.find(left) != visited.end());
  bool visright = (visited.find(right) != visited.end());
  if (visleft != visright)
    return false;
  if (visleft)
    return true;
  visited.insert(left);
  visited.insert(right);

  // Compare type ID, children, etc.
  if (left->getTypeID() != right->getTypeID())
    return false;

  // For pointer types, visit pointed-to elements
  if (left->isPointerTy()) {
    llvm::PointerType *ptl = llvm::cast<llvm::PointerType>(left);
    llvm::PointerType *ptr = llvm::cast<llvm::PointerType>(right);
    llvm::Type *eltl = ptl->getElementType();
    llvm::Type *eltr = ptr->getElementType();
    return fcnPointerCompatible(eltl, eltr, visited);
  }

  // For aggregate types, compare children.
  if (left->isAggregateType()) {
    unsigned leftnct = left->getNumContainedTypes();
    unsigned rightnct = right->getNumContainedTypes();
    if (leftnct != rightnct)
      return false;
    for (unsigned cti = 0; cti < leftnct; cti++) {
      llvm::Type *leftchild = left->getContainedType(cti);
      llvm::Type *rightchild = right->getContainedType(cti);
      if (!fcnPointerCompatible(leftchild, rightchild, visited))
        return false;
    }
    return true;
  } else {
    // For non-aggregate types, we expect underlying llvm types to match
    return (left == right);
  }
}

std::string TypeManager::typToString(Btype *typ)
{
  std::map<Btype *, std::string> smap;
  return typToStringRec(typ, smap);
}

std::string
TypeManager::typToStringRec(Btype *typ, std::map<Btype *, std::string> &smap)
{
  assert(typ != nullptr);
  if (namedTypes_.find(typ) != namedTypes_.end())
    return typ->name();
  if (! typ->name().empty())
    return typ->name();

  auto rnit = revNames_.find(typ);
  if (rnit != revNames_.end())
    return rnit->second->name();

  auto smit = smap.find(typ);
  if (smit != smap.end())
    return smit->second;

  std::stringstream ss;
  switch(typ->flavor()) {
    case Btype::AuxT: {
      if (typ->type() == llvmVoidType_) {
        ss << "void";
        break;
      }
      std::cerr << "unhandled aux type: "; typ->dump();
      assert(false);
      break;
    }
    case Btype::ComplexT: {
      BComplexType *bct = typ->castToBComplexType();
      ss << "complex" << bct->bits();
      break;
    }
    case Btype::FloatT: {
      BFloatType *bft = typ->castToBFloatType();
      ss << "float" << bft->bits();
      break;
    }
    case Btype::IntegerT: {
      BIntegerType *bit = typ->castToBIntegerType();
      if (bit->isUnsigned())
        ss << "u";
      ss << "int" << bit->bits();
      break;
    }
    case Btype::PointerT: {
      BPointerType *bpt = typ->castToBPointerType();

      // all placeholders should be resolved at this point
      assert(!bpt->isPlaceholder());

      // handle circular pointer types
      auto cpit = circularPointerTypes_.find(typ->type());
      if (cpit != circularPointerTypes_.end()) {
        std::string s;
        llvm::raw_string_ostream os(s);
        typ->type()->print(os);
        ss << os.str();
        break;
      }

      assert(bpt->toType() != nullptr);
      ss << "*" << typToStringRec(bpt->toType(), smap);
      break;
    }
    case Btype::StructT: {
      BStructType *bst = typ->castToBStructType();

      // install temp entry to break cycles
      std::stringstream sst;
      sst << "$struct$" << smap.size();
      smap[typ] = sst.str();

      ss << "struct{";
      const std::vector<Backend::Btyped_identifier> &fields = bst->fields();
      for (unsigned i = 0; i < fields.size(); ++i) {
        ss << (i == 0 ? "" : ",");
        ss << typToStringRec(fields[i].btype, smap);
      }
      ss << "}";
      break;
    }
    case Btype::FunctionT: {
      BFunctionType *bft = typ->castToBFunctionType();
      ss << "func(";
      if (bft->receiverType())
        ss << typToStringRec(bft->receiverType(), smap);
      const std::vector<Btype *> &parms = bft->paramTypes();
      for (unsigned i = 0; i < parms.size(); ++i) {
        ss << ((i == 0 && !bft->receiverType()) ? "" : ",");
        ss << typToStringRec(parms[i], smap);
      }
      ss << ")";
      ss << typToStringRec(bft->resultType(), smap);
      break;
    }
    case Btype::ArrayT: {
      BArrayType *bat = typ->castToBArrayType();
      ss << "[" << bat->nelSize() << "]"
         << typToStringRec(bat->elemType(), smap);
      break;
    }
    default: assert(0);
  }
  smap[typ] = ss.str();
  return ss.str();
}

llvm::DIType *TypeManager::buildStructDIType(BStructType *bst,
                                             DIBuildHelper &helper)
{
  auto rnit = revNames_.find(bst);
  if (rnit != revNames_.end())
    bst = rnit->second->castToBStructType();

  std::unordered_map<Btype *, llvm::DIType*> &typeCache = helper.typeCache();

  // Create replaceable placeholder
  llvm::DIBuilder &dibuilder = helper.dibuilder();
  unsigned tag = llvm::dwarf::DW_TAG_structure_type;
  llvm::DIScope *scope = helper.moduleScope();
  llvm::DIFile *file = helper.diFileFromLocation(bst->location());
  unsigned lineNumber = helper.linemap()->location_line(bst->location());
  llvm::DICompositeType *placeholder =
      dibuilder.createReplaceableCompositeType(tag, typToString(bst),
                                               scope, file, lineNumber);
  typeCache[bst] = placeholder;

  // Process struct members
  llvm::SmallVector<llvm::Metadata *, 16> members;
  const std::vector<Backend::Btyped_identifier> &fields = bst->fields();
  for (unsigned fidx = 0; fidx < bst->fields().size(); ++fidx) {
    const Backend::Btyped_identifier &field = fields[fidx];
    llvm::DIType *fieldType = buildDIType(field.btype, helper);
    uint64_t memberBits = typeSize(field.btype) * 8;
    uint32_t memberAlign = typeFieldAlignment(field.btype) * 8;
    uint64_t memberOffset = typeFieldOffset(bst, fidx) * 8;
    unsigned lineNumber = helper.linemap()->location_line(field.location);
    llvm::DIDerivedType *ft =
        dibuilder.createMemberType(scope, field.name, file, lineNumber,
                                   memberBits, memberAlign, memberOffset,
                                   llvm::DINode::FlagZero,
                                   fieldType);
    members.push_back(ft);
  }
  auto memberArray = dibuilder.getOrCreateArray(members);

  // Now create struct type itself.  Q: should this be
  // getTypeAllocSize here instead of getTypeSizeInBits?
  uint64_t sizeInBits = datalayout_->getTypeSizeInBits(bst->type());
  uint32_t alignInBits = datalayout_->getABITypeAlignment(bst->type());
  llvm::DIType *derivedFrom = nullptr;
  llvm::DICompositeType *dist =
      dibuilder.createStructType(scope, typToString(bst),
                                 file, lineNumber, sizeInBits, alignInBits,
                                 llvm::DINode::FlagZero, derivedFrom,
                                 memberArray);

  // Replace the temp
  dibuilder.replaceTemporary(llvm::TempDIType(placeholder), dist);

  // Update cache
  typeCache[bst] = dist;

  // Done.
  return dist;
}

// DIBuilder has limited support for creating general self-referential
// types, e.g. there is no "::createReplaceableFunctionType" method,
// for example, so for the time being we pretend that the
// self-referential elements of circular function types are just
// pointer-to-void.

llvm::DIType *TypeManager::buildDIType(Btype *typ, DIBuildHelper &helper)
{
  llvm::DIBuilder &dibuilder = helper.dibuilder();

  std::unordered_map<Btype *, llvm::DIType*> &typeCache =
      helper.typeCache();
  auto tcit = typeCache.find(typ);
  if (tcit != typeCache.end()) {
    BPointerType *bpt = typ->castToBPointerType();
    if (bpt) {
      auto it = typeCache.find(bpt);
      if (it != typeCache.end() && it->second == nullptr)
        return buildDIType(pointerType(voidType()), helper);
    }
    assert(tcit->second != nullptr);
    return tcit->second;
  }

  // this indicates that we're currently working on creating a
  // DIType for this BType.
  typeCache[typ] = nullptr;

  llvm::DIType *rval = nullptr;
  switch(typ->flavor()) {
    case Btype::AuxT: {
      // FIXME: at the moment Aux types are only created for types
      // that have no direct Go correspondent (for example, the type
      // of an intrinsic function defined by LLVM and not Go) -- there
      // should be no need include such things in the debug meta data.
      // If it turns out that we do need to emit meta-data for an aux
      // type, more special-case code will need to be added here.
      if (typ->type() == llvmVoidType_) {
        rval = dibuilder.createBasicType("void", 0, 0);
        break;
      }
      auto it = revNames_.find(typ);
      if (it != revNames_.end()) {
        rval = buildDIType(it->second, helper);
        break;
      }

      std::cerr << "unhandled aux type: "; typ->dump();
      assert(false);
      return nullptr;
    }
    case Btype::ComplexT: {
      BComplexType *bct = typ->castToBComplexType();
      std::string tstr = typToString(typ);
      rval = dibuilder.createBasicType(tstr, bct->bits(),
                                       llvm::dwarf::DW_ATE_complex_float);
      break;
    }
    case Btype::FloatT: {
      BFloatType *bft = typ->castToBFloatType();
      std::string tstr = typToString(typ);
      rval = dibuilder.createBasicType(tstr, bft->bits(),
                                       llvm::dwarf::DW_ATE_float);
      break;
    }
    case Btype::IntegerT: {
      const BIntegerType *bit = typ->castToBIntegerType();
      unsigned encoding =
          (bit->isUnsigned() ?
           llvm::dwarf::DW_ATE_unsigned :
           llvm::dwarf::DW_ATE_signed);
      std::string tstr = typToString(typ);
      rval = dibuilder.createBasicType(tstr, bit->bits(), encoding);
      break;
    }
    case Btype::PointerT: {
      const BPointerType *bpt = typ->castToBPointerType();

      // If this type is still an unresolved placeholder, treat
      // this as an indication that we don't need accurate debug
      // info for this type.
      if (bpt->isPlaceholder())
        return buildDIType(pointerType(voidType()), helper);

      assert(bpt->toType() != nullptr);

      // If we're pointing to something unresolved, be
      // conservative.
      if (bpt->toType()->isPlaceholder())
        return buildDIType(pointerType(voidType()), helper);

      // For some circular function types we need to check for
      // cycles here (in addition to the check above).
      // FIXME: look into more robust handling of circular
      // types in debug generation.
      auto it = typeCache.find(bpt->toType());
      if (it != typeCache.end() && it->second == nullptr)
        return buildDIType(pointerType(voidType()), helper);

      llvm::DIType *toDI = buildDIType(bpt->toType(), helper);
      uint64_t bits = datalayout_->getPointerSizeInBits();
      rval = dibuilder.createPointerType(toDI, bits);
      break;
    }
    case Btype::ArrayT: {
      const BArrayType *bat = typ->castToBArrayType();
      llvm::DIType *elemDI = buildDIType(bat->elemType(), helper);
      uint64_t arElems = bat->nelSize();
      uint64_t arSize = datalayout_->getTypeSizeInBits(bat->type());
      uint64_t arAlign =
          datalayout_->getABITypeAlignment(bat->elemType()->type());
      llvm::SmallVector<llvm::Metadata *, 1> subscripts;
      subscripts.push_back(dibuilder.getOrCreateSubrange(0, arElems));
      llvm::DINodeArray subsAr = dibuilder.getOrCreateArray(subscripts);
      rval = dibuilder.createArrayType(arSize, arAlign, elemDI, subsAr);
      break;
    }
    case Btype::StructT: {
      BStructType *bst = typ->castToBStructType();
      rval = buildStructDIType(bst, helper);
      break;
    }
    case Btype::FunctionT: {
      const BFunctionType *bft = typ->castToBFunctionType();
      llvm::SmallVector<llvm::Metadata *, 16> ptypes;
      ptypes.push_back(buildDIType(bft->resultType(), helper));
      if (bft->receiverType())
        ptypes.push_back(buildDIType(bft->receiverType(), helper));
      for (auto &pt : bft->paramTypes())
        ptypes.push_back(buildDIType(pt, helper));
      auto paramTypes = dibuilder.getOrCreateTypeArray(ptypes);
      rval = dibuilder.createSubroutineType(paramTypes);
      break;
    }
  }

  assert(rval != nullptr);
  typeCache[typ] = rval;
  return rval;
}
