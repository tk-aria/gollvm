//===-- go-llvm-builtins.cpp - BuiltinTable implementation ----------------===//
//
// Copyright 2018 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
//
//===----------------------------------------------------------------------===//
//
// Methods for BuiltinTable and related classes
//
//===----------------------------------------------------------------------===//

#include "go-llvm.h"
#include "go-llvm-builtins.h"
#include "go-llvm-bfunction.h"
#include "go-llvm-typemanager.h"

BuiltinEntry::~BuiltinEntry()
{
  if (flavor() == IntrinsicBuiltin)
    delete bfunction_;
}

void BuiltinEntry::setBfunction(Bfunction *bfunc)
{
  assert(! bfunction_);
  bfunction_ = bfunc;
}

//........................................................................

BuiltinTable::BuiltinTable(TypeManager *tman, bool addLongDouble)
    : tman_(tman), addLongDouble_(addLongDouble)
{
}

void BuiltinTable::registerIntrinsicBuiltin(const char *name,
                                            const char *libname,
                                            llvm::Intrinsic::ID intrinsicId,
                                            const BuiltinEntryTypeVec &overloadTypes)
{
  assert(lookup(name) == nullptr);
  assert(libname == nullptr || lookup(libname) == nullptr);
  unsigned idx = entries_.size();
  entries_.push_back(BuiltinEntry(intrinsicId, name,
                                  libname ? libname : "",
                                  overloadTypes));
  tab_[std::string(name)] = idx;
  if (libname)
    tab_[std::string(libname)] = idx;
}

void BuiltinTable::registerLibCallBuiltin(const char *libname,
                                          const char *name,
                                          llvm::LibFunc libfunc,
                                          const BuiltinEntryTypeVec &paramTypes)
{
  assert(lookup(name) == nullptr);
  assert(libname == nullptr || lookup(libname) == nullptr);
  unsigned idx = entries_.size();
  entries_.push_back(BuiltinEntry(libfunc, name,
                                  libname ? libname : "",
                                  paramTypes));
  tab_[std::string(name)] = idx;
  if (libname)
    tab_[std::string(libname)] = idx;
}

void BuiltinTable::registerExprBuiltin(const char *name,
                                       const char *libname,
                                       const BuiltinEntryTypeVec &paramTypes,
                                       BuiltinExprMaker exprMaker)
{
  assert(lookup(name) == nullptr);
  assert(libname == nullptr || lookup(libname) == nullptr);
  unsigned idx = entries_.size();
  entries_.push_back(BuiltinEntry(name,
                                  libname ? libname : "",
                                  paramTypes,
                                  exprMaker));
  tab_[std::string(name)] = idx;
  if (libname)
    tab_[std::string(libname)] = idx;
}

BuiltinEntry *BuiltinTable::lookup(const std::string &name)
{
  auto it = tab_.find(name);
  if (it == tab_.end())
    return nullptr;
  unsigned idx = it->second;
  assert(idx < entries_.size());
  return &entries_[idx];
}

void BuiltinTable::defineAllBuiltins() {
  defineSyncFetchAndAddBuiltins();
  defineIntrinsicBuiltins();
  defineTrigBuiltins();
  defineExprBuiltins();
}

void BuiltinTable::defineIntrinsicBuiltins() {
  Btype *boolType = tman_->boolType();
  Btype *ptrType = tman_->pointerType(boolType);
  Btype *uint16Type = tman_->integerType(true, 16);
  Btype *uint32Type = tman_->integerType(true, 32);
  Btype *int32Type = tman_->integerType(false, 32);
  unsigned bitsInPtr = tman_->datalayout()->getPointerSizeInBits();
  Btype *uintPtrType = tman_->integerType(true, bitsInPtr);
  Btype *sizeType = uintPtrType;
  Btype *uint64Type = tman_->integerType(true, 64);
  Btype *int64Type = tman_->integerType(false, 64);

  // A note on the types below:
  // - for intrinsic builtins, return type is implicitly defined
  //   by the intrinsic itself; param types can be overloaded
  // - for libcall builtins, return type appears as the first
  //   entry in the param type list

  defineIntrinsicBuiltin("__builtin_trap", nullptr, llvm::Intrinsic::trap,
                         nullptr);
  defineIntrinsicBuiltin("__builtin_return_address", nullptr,
                         llvm::Intrinsic::returnaddress, ptrType,
                         uint32Type, nullptr);
  defineIntrinsicBuiltin("__builtin_frame_address", nullptr,
                         llvm::Intrinsic::frameaddress, ptrType,
                         uint32Type, nullptr);
  defineIntrinsicBuiltin("__builtin_dwarf_cfa", nullptr,
                         llvm::Intrinsic::eh_dwarf_cfa, ptrType,
                         uint32Type, nullptr);

  defineIntrinsicBuiltin("__builtin_prefetch", nullptr, llvm::Intrinsic::prefetch,
                         ptrType, nullptr);

  defineIntrinsicBuiltin("__builtin_expect", nullptr, llvm::Intrinsic::expect,
                         int64Type, int64Type, nullptr);

  defineLibcallBuiltin("__builtin_memcmp", "memcmp",
                       llvm::LibFunc::LibFunc_memcmp,
                       int32Type, ptrType, ptrType,
                       sizeType, nullptr);

  defineIntrinsicBuiltin("__builtin_memcpy", "memcpy",
                         llvm::Intrinsic::memcpy,
                         ptrType, ptrType, sizeType, nullptr);

  defineIntrinsicBuiltin("__builtin_memmove", "memmove",
                         llvm::Intrinsic::memmove,
                         ptrType, ptrType, sizeType, nullptr);

  // go runtime refers to this intrinsic as "ctz", however the LLVM
  // equivalent is named "cttz".
  defineIntrinsicBuiltin("__builtin_ctz", "ctz", llvm::Intrinsic::cttz,
                         uint32Type, nullptr);

  // go runtime refers to this intrinsic as "ctzll", however the LLVM
  // equivalent is named "cttz".
  defineIntrinsicBuiltin("__builtin_ctzll", "ctzll", llvm::Intrinsic::cttz,
                         uint64Type, nullptr);

  // go runtime refers to this intrinsic as "clz", however the LLVM
  // equivalent is named "ctlz".
  defineIntrinsicBuiltin("__builtin_clz", "clz", llvm::Intrinsic::ctlz,
                         uint32Type, nullptr);

  // go runtime refers to this intrinsic as "clzll", however the LLVM
  // equivalent is named "ctlz".
  defineIntrinsicBuiltin("__builtin_clzll", "clzll", llvm::Intrinsic::ctlz,
                         uint64Type, nullptr);

  // go runtime refers to this intrinsic as "popcount", however the LLVM
  // equivalent is named "ctpop".
  defineIntrinsicBuiltin("__builtin_popcount", "popcount", llvm::Intrinsic::ctpop,
                         uint32Type, nullptr);

  // go runtime refers to this intrinsic as "popcountll", however the LLVM
  // equivalent is named "ctpop".
  defineIntrinsicBuiltin("__builtin_popcountll", "popcountll", llvm::Intrinsic::ctpop,
                         uint64Type, nullptr);

  // go runtime refers to this intrinsic as "bswap16", however the LLVM
  // equivalent is named just "bswap"
  defineIntrinsicBuiltin("__builtin_bswap16", "bswap16", llvm::Intrinsic::bswap,
                         uint16Type, nullptr);

  // go runtime refers to this intrinsic as "bswap32", however the LLVM
  // equivalent is named just "bswap"
  defineIntrinsicBuiltin("__builtin_bswap32", "bswap32", llvm::Intrinsic::bswap,
                         uint32Type, nullptr);

  // go runtime refers to this intrinsic as "bswap64", however the LLVM
  // equivalent is named just "bswap"
  defineIntrinsicBuiltin("__builtin_bswap64", "bswap64", llvm::Intrinsic::bswap,
                         uint64Type, nullptr);
}

namespace {

typedef enum {
  OneArg = 0,  // takes form "double foo(double)"
  TwoArgs = 1, // takes form "double bar(double, double)"
  TwoMixed = 2 // takes form "double bar(double, int)"
} mflav;

typedef struct {
  const char *name;
  mflav nargs;
  llvm::LibFunc lf;
} mathfuncdesc;
}

void BuiltinTable::defineTrigBuiltins() {
  Btype *doubleType = tman_->floatType(64);
  Btype *longDoubleType = tman_->floatType(128);
  Btype *int32Type = tman_->integerType(false, 32);

  BuiltinEntryTypeVec onearg_double(2);
  onearg_double[0] = doubleType;
  onearg_double[1] = doubleType;

  BuiltinEntryTypeVec onearg_long_double(2);
  onearg_long_double[0] = longDoubleType;
  onearg_long_double[1] = longDoubleType;

  BuiltinEntryTypeVec twoargs_double(3);
  twoargs_double[0] = doubleType;
  twoargs_double[1] = doubleType;
  twoargs_double[2] = doubleType;

  BuiltinEntryTypeVec twoargs_long_double(3);
  twoargs_long_double[0] = longDoubleType;
  twoargs_long_double[1] = longDoubleType;
  twoargs_long_double[2] = longDoubleType;

  BuiltinEntryTypeVec mixed_double(3);
  mixed_double[0] = doubleType;
  mixed_double[1] = doubleType;
  mixed_double[2] = int32Type;

  BuiltinEntryTypeVec mixed_long_double(3);
  mixed_long_double[0] = longDoubleType;
  mixed_long_double[1] = longDoubleType;
  mixed_long_double[2] = int32Type;

  std::vector<BuiltinEntryTypeVec *> signatures = {
      &onearg_double, &twoargs_double, &mixed_double};
  std::vector<BuiltinEntryTypeVec *> lsignatures = {
      &onearg_long_double, &twoargs_long_double, &mixed_long_double};

  static const mathfuncdesc funcs[] = {
      {"acos", OneArg, llvm::LibFunc::LibFunc_acos},
      {"asin", OneArg, llvm::LibFunc::LibFunc_asin},
      {"atan", OneArg, llvm::LibFunc::LibFunc_atan},
      {"atan2", TwoArgs, llvm::LibFunc::LibFunc_atan2},
      {"ceil", OneArg, llvm::LibFunc::LibFunc_ceil},
      {"cos", OneArg, llvm::LibFunc::LibFunc_cos},
      {"exp", OneArg, llvm::LibFunc::LibFunc_exp},
      {"expm1", OneArg, llvm::LibFunc::LibFunc_expm1},
      {"fabs", OneArg, llvm::LibFunc::LibFunc_fabs},
      {"floor", OneArg, llvm::LibFunc::LibFunc_floor},
      {"fmod", TwoArgs, llvm::LibFunc::LibFunc_fmod},
      {"log", OneArg, llvm::LibFunc::LibFunc_log},
      {"log1p", OneArg, llvm::LibFunc::LibFunc_log1p},
      {"log10", OneArg, llvm::LibFunc::LibFunc_log10},
      {"log2", OneArg, llvm::LibFunc::LibFunc_log2},
      {"sin", OneArg, llvm::LibFunc::LibFunc_sin},
      {"sqrt", OneArg, llvm::LibFunc::LibFunc_sqrt},
      {"tan", OneArg, llvm::LibFunc::LibFunc_tan},
      {"trunc", OneArg, llvm::LibFunc::LibFunc_trunc},
      {"ldexp", TwoMixed, llvm::LibFunc::LibFunc_trunc},
  };

  const unsigned nfuncs = sizeof(funcs) / sizeof(mathfuncdesc);
  for (unsigned idx = 0; idx < nfuncs; ++idx) {
    const mathfuncdesc &d = funcs[idx];
    char bbuf[128];
    char lbuf[128];

    sprintf(bbuf, "__builtin_%s", d.name);
    BuiltinEntryTypeVec *sig = signatures[d.nargs];
    defineLibcallBuiltin(bbuf, d.name, *sig, d.lf);
    if (addLongDouble_) {
      sprintf(lbuf, "%sl", d.name);
      sprintf(bbuf, "__builtin_%s", lbuf);
      BuiltinEntryTypeVec *lsig = lsignatures[d.nargs];
      defineLibcallBuiltin(bbuf, lbuf, *lsig, d.lf);
    }
  }
}

void BuiltinTable::defineSyncFetchAndAddBuiltins() {
  std::vector<unsigned> sizes = {1, 2, 4, 8};
  for (auto sz : sizes) {
    char nbuf[64];
    sprintf(nbuf, "__sync_fetch_and_add_%u", sz);
    Btype *it = tman_->integerType(true,  sz << 3);
    Btype *pit = tman_->pointerType(it);
    defineLibcallBuiltin(nullptr, nbuf,  // libname, name
                         BuiltinEntry::NotInTargetLib, // Libfunc ID
                         tman_->voidType(),  // result type
                         pit, it,        // param types
                         nullptr);
  }
}

void BuiltinTable::defineLibcallBuiltin(const char *libname,
                                        const char *name,
                                        unsigned libfunc, ...)
{
  va_list ap;
  BuiltinEntryTypeVec types(0);
  va_start(ap, libfunc);
  Btype *resultType = va_arg(ap, Btype *);
  types.push_back(resultType);
  Btype *parmType = va_arg(ap, Btype *);
  while (parmType) {
    types.push_back(parmType);
    parmType = va_arg(ap, Btype *);
  }
  llvm::LibFunc lf = static_cast<llvm::LibFunc>(libfunc);
  registerLibCallBuiltin(libname, name, lf, types);
}

void BuiltinTable::defineLibcallBuiltin(const char *libname,
                                        const char *name,
                                        BuiltinEntryTypeVec &types,
                                        unsigned libfunc)
{
  llvm::LibFunc lf = static_cast<llvm::LibFunc>(libfunc);
  registerLibCallBuiltin(libname, name, lf, types);
}

void BuiltinTable::defineIntrinsicBuiltin(const char *name,
                                          const char *libname,
                                          unsigned intrinsicID, ...) {
  va_list ap;
  BuiltinEntryTypeVec overloadTypes;
  va_start(ap, intrinsicID);
  Btype *oType = va_arg(ap, Btype *);
  while (oType) {
    overloadTypes.push_back(oType);
    oType = va_arg(ap, Btype *);
  }
  llvm::Intrinsic::ID iid = static_cast<llvm::Intrinsic::ID>(intrinsicID);
  registerIntrinsicBuiltin(name, libname, iid, overloadTypes);
}

static llvm::Value *builtinExtractReturnAddrMaker(llvm::SmallVectorImpl<llvm::Value*> &args,
                                                  BlockLIRBuilder *builder,
                                                  Llvm_backend *be)
{
  // __builtin_extract_return_addr(uintptr) uintptr
  // extracts the actual encoded address from the address as returned
  // by __builtin_return_address, for example, used on 31-bit S390 to
  // mask out the top bit.
  // On most architectures this is simply identity function.
  // TODO: this is identity function for now. When we get to the
  // architectures that this matters, do the real thing.
  assert(args.size() == 1);
  return args[0];
}

static llvm::Value *builtinUnreachableMaker(llvm::SmallVectorImpl<llvm::Value*> &args,
                                            BlockLIRBuilder *builder,
                                            Llvm_backend *be)
{
  llvm::UnreachableInst *unr = builder->CreateUnreachable();
  return unr;
}

static llvm::Value *builtinMemsetMaker(llvm::SmallVectorImpl<llvm::Value*> &args,
                                       BlockLIRBuilder *builder,
                                       Llvm_backend *be)
{
  // __builtin_memset takes int32 as its second argument, whereas
  // LLVM intrinsic memset takes an i8. We wrap the intrinsic in
  // an expression builtin, which does a cast first.
  assert(args.size() == 3);
  llvm::Value *cast = builder->CreateTrunc(args[1], be->llvmInt8Type());
  llvm::Function* fn =
      llvm::Intrinsic::getDeclaration(&be->module(),
                                      llvm::Intrinsic::memset,
                                      {args[0]->getType(), args[2]->getType()});
  // LLVM memset takes an extra isVolatile argument.
  return builder->CreateCall(fn, {args[0], cast, args[2], builder->getFalse()});
}

static llvm::AtomicOrdering llvmOrder(int o)
{
  switch (o) {
  case __ATOMIC_RELAXED:
    return llvm::AtomicOrdering::Monotonic;
  case __ATOMIC_CONSUME:
    return llvm::AtomicOrdering::Acquire; // LLVM does not define consume
  case __ATOMIC_ACQUIRE:
    return llvm::AtomicOrdering::Acquire;
  case __ATOMIC_RELEASE:
    return llvm::AtomicOrdering::Release;
  case __ATOMIC_ACQ_REL:
    return llvm::AtomicOrdering::AcquireRelease;
  case __ATOMIC_SEQ_CST:
    return llvm::AtomicOrdering::SequentiallyConsistent;
  }
  llvm_unreachable("unknown atomic order");
}

static llvm::Value *atomicLoadMaker(llvm::SmallVectorImpl<llvm::Value*> &args,
                                    BlockLIRBuilder *builder,
                                    Llvm_backend *be, int sz)
{
  assert(args.size() == 2);
  llvm::Type *t = sz == 8 ? be->llvmInt64Type() : be->llvmInt32Type();
  llvm::LoadInst *load = builder->CreateLoad(t, args[0]);
  // FIXME: we assume the FE always emits constant memory order.
  // in case it doesn't, conservatively use SequentiallyConsistent.
  llvm::AtomicOrdering o =
      llvm::isa<llvm::ConstantInt>(args[1]) ?
      llvmOrder(llvm::cast<llvm::ConstantInt>(args[1])->getZExtValue()) :
      llvm::AtomicOrdering::SequentiallyConsistent;
  load->setAtomic(o);
  load->setAlignment(llvm::MaybeAlign(sz));
  return load;
}

static llvm::Value *atomicLoad4Maker(llvm::SmallVectorImpl<llvm::Value*> &args,
                                     BlockLIRBuilder *builder,
                                     Llvm_backend *be)
{
  return atomicLoadMaker(args, builder, be, 4);
}

static llvm::Value *atomicLoad8Maker(llvm::SmallVectorImpl<llvm::Value*> &args,
                                     BlockLIRBuilder *builder,
                                     Llvm_backend *be)
{
  return atomicLoadMaker(args, builder, be, 8);
}

static llvm::Value *atomicStoreMaker(llvm::SmallVectorImpl<llvm::Value*> &args,
                                     BlockLIRBuilder *builder,
                                     Llvm_backend *be, int sz)
{
  assert(args.size() == 3);
  llvm::StoreInst *store = builder->CreateStore(args[1], args[0]);
  // FIXME: see atomicLoadMaker.
  llvm::AtomicOrdering o =
      llvm::isa<llvm::ConstantInt>(args[2]) ?
      llvmOrder(llvm::cast<llvm::ConstantInt>(args[2])->getZExtValue()) :
      llvm::AtomicOrdering::SequentiallyConsistent;
  store->setAtomic(o);
  store->setAlignment(llvm::MaybeAlign(sz));
  return store;
}

static llvm::Value *atomicStore4Maker(llvm::SmallVectorImpl<llvm::Value*> &args,
                                      BlockLIRBuilder *builder,
                                      Llvm_backend *be)
{
  return atomicStoreMaker(args, builder, be, 4);
}

static llvm::Value *atomicStore8Maker(llvm::SmallVectorImpl<llvm::Value*> &args,
                                      BlockLIRBuilder *builder,
                                      Llvm_backend *be)
{
  return atomicStoreMaker(args, builder, be, 8);
}

static llvm::Value *atomicRMWMaker(llvm::AtomicRMWInst::BinOp op,
                                   llvm::SmallVectorImpl<llvm::Value*> &args,
                                   BlockLIRBuilder *builder,
                                   Llvm_backend *be)
{
  assert(args.size() == 3);
  // FIXME: see atomicLoadMaker.
  llvm::AtomicOrdering o =
      llvm::isa<llvm::ConstantInt>(args[2]) ?
      llvmOrder(llvm::cast<llvm::ConstantInt>(args[2])->getZExtValue()) :
      llvm::AtomicOrdering::SequentiallyConsistent;
  return builder->CreateAtomicRMW(op, args[0], args[1], o);
}

static llvm::Value *atomicXchgMaker(llvm::SmallVectorImpl<llvm::Value*> &args,
                                    BlockLIRBuilder *builder,
                                    Llvm_backend *be)
{
  return atomicRMWMaker(llvm::AtomicRMWInst::Xchg, args, builder, be);
}

static llvm::Value *atomicAddMaker(llvm::SmallVectorImpl<llvm::Value*> &args,
                                   BlockLIRBuilder *builder,
                                   Llvm_backend *be)
{
  // atomicrmw returns the old content. We need to do the add.
  llvm::Value* old = atomicRMWMaker(llvm::AtomicRMWInst::Add, args, builder, be);
  return builder->CreateAdd(old, args[1]);
}

static llvm::Value *atomicAndMaker(llvm::SmallVectorImpl<llvm::Value*> &args,
                                   BlockLIRBuilder *builder,
                                   Llvm_backend *be)
{
  // atomicrmw returns the old content. We need to do the and.
  llvm::Value* old = atomicRMWMaker(llvm::AtomicRMWInst::And, args, builder, be);
  return builder->CreateAnd(old, args[1]);
}

static llvm::Value *atomicOrMaker(llvm::SmallVectorImpl<llvm::Value*> &args,
                                  BlockLIRBuilder *builder,
                                  Llvm_backend *be)
{
  // atomicrmw returns the old content. We need to do the or.
  llvm::Value* old = atomicRMWMaker(llvm::AtomicRMWInst::Or, args, builder, be);
  return builder->CreateOr(old, args[1]);
}

static llvm::Value *atomicCasMaker(llvm::SmallVectorImpl<llvm::Value*> &args,
                                   BlockLIRBuilder *builder,
                                   Llvm_backend *be)
{
  assert(args.size() == 6);
  // GCC __atomic_compare_exchange_n takes a pointer to the old value.
  // We need to load it.
  llvm::Value *old = builder->CreateLoad(args[1]);
  // FIXME: see atomicLoadMaker, but default to SequentiallyConsistent
  // for success order, Monotonic (i.e. relaxed) for failed order,
  // and false for weak.
  llvm::AtomicOrdering o =
      llvm::isa<llvm::ConstantInt>(args[4]) ?
      llvmOrder(llvm::cast<llvm::ConstantInt>(args[4])->getZExtValue()) :
      llvm::AtomicOrdering::SequentiallyConsistent;
  llvm::AtomicOrdering o2 =
      llvm::isa<llvm::ConstantInt>(args[5]) ?
      llvmOrder(llvm::cast<llvm::ConstantInt>(args[5])->getZExtValue()) :
      llvm::AtomicOrdering::Monotonic;
  bool weak =
      llvm::isa<llvm::ConstantInt>(args[3]) &&
      !llvm::cast<llvm::ConstantInt>(args[3])->isZero();
  llvm::AtomicCmpXchgInst *cas =
      builder->CreateAtomicCmpXchg(args[0], old, args[2], o, o2);
  cas->setWeak(weak);
  // LLVM cmpxchg instruction returns { valType, i1 }. Extract the second
  // value, and cast to Go bool type (i8).
  llvm::Value *ret = builder->CreateExtractValue(cas, {1});
  return builder->CreateZExt(ret, be->llvmInt8Type());
}

void BuiltinTable::defineExprBuiltins()
{
  Btype *boolType = tman_->boolType();
  Btype *int32Type = tman_->integerType(false, 32);
  Btype *uint8Type = tman_->integerType(true, 8);
  Btype *uint8PtrType = tman_->pointerType(uint8Type);
  Btype *uint32Type = tman_->integerType(true, 32);
  Btype *uint32PtrType = tman_->pointerType(uint32Type);
  Btype *uint64Type = tman_->integerType(true, 64);
  Btype *uint64PtrType = tman_->pointerType(uint64Type);
  unsigned bitsInPtr = tman_->datalayout()->getPointerSizeInBits();
  Btype *uintPtrType = tman_->integerType(true, bitsInPtr);

  {
    BuiltinEntryTypeVec typeVec = {uintPtrType, uintPtrType};
    registerExprBuiltin("__builtin_extract_return_addr", nullptr,
                        typeVec, builtinExtractReturnAddrMaker);
  }

  {
    BuiltinEntryTypeVec typeVec;
    registerExprBuiltin("__builtin_unreachable", nullptr,
                        typeVec, builtinUnreachableMaker);
  }

  {
    BuiltinEntryTypeVec typeVec = {nullptr, uint8Type, int32Type, uintPtrType};
    registerExprBuiltin("__builtin_memset", nullptr,
                        typeVec, builtinMemsetMaker);
  }

  {
    BuiltinEntryTypeVec typeVec = {uint32Type, uint32PtrType, int32Type};
    registerExprBuiltin("__atomic_load_4", nullptr,
                        typeVec, atomicLoad4Maker);
  }
  {
    BuiltinEntryTypeVec typeVec = {uint64Type, uint64PtrType, int32Type};
    registerExprBuiltin("__atomic_load_8", nullptr,
                        typeVec, atomicLoad8Maker);
  }

  {
    BuiltinEntryTypeVec typeVec = {nullptr, uint32PtrType, uint32Type, int32Type};
    registerExprBuiltin("__atomic_store_4", nullptr,
                        typeVec, atomicStore4Maker);
  }
  {
    BuiltinEntryTypeVec typeVec = {nullptr, uint64PtrType, uint64Type, int32Type};
    registerExprBuiltin("__atomic_store_8", nullptr,
                        typeVec, atomicStore8Maker);
  }

  {
    BuiltinEntryTypeVec typeVec = {uint32Type, uint32PtrType, uint32Type, int32Type};
    registerExprBuiltin("__atomic_exchange_4", nullptr,
                        typeVec, atomicXchgMaker);
    registerExprBuiltin("__atomic_add_fetch_4", nullptr,
                        typeVec, atomicAddMaker);
  }
  {
    BuiltinEntryTypeVec typeVec = {uint64Type, uint64PtrType, uint64Type, int32Type};
    registerExprBuiltin("__atomic_exchange_8", nullptr,
                        typeVec, atomicXchgMaker);
    registerExprBuiltin("__atomic_add_fetch_8", nullptr,
                        typeVec, atomicAddMaker);
  }

  {
    BuiltinEntryTypeVec typeVec = {uint8Type, uint8PtrType, uint8Type, int32Type};
    registerExprBuiltin("__atomic_and_fetch_1", nullptr,
                        typeVec, atomicAndMaker);
    registerExprBuiltin("__atomic_or_fetch_1", nullptr,
                        typeVec, atomicOrMaker);
  }

  {
    BuiltinEntryTypeVec typeVec = {boolType, uint32PtrType, uint32PtrType, uint32Type,
                                   boolType, int32Type, int32Type};
    registerExprBuiltin("__atomic_compare_exchange_4", nullptr,
                        typeVec, atomicCasMaker);
  }
  {
    BuiltinEntryTypeVec typeVec = {boolType, uint64PtrType, uint64PtrType, uint64Type,
                                   boolType, int32Type, int32Type};
    registerExprBuiltin("__atomic_compare_exchange_8", nullptr,
                        typeVec, atomicCasMaker);
  }
}
