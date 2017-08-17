//===-- go-llvm.cpp - LLVM implementation of 'Backend'  -------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// Methods for class Llvm_backend, a subclass of the gofrontend class
// Backend, with LLVM-specific implementation methods.
//
//===----------------------------------------------------------------------===//

#include "go-llvm.h"
#include "go-llvm-builtins.h"
#include "backend.h"
#include "go-c.h"
#include "go-system.h"
#include "go-llvm-linemap.h"
#include "go-llvm-dibuildhelper.h"
#include "go-llvm-cabi-oracle.h"
#include "go-llvm-irbuilders.h"
#include "gogo.h"

#include "llvm/Analysis/TargetLibraryInfo.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/CFG.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/DebugInfo.h"
#include "llvm/IR/DebugInfoMetadata.h"
#include "llvm/IR/DIBuilder.h"
#include "llvm/IR/GlobalValue.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Value.h"
#include "llvm/IR/Verifier.h"

Llvm_backend::Llvm_backend(llvm::LLVMContext &context,
                           llvm::Module *module,
                           Llvm_linemap *linemap)
    : TypeManager(context, llvm::CallingConv::X86_64_SysV)
    , context_(context)
    , module_(module)
    , datalayout_(module ? &module->getDataLayout() : nullptr)
    , nbuilder_(this)
    , linemap_(linemap)
    , addressSpace_(0)
    , traceLevel_(0)
    , checkIntegrity_(true)
    , createDebugMetaData_(true)
    , exportDataStarted_(false)
    , exportDataFinalized_(false)
    , errorCount_(0u)
    , compositeSizeThreshold_(8u) // TODO: adjust later to larger value
    , TLI_(nullptr)
    , builtinTable_(new BuiltinTable(typeManager(), false))
    , errorFunction_(nullptr)
    , personalityFunction_(nullptr)
{
  // If nobody passed in a linemap, create one for internal use (unit testing)
  if (!linemap_) {
    ownLinemap_.reset(new Llvm_linemap());
    linemap_ = ownLinemap_.get();
  }

  // Similarly for the LLVM module (unit testing)
  if (!module_) {
    ownModule_.reset(new llvm::Module("gomodule", context));
    ownModule_->setTargetTriple("x86_64-unknown-linux-gnu");
    ownModule_->setDataLayout("e-m:e-i64:64-f80:128-n8:16:32:64-S128");
    module_ = ownModule_.get();
  }

  datalayout_ = &module_->getDataLayout();

  // Reuse the error function as the value for error_expression
  errorExpression_ = nbuilder_.mkError(errorType());

  // We now have the necessary bits to finish initialization of the
  // type manager.
  initializeTypeManager(errorExpression(),
                        datalayout_,
                        nameTags());

  // Create and record an error function. By marking it as varargs this will
  // avoid any collisions with things that the front end might create, since
  // Go varargs is handled/lowered entirely by the front end.
  llvm::SmallVector<llvm::Type *, 1> elems(0);
  elems.push_back(llvmPtrType());
  const bool isVarargs = true;
  llvm::FunctionType *eft = llvm::FunctionType::get(
      llvm::Type::getVoidTy(context_), elems, isVarargs);
  llvm::GlobalValue::LinkageTypes plinkage = llvm::GlobalValue::ExternalLinkage;
  llvm::Function *ef = llvm::Function::Create(eft, plinkage, "", module_);
  errorFunction_.reset(new Bfunction(ef, makeAuxFcnType(eft), "", "",
                                     Location(), typeManager()));

  // Error statement.
  errorStatement_ = nbuilder_.mkErrorStmt();

  // Error variable.
  Location loc;
  errorVariable_.reset(
      new Bvariable(errorType(), loc, "", ErrorVar, false, nullptr));

  // Initialize machinery for builtins
  builtinTable_->defineAllBuiltins();

  if (createDebugMetaData_)
    dibuildhelper_.reset(new DIBuildHelper(module_, typeManager(), linemap_));
}

Llvm_backend::~Llvm_backend() {
  for (auto &kv : valueVarMap_)
    delete kv.second;
  for (auto &bfcn : functions_)
    delete bfcn;
}

TypeManager *Llvm_backend::typeManager() const {
  const TypeManager *tm = this;
  return const_cast<TypeManager*>(tm);
}

void Llvm_backend::setTraceLevel(unsigned level)
{
  traceLevel_ = level;
  setTypeManagerTraceLevel(level);
}

void
Llvm_backend::verifyModule()
{
  bool broken = llvm::verifyModule(module(), &llvm::dbgs());
  assert(!broken && "Module not well-formed.");
}

void
Llvm_backend::dumpModule()
{
  module().dump();
}

void Llvm_backend::dumpExpr(Bexpression *e)
{
  if (e) {
    e->srcDump(linemap_);
    TreeIntegCtl ctl(DumpPointers, ReportRepairableSharing, BatchMode);
    auto p = checkTreeIntegrity(e, ctl);
    if (p.first)
      std::cerr << p.second;
  }
}

void Llvm_backend::dumpStmt(Bstatement *s)
{
  if (s) {
    s->srcDump(linemap_);
    TreeIntegCtl ctl(DumpPointers, ReportRepairableSharing, BatchMode);
    auto p = checkTreeIntegrity(s, ctl);
    if (p.first)
      std::cerr << p.second;
  }
}

void Llvm_backend::dumpVar(Bvariable *v)
{
  if (v)
    v->srcDump(linemap_);
}

std::pair<bool, std::string>
Llvm_backend::checkTreeIntegrity(Bnode *n, TreeIntegCtl control)
{
  Llvm_backend *be = const_cast<Llvm_backend *>(this);
  IntegrityVisitor iv(be, control);
  bool rval = iv.examine(n);
  return std::make_pair(rval, iv.msg());
}

void Llvm_backend::disableIntegrityChecks()
{
  checkIntegrity_ = false;
  nodeBuilder().setIntegrityChecks(false);
}

void Llvm_backend::enforceTreeIntegrity(Bnode *n)
{
  Llvm_backend *be = const_cast<Llvm_backend *>(this);
  TreeIntegCtl control(DumpPointers, DontReportRepairableSharing, BatchMode);
  IntegrityVisitor iv(be, control);
  bool res = iv.examine(n);
  if (!res && checkIntegrity_) {
    std::cerr << iv.msg() << "\n";
    assert(false);
  }
}

Btype *Llvm_backend::error_type() {
  errorCount_++;
  return errorType();
}

Btype *Llvm_backend::void_type() { return voidType(); }

Btype *Llvm_backend::bool_type() { return boolType(); }

// Get an unnamed float type.

Btype *Llvm_backend::float_type(int bits)
{
  return floatType(bits);
}

Btype *Llvm_backend::integer_type(bool is_unsigned, int bits)
{
  return integerType(is_unsigned, bits);
}

Btype *Llvm_backend::struct_type(const std::vector<Btyped_identifier> &fields)
{
  return structType(fields);
}

// Create a placeholder for a struct type.
Btype *Llvm_backend::placeholder_struct_type(const std::string &name,
                                             Location location)
{
  return placeholderStructType(name, location);
}

Btype *Llvm_backend::complex_type(int bits) {
  return complexType(bits);
}

// Get a pointer type.

Btype *Llvm_backend::pointer_type(Btype *toType)
{
  return pointerType(toType);
}

Btype *Llvm_backend::placeholder_pointer_type(const std::string &name,
                                              Location location, bool forfunc)
{
  return placeholderPointerType(name, location, forfunc);
}

// Make a function type.

Btype *
Llvm_backend::function_type(const Btyped_identifier &receiver,
                            const std::vector<Btyped_identifier> &parameters,
                            const std::vector<Btyped_identifier> &results,
                            Btype *result_struct, Location loc) {
  bool followsCabi = true;
  return functionType(receiver, parameters, results,
                      result_struct, followsCabi, loc);
}

Btype *Llvm_backend::array_type(Btype *elemType, Bexpression *length)
{
  return arrayType(elemType, length);
}

Btype *Llvm_backend::placeholder_array_type(const std::string &name,
                                            Location location)
{
  return placeholderArrayType(name, location);
}

bool Llvm_backend::set_placeholder_pointer_type(Btype *placeholder,
                                                Btype *to_type)
{
  return setPlaceholderPointerType(placeholder, to_type);
}

bool Llvm_backend::set_placeholder_function_type(Btype *placeholder,
                                                 Btype *ft) {
  return setPlaceholderPointerType(placeholder, ft);
}

bool
Llvm_backend::set_placeholder_struct_type(Btype *placeholder,
                            const std::vector<Btyped_identifier> &fields)
{
  return setPlaceholderStructType(placeholder, fields);
}

// Fill in the components of a placeholder array type.

bool Llvm_backend::set_placeholder_array_type(Btype *placeholder,
                                              Btype *element_btype,
                                              Bexpression *length) {
  return setPlaceholderArrayType(placeholder, element_btype, length);
}

Btype *Llvm_backend::named_type(const std::string &name,
                                Btype *btype,
                                Location location)
{
  return namedType(name, btype, location);
}

Btype *Llvm_backend::circular_pointer_type(Btype *placeholder, bool isf) {
  return circularPointerType(placeholder, isf);
}

bool Llvm_backend::is_circular_pointer_type(Btype *btype) {
  return isCircularPointerType(btype) || isCircularFunctionType(btype);
}

int64_t Llvm_backend::type_size(Btype *btype) {
  return typeSize(btype);
}

int64_t Llvm_backend::type_alignment(Btype *btype) {
  return typeAlignment(btype);
}

int64_t Llvm_backend::type_field_alignment(Btype *btype)
{
  return typeFieldAlignment(btype);
}

int64_t Llvm_backend::type_field_offset(Btype *btype, size_t index)
{
  return typeFieldOffset(btype, index);
}

llvm::Function *Llvm_backend::personalityFunction()
{
  if (personalityFunction_)
    return personalityFunction_;

  llvm::FunctionType *pft = personalityFunctionType();
  llvm::GlobalValue::LinkageTypes plinkage = llvm::GlobalValue::ExternalLinkage;
  const char *pfn = "__gccgo_personality_v0";
  personalityFunction_ =
      llvm::Function::Create(pft, plinkage, pfn, module_);
  return personalityFunction_;
}

Bfunction *Llvm_backend::defineBuiltinFcn(const std::string &name,
                                          llvm::Function *fcn)
{
  llvm::PointerType *llpft =
      llvm::cast<llvm::PointerType>(fcn->getType());
  llvm::FunctionType *llft =
      llvm::cast<llvm::FunctionType>(llpft->getElementType());
  BFunctionType *fcnType = makeAuxFcnType(llft);
  Location pdcl = linemap()->get_predeclared_location();
  Bfunction *bfunc = new Bfunction(fcn, fcnType, name, name, pdcl,
                                   typeManager());
  return bfunc;
}

// Look up a named built-in function in the current backend implementation.
// Returns NULL if no built-in function by that name exists.

Bfunction *Llvm_backend::lookup_builtin(const std::string &name) {

  // Do we have an entry at all for this builtin?
  BuiltinEntry *be = builtinTable_->lookup(name);
  if (!be)
    return nullptr;

  // We have an entry -- have we materialized a Bfunction for it yet?
  Bfunction *bf = be->bfunction();
  if (bf != nullptr)
    return bf;

  // Materialize a Bfunction for the builtin
  if (be->flavor() == BuiltinEntry::IntrinsicBuiltin) {
    llvm::Function *fcn;
    llvm::Intrinsic::ID id = be->intrinsicId();
    if (!llvm::Intrinsic::isOverloaded(id))
      fcn = llvm::Intrinsic::getDeclaration(module_, id);
    else {
      llvm::SmallVector<llvm::Type *, 8> ltypes;
      for (auto &t : be->types())
        ltypes.push_back(t->type());
      fcn = llvm::Intrinsic::getDeclaration(module_, id, ltypes);
    }
    assert(fcn != nullptr);
    assert(fcn->isIntrinsic() && fcn->getIntrinsicID() == id);
    bf = defineBuiltinFcn(be->name(), fcn);
  } else {
    assert(be->flavor() == BuiltinEntry::LibcallBuiltin);

    // Create function type.
    Btyped_identifier receiver;
    std::vector<Btyped_identifier> params;
    std::vector<Btyped_identifier> results;
    Location bloc(linemap_->get_predeclared_location());
    const BuiltinEntryTypeVec &types = be->types();
    Btyped_identifier result("ret", types[0], bloc);
    results.push_back(result);
    for (unsigned idx = 1; idx < types.size(); ++idx)
      params.push_back(Btyped_identifier("", types[idx], bloc));
    bool followsCabi = false;
    Btype *fcnType = functionType(receiver, params, results, nullptr,
                                  followsCabi, bloc);

    // Create function
    bf = function(fcnType, be->name(), be->name(),
                  true, false, false, false, false, bloc);

    // FIXME: add attributes this function? Such as maybe
    // llvm::Attribute::ArgMemOnly, llvm::Attribute::ReadOnly?

    // FIXME: once we have a pass manager set up for the back end, we'll
    // want to turn on this code, since it will be helpful to catch
    // errors/mistakes. For the time being it can't be turned on (not
    // pass manager is set up).

    llvm::LibFunc lf = be->libfunc();
    if (TLI_ && lf != llvm::LibFunc::NumLibFuncs) {

      // Verify that the function is available on this target.
      assert(TLI_->has(lf));

      // Verify that the name and type we've computer so far matches up
      // with how LLVM views the routine. For example, if we are trying
      // to create a version of memcmp() that takes a single boolean as
      // an argument, that's going to be a show-stopper type problem.
      assert(TLI_->getLibFunc(*bf->function(), lf));
    }
  }
  be->setBfunction(bf);

  return bf;
}

bool Llvm_backend::moduleScopeValue(llvm::Value *val, Btype *btype) const
{
  valbtype vbt(std::make_pair(val, btype));
  return (valueExprmap_.find(vbt) != valueExprmap_.end());
}

Bexpression *Llvm_backend::makeGlobalExpression(Bexpression *expr,
                                                llvm::Value *val,
                                                Btype *btype,
                                                Location location) {
  assert(! llvm::isa<llvm::Instruction>(val));
  valbtype vbt(std::make_pair(val, btype));
  auto it = valueExprmap_.find(vbt);
  if (it != valueExprmap_.end()) {
    nbuilder_.freeNode(expr);
    return it->second;
  }
  valueExprmap_[vbt] = expr;
  return expr;
}

// Return the zero value for a type.

Bexpression *Llvm_backend::zero_expression(Btype *btype) {
  if (btype == errorType())
    return errorExpression();
  llvm::Value *zeroval = llvm::Constant::getNullValue(btype->type());
  Bexpression *cexpr = nbuilder_.mkConst(btype, zeroval);
  return makeGlobalExpression(cexpr, zeroval, btype, Location());
}

Bexpression *Llvm_backend::error_expression()
{
  errorCount_++;
  return errorExpression();
}

Bexpression *Llvm_backend::nil_pointer_expression()
{
  // What type should we assign a NIL pointer expression? This
  // is something of a head-scratcher. For now use uintptr.
  return zero_expression(pointerType(uintPtrType()));
}

Bexpression *Llvm_backend::genCircularConversion(Btype *toType,
                                                 Bexpression *expr,
                                                 Location loc)
{
  llvm::Value *val = expr->value();
  llvm::Type *llToType = toType->type();
  if (expr->varExprPending()) {
    llvm::Type *pet = llvm::PointerType::get(expr->btype()->type(),
                                             addressSpace_);
    if (val->getType() == pet)
      llToType = llvm::PointerType::get(llToType, addressSpace_);
  }
  if (val->getType() == llToType)
    return expr;

  std::string tag(namegen("cast"));
  LIRBuilder builder(context_, llvm::ConstantFolder());
  llvm::Value *bitcast = builder.CreateBitCast(val, llToType, tag);
  return nbuilder_.mkConversion(toType, bitcast, expr, loc);
}

Bexpression *Llvm_backend::genLoad(Bexpression *expr,
                                   Btype *btype,
                                   Location loc,
                                   const std::string &tag)
{
  // If this is a load from a pointer flagged as being a circular
  // type, insert a conversion prior to the load so as to force
  // the value to the correct type. This is weird but necessary,
  // since the LLVM type system can't accurately model Go circular
  // pointer types.
  Bexpression *space = expr;
  Btype *loadResultType;
  if (btype) {
    loadResultType = btype;
    Btype *tctyp = circularTypeLoadConversion(expr->btype());
    if (tctyp != nullptr) {
      space = genCircularConversion(pointer_type(tctyp), expr, loc);
      loadResultType = tctyp;
    }
  } else {
    // Here we are resolving a pending var expression. The LLVM
    // value should already be pointer to the expression type.
    // No need to check circular pointer type.
    loadResultType = expr->btype();
  }

  llvm::PointerType *llpt =
      llvm::cast<llvm::PointerType>(space->value()->getType());
  llvm::Type *llrt = llpt->getElementType();

  // If this type meets our criteria (composite/aggregate whose
  // size is above a certain threshhold) then assume that the
  // consumer will want an address (for memcpy) instead of a
  // value.
  Bexpression *rval = nullptr;
  if (! useCopyForLoadStore(llrt)) {
    // Non-composite value.
    std::string ldname(tag);
    ldname += ".ld";
    ldname = namegen(ldname);
    llvm::Instruction *loadInst = new llvm::LoadInst(space->value(), ldname);
    rval = nbuilder_.mkDeref(loadResultType, loadInst, space, loc);
    rval->appendInstruction(loadInst);
  } else {
    // Composite value. Note that "var expr pending" is being set on
    // the resulting expression to flag the fact that there is a
    // disconnect between the concrete Btype (for example, maybe
    // "{ int64, int64}") and the llvm::Type of the llvm::Value (which in
    // this case will be a pointer to struct, "{ int64, int64 }*").
    rval = nbuilder_.mkDeref(loadResultType, space->value(), space, loc);
    rval->setVarExprPending(false, 0);
  }
  return rval;
}

// An expression that indirectly references an expression.

Bexpression *Llvm_backend::indirect_expression(Btype *btype,
                                               Bexpression *expr,
                                               bool known_valid,
                                               Location location)
{

  if (expr == errorExpression() || btype == errorType())
    return errorExpression();

  assert(expr->btype()->type()->isPointerTy());

  Bexpression *rval = nbuilder_.mkDeref(btype, nullptr, expr, location);
  return rval;
}

// Get the address of an expression.

Bexpression *Llvm_backend::address_expression(Bexpression *bexpr,
                                              Location location)
{
  if (bexpr == errorExpression())
    return errorExpression();

  Btype *pt = pointer_type(bexpr->btype());
  Bexpression *rval = nbuilder_.mkAddress(pt, nullptr, bexpr, location);
  return rval;
}

bool Llvm_backend::exprVectorHasError(const std::vector<Bexpression *> &vals) {
  for (auto v : vals)
    if (v == errorExpression())
      return true;
  return false;
}

bool Llvm_backend::stmtVectorHasError(const std::vector<Bstatement *> &stmts)
{
  for (auto s : stmts)
    if (s == errorStatement())
      return true;
  return false;
}

Bexpression *Llvm_backend::resolve(Bexpression *expr,
                                   Varexpr_context ctx)

{
  if (expr->compositeInitPending())
    expr = resolveCompositeInit(expr, nullptr);
  if (expr->varExprPending())
    expr = resolveVarContext(expr, ctx);
  return expr;
}

Bexpression *Llvm_backend::resolveVarContext(Bexpression *expr,
                                             Varexpr_context ctx)

{
  if (expr->varExprPending()) {
    const VarContext &vc = expr->varContext();
    assert(vc.addrLevel() == 0 || vc.addrLevel() == 1);
    if (vc.addrLevel() == 1 || vc.lvalue() || ctx == VE_lvalue) {
      assert(vc.addrLevel() == 0 || expr->btype()->type()->isPointerTy());
      expr->resetVarExprContext();
      return expr;
    }
    Bexpression *rval = genLoad(expr, nullptr, expr->location(), expr->tag());
    return rval;
  }
  return expr;
}

Bvariable *Llvm_backend::genVarForConstant(llvm::Constant *conval,
                                           Btype *type)
{
  auto it = valueVarMap_.find(conval);
  if (it != valueVarMap_.end())
    return it->second;

  std::string ctag(namegen("const"));
  Bvariable *rv = makeModuleVar(type, ctag, "", Location(),
                                MV_Constant, MV_DefaultSection,
                                MV_NotInComdat, MV_DefaultVisibility,
                                MV_NotExternallyInitialized, MV_SkipDebug,
                                llvm::GlobalValue::PrivateLinkage,
                                conval, 0);
  assert(llvm::isa<llvm::GlobalVariable>(rv->value()));
  return rv;
}

llvm::Value *Llvm_backend::genStore(BlockLIRBuilder *builder,
                                    Btype *srcType,
                                    bool srcConstant,
                                    llvm::Type *dstType,
                                    llvm::Value *srcVal,
                                    llvm::Value *dstLoc)
{
  // Don't try to emit a store if the value in question is void
  // (for example, the return value from a call to a function that
  // returns a struct with no fields). In this case just manufacture
  // an undef and return it.
  if (srcVal->getType()->isVoidTy())
    return llvm::UndefValue::get(srcVal->getType());

  // Decide whether we want a simple store instruction or a memcpy.
  if (! useCopyForLoadStore(srcType)) {

    if (srcVal->getType()->isPointerTy()) {
      llvm::PointerType *dstpt =
          llvm::cast<llvm::PointerType>(dstType);
      srcVal = convertForAssignment(srcType, srcVal,
                                    dstpt->getElementType(), builder);
    }

    // At this point the types should agree
    llvm::PointerType *dpt = llvm::cast<llvm::PointerType>(dstType);
    assert(srcVal->getType() == dpt->getElementType());

    // Create and return store
    return builder->CreateStore(srcVal, dstLoc);
  }

  // destination should be pointer
  assert(dstLoc->getType()->isPointerTy());

  // memcpy src: handle constant input (we need something addressable
  // in order to do a memcpy, not a raw constant value)
  if (srcConstant) {
    llvm::Constant *cval = llvm::cast<llvm::Constant>(srcVal);
    Bvariable *cvar = genVarForConstant(cval, srcType);
    srcVal = cvar->value();
  }
  assert(srcVal->getType()->isPointerTy());

  // number of bytes to copy
  uint64_t sz = typeSize(srcType);

  // alignment of src expr
  unsigned algn = typeAlignment(srcType);

  // Q: should we be using memmove here instead?
  llvm::CallInst *call = builder->CreateMemCpy(dstLoc, srcVal, sz, algn);

  return call;
}

Bexpression *Llvm_backend::genStore(Bfunction *func,
                                    Bexpression *srcExpr,
                                    Bexpression *dstExpr,
                                    Location location)
{
  llvm::Function *dummyFcn = errorFunction_->function();
  BlockLIRBuilder builder(dummyFcn, this);

  Varexpr_context ctx = varContextDisp(srcExpr);

  // Resolve pending var exprs and/or composites
  Bexpression *valexp = resolve(srcExpr, ctx);

  // Call helper to generate instructions
  llvm::Value *val = valexp->value();
  llvm::Value *dst = dstExpr->value();
  llvm::Value *result = genStore(&builder, srcExpr->btype(),
                                 valexp->isConstant(),
                                 dstExpr->value()->getType(),
                                 val, dst);

  // Wrap result in a Bexpression
  Binstructions insns(builder.instructions());
  Bexpression *rval =
      nbuilder_.mkBinaryOp(OPERATOR_EQ, valexp->btype(), result,
                             dstExpr, valexp, insns, location);
  return rval;
}

Bexpression *Llvm_backend::genArrayInit(llvm::ArrayType *llat,
                                        Bexpression *expr,
                                        llvm::Value *storage)
{
  Location loc = expr->location();
  Btype *btype = expr->btype();
  std::vector<Bexpression *> aexprs = nbuilder_.extractChildenAndDestroy(expr);
  expr = nullptr;

  unsigned nElements = llat->getNumElements();
  assert(nElements == aexprs.size());

  llvm::Function *dummyFcn = errorFunction_->function();
  BlockLIRBuilder builder(dummyFcn, this);
  std::vector<Bexpression *> values;

  for (unsigned eidx = 0; eidx < nElements; ++eidx) {

    // Construct an appropriate GEP
    llvm::SmallVector<llvm::Value *, 2> elems(2);
    llvm::Value *idxval = llvm::ConstantInt::get(llvmInt32Type(), eidx);
    elems[0] = llvm::ConstantInt::get(llvmInt32Type(), 0);
    elems[1] = idxval;
    std::string tag(namegen("index"));
    llvm::Value *gep = builder.CreateGEP(llat, storage, elems, tag);

    // Resolve element value if needed
    Varexpr_context ctx = varContextDisp(aexprs[eidx]);
    Bexpression *valexp = resolve(aexprs[eidx], ctx);

    // Store field value into GEP
    genStore(&builder, valexp->btype(), valexp->isConstant(),
             gep->getType(), valexp->value(), gep);

    values.push_back(valexp);
  }

  Binstructions instructions(builder.instructions());
  Bexpression *arexp =
      nbuilder_.mkComposite(btype, storage, values, instructions, loc);
  return arexp;
}

Bexpression *Llvm_backend::genStructInit(llvm::StructType *llst,
                                         Bexpression *expr,
                                         llvm::Value *storage)
{
  Location loc = expr->location();
  Btype *btype = expr->btype();
  std::vector<Bexpression *> fexprs = nbuilder_.extractChildenAndDestroy(expr);
  expr = nullptr;
  unsigned nFields = llst->getNumElements();
  assert(nFields == fexprs.size());

  llvm::Function *dummyFcn = errorFunction_->function();
  BlockLIRBuilder builder(dummyFcn, this);
  std::vector<Bexpression *> values;

  for (unsigned fidx = 0; fidx < nFields; ++fidx) {
    Bexpression *fieldValExpr = fexprs[fidx];
    assert(fieldValExpr);

    Varexpr_context ctx = varContextDisp(fieldValExpr);
    Bexpression *valexp = resolve(fieldValExpr, ctx);

    // Create GEP
    assert(fidx < llst->getNumElements());
    std::string tag(namegen("field"));
    llvm::Value *gep =
        builder.CreateConstInBoundsGEP2_32(llst, storage,
                                           0, fidx, tag);

    // Store field value into GEP
    genStore(&builder, valexp->btype(), valexp->isConstant(),
             gep->getType(), valexp->value(), gep);

    values.push_back(valexp);
  }

  Binstructions instructions(builder.instructions());
  Bexpression *structexp =
      nbuilder_.mkComposite(btype, storage, values, instructions, loc);
  return structexp;
}

Bexpression *Llvm_backend::resolveCompositeInit(Bexpression *expr,
                                                llvm::Value *storage)
{
  assert(expr != errorExpression());
  bool setPending = false;
  Bvariable *tvar = nullptr;
  if (!storage) {
    std::string tname(namegen("tmp"));
    tvar = nbuilder_.mkTempVar(expr->btype(), expr->location(), tname);
    assert(tvar != errorVariable_.get());
    storage = tvar->value();
    setPending = true;
  }

  // Call separate helper depending on array or struct
  llvm::Type *llt = expr->btype()->type();
  assert(llt->isStructTy() || llt->isArrayTy());
  Bexpression *rval = nullptr;
  if (llt->isStructTy()) {
    llvm::StructType *llst = llvm::cast<llvm::StructType>(llt);
    rval = genStructInit(llst, expr, storage);
  } else {
    llvm::ArrayType *llat = llvm::cast<llvm::ArrayType>(llt);
    rval = genArrayInit(llat, expr, storage);
  }
  if (setPending) {
    rval->setVarExprPending(false, 0);
    tvar->setInitializerExpr(rval);
  }
  return rval;
}

Varexpr_context Llvm_backend::varContextDisp(Bexpression *varexp)
{
  if (useCopyForLoadStore(varexp->btype()))
    return VE_lvalue;
  return VE_rvalue;
}

// Create a temporary variable holding the value of EXPR.
// Return the variable and the assignment statement (to be
// attached to some node).

std::pair<Bvariable*, Bstatement*>
Llvm_backend::makeTempVar(Bexpression *expr, Location location) {
  assert(expr);
  std::string tname(namegen("tmp"));
  Bvariable *var = nbuilder_.mkTempVar(expr->btype(), location, tname);
  assert(var != errorVariable_.get());
  Bfunction *dummyFcn = errorFunction_.get();
  Bstatement *init = makeInitStatement(dummyFcn, var, expr);
  assert(init != errorStatement_);
  return std::make_pair(var, init);
}

// An expression that references a variable.

Bexpression *Llvm_backend::var_expression(Bvariable *var,
                                          Varexpr_context in_lvalue_pos,
                                          Location location)
{
  if (var == errorVariable_.get())
    return errorExpression();

  Bexpression *varexp = nbuilder_.mkVar(var, location);
  varexp->setTag(var->name().c_str());
  return varexp;
}

// Return an expression that declares a constant named NAME with the
// constant value VAL in BTYPE.

Bexpression *Llvm_backend::named_constant_expression(Btype *btype,
                                                     const std::string &name,
                                                     Bexpression *val,
                                                     Location location) {
  if (btype == errorType() || val == errorExpression())
    return errorExpression();

  // FIXME: declare named read-only variable with initial value 'val'

  return val;
}

template <typename wideint_t>
wideint_t checked_convert_mpz_to_int(mpz_t value) {
  // See http://gmplib.org/manual/Integer-Import-and-Export.html for an
  // explanation of this formula
  size_t numbits = 8 * sizeof(wideint_t);
  size_t count = (mpz_sizeinbase(value, 2) + numbits - 1) / numbits;
  // frontend should have insured this already
  assert(count <= 2);
  count = 2;
  wideint_t receive[2];
  receive[0] = 0;
  receive[1] = 0;
  mpz_export(&receive[0], &count, -1, sizeof(wideint_t), 0, 0, value);
  // frontend should have insured this already
  assert(receive[1] == 0);
  wideint_t rval = receive[0];
  if (mpz_sgn(value) < 0)
    rval = -rval;
  return rval;
}

// Return a typed value as a constant integer.

Bexpression *Llvm_backend::integer_constant_expression(Btype *btype,
                                                       mpz_t mpz_val) {
  if (btype == errorType())
    return errorExpression();
  assert(btype->type()->isIntegerTy());

  // Force mpz_val into either into uint64_t or int64_t depending on
  // whether btype was declared as signed or unsigned.

  Bexpression *rval;
  BIntegerType *bit = btype->castToBIntegerType();
  if (bit->isUnsigned()) {
    uint64_t val = checked_convert_mpz_to_int<uint64_t>(mpz_val);
    llvm::APInt apiv(bit->bits(), val);
    llvm::Constant *lval = llvm::ConstantInt::get(btype->type(), apiv);
    Bexpression *bconst = nbuilder_.mkConst(btype, lval);
    return makeGlobalExpression(bconst, lval, btype, Location());
  } else {
    int64_t val = checked_convert_mpz_to_int<int64_t>(mpz_val);
    llvm::APInt apiv(bit->bits(), val, true);
    llvm::Constant *lval = llvm::ConstantInt::get(btype->type(), apiv);
    Bexpression *bconst = nbuilder_.mkConst(btype, lval);
    return makeGlobalExpression(bconst, lval, btype, Location());
  }
  return rval;
}

// Return a typed value as a constant floating-point number.

Bexpression *Llvm_backend::float_constant_expression(Btype *btype, mpfr_t val) {
  if (btype == errorType())
    return errorExpression();

  // Force the mpfr value into float, double, or APFloat as appropriate.
  //
  // Note: at the moment there is no way to create an APFloat from a
  // "long double" value, so this code takes the unpleasant step of
  // converting a quad mfr value from text and then back into APFloat
  // from there.

  if (btype->type() == llvmFloatType()) {
    float fval = mpfr_get_flt(val, GMP_RNDN);
    llvm::APFloat apf(fval);
    llvm::Constant *fcon = llvm::ConstantFP::get(context_, apf);
    Bexpression *bconst = nbuilder_.mkConst(btype, fcon);
    return makeGlobalExpression(bconst, fcon, btype, Location());
  } else if (btype->type() == llvmDoubleType()) {
    double dval = mpfr_get_d(val, GMP_RNDN);
    llvm::APFloat apf(dval);
    llvm::Constant *fcon = llvm::ConstantFP::get(context_, apf);
    Bexpression *bconst = nbuilder_.mkConst(btype, fcon);
    return makeGlobalExpression(bconst, fcon, btype, Location());
  } else if (btype->type() == llvmLongDoubleType()) {
    assert("not yet implemented" && false);
    return nullptr;
  } else {
    return errorExpression();
  }
}

// Return a typed real and imaginary value as a constant complex number.

Bexpression *Llvm_backend::complex_constant_expression(Btype *btype,
                                                       mpc_t val) {
  if (btype == errorType())
    return errorExpression();

  BComplexType *bct = btype->castToBComplexType();
  assert(bct);
  llvm::Type *llt = btype->type();
  assert(llt->isStructTy());
  llvm::StructType *llst = llvm::cast<llvm::StructType>(llt);
  assert(llst->getNumElements() == 2);
  llvm::Type *llet = llst->getElementType(0);
  assert(llet == llst->getElementType(1));

  std::vector<Bexpression *> exps;

  if (llet == llvmFloatType()) {
    float frval = mpfr_get_flt(mpc_realref(val), GMP_RNDN);
    float fival = mpfr_get_flt(mpc_imagref(val), GMP_RNDN);
    llvm::APFloat apr(frval);
    llvm::APFloat api(fival);
    llvm::Constant *rcon = llvm::ConstantFP::get(context_, apr);
    llvm::Constant *icon = llvm::ConstantFP::get(context_, api);

    Btype *bet = floatType(32);
    Bexpression *brcon = nbuilder_.mkConst(bet, rcon);
    Bexpression *bicon = nbuilder_.mkConst(bet, icon);
    exps.push_back(brcon);
    exps.push_back(bicon);
  } else if (llet == llvmDoubleType()) {
    double drval = mpfr_get_d(mpc_realref(val), GMP_RNDN);
    double dival = mpfr_get_d(mpc_imagref(val), GMP_RNDN);
    llvm::APFloat apr(drval);
    llvm::APFloat api(dival);
    llvm::Constant *rcon = llvm::ConstantFP::get(context_, apr);
    llvm::Constant *icon = llvm::ConstantFP::get(context_, api);

    Btype *bet = floatType(64);
    Bexpression *brcon = nbuilder_.mkConst(bet, rcon);
    Bexpression *bicon = nbuilder_.mkConst(bet, icon);
    exps.push_back(brcon);
    exps.push_back(bicon);
  } else {
    assert(false && "unknown complex type");
    return nullptr;
  }

  return makeConstCompositeExpr(btype, llst, 2, nullptr, exps, Location());
}

// Make a constant string expression.

Bexpression *Llvm_backend::string_constant_expression(const std::string &val)
{
  if (val.size() == 0) {
    llvm::Value *zer = llvm::Constant::getNullValue(stringType()->type());
    Bexpression *bconst = nbuilder_.mkConst(stringType(), zer);
    return makeGlobalExpression(bconst, zer, stringType(), Location());
  }

  // Create constant
  bool doAddNull = true;
  llvm::Constant *scon =
      llvm::ConstantDataArray::getString(context_,
                                         llvm::StringRef(val),
                                         doAddNull);

  // Return existing const if installed in table already.
  auto it = stringConstantMap_.find(scon);
  if (it != stringConstantMap_.end())
    return it->second;

  // New string. Manufacture a module-scope var to hold the constant,
  // then install various maps.
  Bvariable *svar =
      makeModuleVar(makeAuxType(scon->getType()),
                    "", "", Location(), MV_Constant, MV_DefaultSection,
                    MV_NotInComdat, MV_DefaultVisibility,
                    MV_NotExternallyInitialized, MV_SkipDebug,
                    llvm::GlobalValue::PrivateLinkage, scon, 1);
  llvm::Constant *varval = llvm::cast<llvm::Constant>(svar->value());
  llvm::Constant *bitcast =
      llvm::ConstantExpr::getBitCast(varval, stringType()->type());
  Bexpression *bconst = nbuilder_.mkConst(stringType(), bitcast);
  Bexpression *rval =
      makeGlobalExpression(bconst, bitcast, stringType(), Location());
  stringConstantMap_[scon] = rval;
  return rval;
}

// Make a constant boolean expression.

Bexpression *Llvm_backend::boolean_constant_expression(bool val) {
  LIRBuilder builder(context_, llvm::ConstantFolder());
  llvm::Value *con = (val ? llvm::ConstantInt::getTrue(context_)
                          : llvm::ConstantInt::getFalse(context_));
  llvm::Value *tobool = builder.CreateZExt(con, bool_type()->type(), "");

  Bexpression *bconst = nbuilder_.mkConst(bool_type(), tobool);
  return makeGlobalExpression(bconst, tobool, bool_type(), Location());
}

// Return the real part of a complex expression.

Bexpression *Llvm_backend::real_part_expression(Bexpression *bcomplex,
                                                Location location)
{
  if (bcomplex == errorExpression())
    return errorExpression();

  BComplexType *bct = bcomplex->btype()->castToBComplexType();
  assert(bct);
  Btype *bft = elementTypeByIndex(bct, 0);
  unsigned fIndex = 0;
  Bexpression *rval = nbuilder_.mkStructField(bft, nullptr,
                                              bcomplex, fIndex, location);
  rval->setTag(".real");
  return rval;
}

// Return the imaginary part of a complex expression.

Bexpression *Llvm_backend::imag_part_expression(Bexpression *bcomplex,
                                                Location location) {
  if (bcomplex == errorExpression())
    return errorExpression();

  BComplexType *bct = bcomplex->btype()->castToBComplexType();
  assert(bct);
  Btype *bft = elementTypeByIndex(bct, 0);
  unsigned fIndex = 1;
  Bexpression *rval = nbuilder_.mkStructField(bft, nullptr,
                                              bcomplex, fIndex, location);
  rval->setTag(".imag");
  return rval;
}

// Make a complex expression given its real and imaginary parts.

Bexpression *Llvm_backend::complex_expression(Bexpression *breal,
                                              Bexpression *bimag,
                                              Location location) {
  if (breal == errorExpression() || bimag == errorExpression())
    return errorExpression();

  BFloatType *brft = breal->btype()->castToBFloatType();
  BFloatType *bift = bimag->btype()->castToBFloatType();
  assert(brft && bift);
  assert(brft->bits() == bift->bits());
  Btype *bct = complexType(brft->bits()*2);
  std::vector<Bexpression *> vals = { breal, bimag };
  Binstructions noInstructions;
  Bexpression *rval = nbuilder_.mkComposite(bct, nullptr, vals,
                                            noInstructions, location);
  return rval;
}

Bexpression *Llvm_backend::makeComplexConvertExpr(Btype *type,
                                                  Bexpression *expr,
                                                  Location location) {
  if (expr->btype()->type() == type->type())
    return expr;

  BComplexType *bct = type->castToBComplexType();
  assert(bct);
  assert(expr->btype()->castToBComplexType());
  Btype *bft = floatType(bct->bits()/2);

  // We need to avoid sharing between real part and imag part of the operand.
  // Create temp variables and assign operand to the temp variable first.
  // TODO: maybe not make temp var if the operand is already a var or constant?
  auto p = makeTempVar(expr, location);
  Bvariable *var = p.first;
  Bstatement *einit = p.second;

  Bexpression *vex = nbuilder_.mkVar(var, location);
  vex->setVarExprPending(false, 0);
  Bexpression *vexr = real_part_expression(vex, location);
  Bexpression *vexi = imag_part_expression(vex, location);

  // Make the conversion
  Bexpression *valr = convert_expression(bft, vexr, location);
  Bexpression *vali = convert_expression(bft, vexi, location);
  Bexpression *val = complex_expression(valr, vali, location);

  // Wrap result and the init statements in a compound expression.
  // Currently we can't resolve composite storage for compound
  // expression, so we resolve the inner complex expression
  // here with another temp variable.
  auto p2 = makeTempVar(val, location);
  Bvariable *rvar = p2.first;
  Bstatement *rinit = p2.second;
  Bexpression *rvex = nbuilder_.mkVar(rvar, location);
  Bstatement *init = statement_list(std::vector<Bstatement*>{einit, rinit});
  return nbuilder_.mkCompound(init, rvex, nullptr, location);
}

// An expression that converts an expression to a different type.

Bexpression *Llvm_backend::convert_expression(Btype *type,
                                              Bexpression *expr,
                                              Location location)
{
  if (type == errorType() || expr == errorExpression())
    return errorExpression();
  if (expr->btype() == type)
    return expr;

  // When the frontend casts something to function type, what this
  // really means in the LLVM realm is "pointer to function" type.
  llvm::Type *toType = type->type();
  if (toType->isFunctionTy()) {
    type = pointer_type(type);
    toType = type->type();
  }

  // Complex -> complex conversion
  if (type->castToBComplexType())
    return makeComplexConvertExpr(type, expr, location);

  Bexpression *rval = nbuilder_.mkConversion(type, nullptr, expr, location);
  return rval;
}

// Get the address of a function.

Bexpression *Llvm_backend::function_code_expression(Bfunction *bfunc,
                                                    Location location) {
  if (bfunc == errorFunction_.get())
    return errorExpression();

  // Look up pointer-to-function type
  Btype *fpBtype = pointer_type(bfunc->fcnType());

  // Create an address-of-function expr
  Bexpression *fexpr = nbuilder_.mkFcnAddress(fpBtype, bfunc->fcnValue(),
                                              bfunc, location);
  return makeGlobalExpression(fexpr, bfunc->fcnValue(), fpBtype, location);
}

// Return an expression for the field at INDEX in BSTRUCT.

Bexpression *Llvm_backend::struct_field_expression(Bexpression *bstruct,
                                                   size_t index,
                                                   Location location)
{
  if (bstruct == errorExpression())
    return errorExpression();

  Btype *bft = elementTypeByIndex(bstruct->btype(), index);
  Bexpression *rval = nbuilder_.mkStructField(bft, nullptr,
                                              bstruct, index, location);
  return rval;
}

// Return an expression that executes BSTAT before BEXPR.

Bexpression *Llvm_backend::compound_expression(Bstatement *bstat,
                                               Bexpression *bexpr,
                                               Location location)
{
  if (bstat == errorStatement() || bexpr == errorExpression())
    return errorExpression();

  Bexpression *rval = nbuilder_.mkCompound(bstat, bexpr, nullptr, location);
  return rval;
}

// Return an expression that executes THEN_EXPR if CONDITION is true, or
// ELSE_EXPR otherwise.

Bexpression *Llvm_backend::conditional_expression(Bfunction *function,
                                                  Btype *btype,
                                                  Bexpression *condition,
                                                  Bexpression *then_expr,
                                                  Bexpression *else_expr,
                                                  Location location)
{
  if (function == errorFunction_.get() ||
      btype == errorType() ||
      condition == errorExpression() ||
      then_expr == errorExpression() ||
      else_expr == errorExpression())
    return errorExpression();

  assert(condition && then_expr);

  Bexpression *rval =
      nbuilder_.mkConditional(function, btype, condition,
                              then_expr, else_expr, location);
  return rval;
}

// Return an expression for the unary operation OP EXPR.

Bexpression *Llvm_backend::unary_expression(Operator op,
                                            Bexpression *expr,
                                            Location location)
{
  if (expr == errorExpression())
    return errorExpression();

  Btype *bt = expr->btype();

  if (op == OPERATOR_MINUS) {
    // Special handling for unary minus applied to fp type.
    BFloatType *ft = bt->castToBFloatType();
    Bexpression *zerexp = (ft ? minusZeroExpr(ft) : zero_expression(bt));
    return binary_expression(OPERATOR_MINUS, zerexp, expr, location);
  }

  Bexpression *rval = nbuilder_.mkUnaryOp(op, bt, nullptr, expr, location);
  return rval;
}

Bexpression *Llvm_backend::minusZeroExpr(BFloatType *typ)
{
  assert(typ);
  llvm::Constant *nzcon = llvm::ConstantFP::getNegativeZero(typ->type());
  Bexpression *bconst = nbuilder_.mkConst(typ, nzcon);
  return makeGlobalExpression(bconst, nzcon, typ, Location());
}

Bexpression *Llvm_backend::makeComplexBinaryExpr(Operator op, Bexpression *left,
                                                 Bexpression *right,
                                                 Location location) {
  // We need to avoid sharing between real part and imag part of the operand.
  // Create temp variables and assign operands to the temp vars first.
  // TODO: maybe not make temp var if the operand is already a var or constant?
  auto p = makeTempVar(left, location), p2 = makeTempVar(right, location);
  Bvariable *lvar = p.first, *rvar = p2.first;
  Bstatement *linit = p.second, *rinit = p2.second;

  Bexpression *lvex = nbuilder_.mkVar(lvar, location);
  lvex->setVarExprPending(false, 0);
  Bexpression *rvex = nbuilder_.mkVar(rvar, location);
  rvex->setVarExprPending(false, 0);
  Bexpression *lr = real_part_expression(lvex, location);
  Bexpression *li = imag_part_expression(lvex, location);
  Bexpression *rr = real_part_expression(rvex, location);
  Bexpression *ri = imag_part_expression(rvex, location);
  Bexpression *val;

  switch (op) {
  case OPERATOR_PLUS:
  case OPERATOR_MINUS: {
    Bexpression *valr = binary_expression(op, lr, rr, location);
    Bexpression *vali = binary_expression(op, li, ri, location);
    val = complex_expression(valr, vali, location);
    break;
  }
  case OPERATOR_MULT: {
    // (a+bi)*(c+di) = (ac-bd) + (ad+bc)i
    Bexpression *ac = binary_expression(OPERATOR_MULT, lr, rr, location);
    Bexpression *bd = binary_expression(OPERATOR_MULT, li, ri, location);
    Bexpression *ad = binary_expression(OPERATOR_MULT, lr, ri, location);
    Bexpression *bc = binary_expression(OPERATOR_MULT, li, rr, location);
    Bexpression *valr = binary_expression(OPERATOR_MINUS, ac, bd, location);
    Bexpression *vali = binary_expression(OPERATOR_PLUS, ad, bc, location);
    val = complex_expression(valr, vali, location);
    break;
  }
  case OPERATOR_EQEQ:
  case OPERATOR_NOTEQ: {
    Bexpression *cmpr = binary_expression(op, lr, rr, location);
    Bexpression *cmpi = binary_expression(op, li, ri, location);
    if (op == OPERATOR_EQEQ)
      val = binary_expression(OPERATOR_ANDAND, cmpr, cmpi, location);
    else
      val = binary_expression(OPERATOR_OROR, cmpr, cmpi, location);
    Bstatement *init = statement_list(std::vector<Bstatement*>{linit, rinit});
    return materialize(compound_expression(init, val, location));
  }
  default:
    std::cerr << "Op " << op << " unhandled\n";
    assert(false);
  }

  // Wrap result and the init statements in a compound expression.
  // Currently we can't resolve composite storage for compound
  // expression, so we resolve the inner complex expression
  // here with another temp variable.
  auto p3 = makeTempVar(val, location);
  Bvariable *vvar = p3.first;
  Bstatement *vinit = p3.second;
  Bexpression *vvex = nbuilder_.mkVar(vvar, location);
  Bstatement *init = statement_list(std::vector<Bstatement*>{linit, rinit, vinit});
  return compound_expression(init, vvex, location);
}

// Return an expression for the binary operation LEFT OP RIGHT.

Bexpression *Llvm_backend::binary_expression(Operator op, Bexpression *left,
                                             Bexpression *right,
                                             Location location) {
  if (left == errorExpression() || right == errorExpression())
    return errorExpression();

  Btype *bltype = left->btype();
  Btype *brtype = right->btype();
  BComplexType *blctype = bltype->castToBComplexType();
  BComplexType *brctype = brtype->castToBComplexType();
  assert((blctype == nullptr) == (brctype == nullptr));
  if (blctype)
    return makeComplexBinaryExpr(op, left, right, location);

  // Arbitrarily select the left child type as the type for the binop.
  // This may be revised later during materializeBinary.
  Btype *btype = left->btype();
  Bexpression *rval =
      nbuilder_.mkBinaryOp(op, btype, nullptr, left, right, location);
  return rval;
}

bool
Llvm_backend::valuesAreConstant(const std::vector<Bexpression *> &vals)
{
  for (auto &val : vals)
    if (!val->isConstant())
       return false;
  return true;
}

// Return an expression that constructs BTYPE with VALS.

Bexpression *Llvm_backend::constructor_expression(
    Btype *btype, const std::vector<Bexpression *> &vals, Location location) {
  if (btype == errorType() || exprVectorHasError(vals))
    return errorExpression();

  Binstructions noInstructions;
  Bexpression *rval = nbuilder_.mkComposite(btype, nullptr, vals,
                                            noInstructions, location);
  return rval;
}

Bexpression *Llvm_backend::array_constructor_expression(
    Btype *array_btype, const std::vector<unsigned long> &indexes,
    const std::vector<Bexpression *> &vals, Location location)
{
  if (array_btype == errorType() || exprVectorHasError(vals))
    return errorExpression();

  Binstructions noInstructions;
  Bexpression *rval =
      nbuilder_.mkIndexedComposite(array_btype, nullptr, vals,
                                   indexes, noInstructions, location);
  return rval;
}

// Return an expression for the address of BASE[INDEX].

Bexpression *Llvm_backend::pointer_offset_expression(Bexpression *base,
                                                     Bexpression *index,
                                                     Location location) {
  if (base == errorExpression() || index == errorExpression())
    return errorExpression();

  Bexpression *rval = nbuilder_.mkPointerOffset(base->btype(), nullptr,
                                                base, index, location);
  return rval;
}

// Return an expression representing ARRAY[INDEX]

Bexpression *Llvm_backend::array_index_expression(Bexpression *barray,
                                                  Bexpression *index,
                                                  Location location)
{
  if (barray == errorExpression() || index == errorExpression())
    return errorExpression();

  Btype *bet = elementTypeByIndex(barray->btype(), 0);
  Bexpression *rval =
      nbuilder_.mkArrayIndex(bet, nullptr, barray, index, location);
  return rval;
}

// Create an expression for a call to FN_EXPR with FN_ARGS.
Bexpression *
Llvm_backend::call_expression(Bfunction *caller,
                              Bexpression *fn_expr,
                              const std::vector<Bexpression *> &fn_args,
                              Bexpression *chain_expr,
                              Location location) {
  if (fn_expr == errorExpression() || exprVectorHasError(fn_args) ||
      chain_expr == errorExpression())
    return errorExpression();

  BFunctionType *ft = unpackFunctionType(fn_expr->btype());
  Btype *rbtype = ft->resultType();
  Binstructions noInstructions;
  if (!chain_expr)
    chain_expr = nbuilder_.mkVoidValue(void_type());
  Bexpression *rval = nbuilder_.mkCall(rbtype, nullptr, caller,
                                       fn_expr, chain_expr, fn_args,
                                       noInstructions, location);
  return rval;
}

// Return an expression that allocates SIZE bytes on the stack.

Bexpression *Llvm_backend::stack_allocation_expression(int64_t size,
                                                       Location location) {
  assert(false && "Llvm_backend::stack_allocation_expression not yet impl");
  return nullptr;
}

Bstatement *Llvm_backend::error_statement()
{
  errorCount_++;
  return errorStatement();
}

// An expression as a statement.

Bstatement *Llvm_backend::expression_statement(Bfunction *bfunction,
                                               Bexpression *expr)
{
  if (expr == errorExpression() || bfunction == errorFunction_.get())
    return errorStatement();
  expr = materialize(expr);
  Bstatement *es =
      nbuilder_.mkExprStmt(bfunction, resolve(expr), expr->location());
  return es;
}

// Variable initialization.

Bstatement *Llvm_backend::init_statement(Bfunction *bfunction,
                                         Bvariable *var,
                                         Bexpression *init)
{
  if (var == errorVariable_.get() || init == errorExpression() ||
      bfunction == errorFunction_.get())
    return errorStatement();

  return makeInitStatement(bfunction, var, init);
}

Bstatement *Llvm_backend::makeInitStatement(Bfunction *bfunction,
                                            Bvariable *var,
                                            Bexpression *init)
{
  if (init) {
    if (traceLevel() > 1) {
      std::cerr << "\n^ init statement " << ((void*)var)
                << " = " << ((void*)init) << "\n";
      std::cerr << "\nLHS:\n";
      var->dump();
      std::cerr << "\nRHS:\n";
      init->dump();
    }

    init = materialize(init);
    if (init->compositeInitPending()) {
      init = resolveCompositeInit(init, var->value());
      Bstatement *es = nbuilder_.mkExprStmt(bfunction, init,
                                            init->location());
      var->setInitializer(es->getExprStmtExpr()->value());
      return es;
    }
  } else {
    init = zero_expression(var->btype());
  }
  Bexpression *varexp = nbuilder_.mkVar(var, var->location());
  Bstatement *st = makeAssignment(bfunction, var->value(),
                                  varexp, init, Location());
  llvm::Value *ival = st->getExprStmtExpr()->value();
  if (! llvm::isa<llvm::UndefValue>(ival))
    var->setInitializer(ival);
  return st;
}

Bstatement *Llvm_backend::makeAssignment(Bfunction *function,
                                         llvm::Value *lval, Bexpression *lhs,
                                         Bexpression *rhs, Location location)
{
  assert(lval->getType()->isPointerTy());

  // This cases should have been handled in the caller
  assert(!rhs->compositeInitPending());

  // Invoke helper to create store or memcpy
  Bexpression *stexp = genStore(function, rhs, lhs, location);

  // Return wrapped in statement
  return nbuilder_.mkExprStmt(function, stexp, location);
}

// Assignment.

Bstatement *Llvm_backend::assignment_statement(Bfunction *bfunction,
                                               Bexpression *lhs,
                                               Bexpression *rhs,
                                               Location location) {
  if (lhs == errorExpression() || rhs == errorExpression() ||
      bfunction == errorFunction_.get())
    return errorStatement();

  if (traceLevel() > 1) {
    std::cerr << "\n^ assignment statement " << ((void*)lhs)
              << " = " << ((void*)rhs) << "\n";
    std::cerr << "\nLHS:\n";
    lhs->dump();
    std::cerr << "\nRHS:\n";
    rhs->dump();
  }

  lhs = materialize(lhs, VE_lvalue);
  rhs = materialize(rhs);
  Bexpression *lhs2 = resolveVarContext(lhs, VE_lvalue);
  Bexpression *rhs2 = rhs;
  if (rhs->compositeInitPending()) {
    rhs2 = resolveCompositeInit(rhs, lhs2->value());
    Bexpression *stexp =
        nbuilder_.mkBinaryOp(OPERATOR_EQ, voidType(), lhs2->value(),
                             lhs2, rhs2, location);
    Bstatement *es = nbuilder_.mkExprStmt(bfunction, stexp, location);
    return es;
  }

  Bstatement *st = makeAssignment(bfunction, lhs->value(),
                                  lhs2, rhs2, location);
  return st;
}

Bstatement*
Llvm_backend::return_statement(Bfunction *bfunction,
                               const std::vector<Bexpression *> &vals,
                               Location location)
{
  if (bfunction == errorFunction_.get() || exprVectorHasError(vals))
    return errorStatement();

  // Materialize llvm instructions/values for the return vals.
  std::vector<Bexpression *> materializedVals;
  for (auto &val : vals)
    materializedVals.push_back(materialize(val));

  // Resolve arguments
  std::vector<Bexpression *> resolvedVals;
  for (auto &val : materializedVals)
    resolvedVals.push_back(resolve(val, varContextDisp(val)));

  // Collect up the return value
  Bexpression *toret = nullptr;
  if (vals.size() == 0) {
    // The return Bexpression node has a single child, so in the case
    // of the void function, create a dummy return value (easier than
    // making return a variadic node).
    toret = nbuilder_.mkConst(voidType(),
                              llvm::UndefValue::get(llvmVoidType()));
  } else if (vals.size() == 1) {
    toret = resolvedVals[0];
  } else {
    Btype *rtyp = bfunction->fcnType()->resultType();
    Bexpression *structVal =
        materialize(constructor_expression(rtyp, resolvedVals, location));
    if (structVal->compositeInitPending()) {
      structVal = resolveCompositeInit(structVal, nullptr);
    } else if (structVal->isConstant()) {
      llvm::Constant *cval = llvm::cast<llvm::Constant>(structVal->value());
      Bvariable *cv = genVarForConstant(cval, structVal->btype());
      structVal = var_expression(cv, VE_rvalue, location);
      structVal = materialize(address_expression(structVal, location));
    }
    toret = structVal;
  }

  Binstructions retInsns;
  llvm::Value *rval = bfunction->genReturnSequence(toret, &retInsns, this);
  llvm::ReturnInst *ri = llvm::ReturnInst::Create(context_, rval);
  toret->appendInstructions(retInsns.instructions());
  toret->appendInstruction(ri);
  Bstatement *rst =
      nbuilder_.mkReturn(bfunction, toret, location);
  return rst;
}

// Create a statement that attempts to execute BSTAT and calls EXCEPT_STMT if an
// error occurs.  EXCEPT_STMT may be NULL.  FINALLY_STMT may be NULL and if not
// NULL, it will always be executed.  This is used for handling defers in Go
// functions.  In C++, the resulting code is of this form:
//   try { BSTAT; } catch { EXCEPT_STMT; } finally { FINALLY_STMT; }

Bstatement *Llvm_backend::exception_handler_statement(Bstatement *bstat,
                                                      Bstatement *except_stmt,
                                                      Bstatement *finally_stmt,
                                                      Location location)
{
  if (bstat == errorStatement() ||
      except_stmt == errorStatement() ||
      finally_stmt == errorStatement())
    return errorStatement();

  Bfunction *func = bstat->function();
  assert(func == except_stmt->function());
  assert(!finally_stmt || func == finally_stmt->function());

  Bstatement *excepst = nbuilder_.mkExcepStmt(func, bstat,
                                              except_stmt,
                                              finally_stmt, location);
  return excepst;
}

// If statement.

Bstatement *Llvm_backend::if_statement(Bfunction *bfunction,
                                       Bexpression *condition,
                                       Bblock *then_block, Bblock *else_block,
                                       Location location) {
  if (condition == errorExpression())
    return errorStatement();
  Btype *bt = makeAuxType(llvmBoolType());
  Bexpression *conv = convert_expression(bt, condition, location);
  conv = materialize(conv);
  assert(then_block);

  Bstatement *ifst = nbuilder_.mkIfStmt(bfunction, conv, then_block,
                                        else_block, location);
  return ifst;
}

// Switch.

Bstatement *Llvm_backend::switch_statement(Bfunction *bfunction,
                                           Bexpression *value,
    const std::vector<std::vector<Bexpression *>> &cases,
    const std::vector<Bstatement *> &statements, Location switch_location)
{
  // Error handling
  if (value == errorExpression())
    return errorStatement();
  for (auto casev : cases)
    if (exprVectorHasError(casev))
      return errorStatement();
  if (stmtVectorHasError(statements))
      return errorStatement();

  value = materialize(value);

  // Resolve value
  value = resolve(value);

  // Case expressions are expected to be constants for this flavor of switch
  for (auto &bexpvec : cases)
    for (auto &exp : bexpvec)
      if (! llvm::cast<llvm::Constant>(exp->value()))
        go_assert(false && "bad case value expression");

  // Store results stmt
  Bstatement *swst =
      nbuilder_.mkSwitchStmt(bfunction, value, cases, statements,
                             switch_location);
  return swst;
}

// Pair of statements.

Bstatement *Llvm_backend::compound_statement(Bstatement *s1, Bstatement *s2) {
  if (s1 == errorStatement() || s2 == errorStatement())
    return errorStatement();

  assert(!s1->function() || !s2->function() ||
         s1->function() == s2->function());

  std::vector<Bstatement *> stvec;
  stvec.push_back(s1);
  stvec.push_back(s2);
  return statement_list(stvec);
}

// List of statements.

Bstatement *
Llvm_backend::statement_list(const std::vector<Bstatement *> &statements) {

  Bfunction *func = nullptr;
  for (auto &st : statements) {
    if (st == errorStatement())
      return errorStatement();
    if (st->function()) {
      if (func)
        assert(st->function() == func);
      else
        func = st->function();
    }
  }

  std::vector<Bvariable *> novars;
  Bblock *block = nbuilder_.mkBlock(func, novars, Location());
  for (auto &st : statements)
    nbuilder_.addStatementToBlock(block, st);
  return block;
}

Bblock *Llvm_backend::block(Bfunction *function, Bblock *enclosing,
                            const std::vector<Bvariable *> &vars,
                            Location start_location, Location) {
  assert(function);

  // FIXME: record debug location

  // Create new Bblock
  Bblock *bb = nbuilder_.mkBlock(function, vars, start_location);
  function->addBlock(bb);

  // FIXME:
  // Mark start and end of lifetime for each variable
  // for (auto var : vars) {
  //   Not yet implemented
  // }

  return bb;
}

// Add statements to a block.

void Llvm_backend::block_add_statements(Bblock *bblock,
                           const std::vector<Bstatement *> &statements) {
  for (auto st : statements)
    if (st == errorStatement())
      return;
  assert(bblock);
  for (auto st : statements)
    nbuilder_.addStatementToBlock(bblock, st);
}

// Return a block as a statement.

Bstatement *Llvm_backend::block_statement(Bblock *bblock)
{
  return bblock; // class Bblock inherits from Bstatement
}

// Helper routine for creating module-scope variables (static, global, etc).

Bvariable *
Llvm_backend::makeModuleVar(Btype *btype,
                            const std::string &name,
                            const std::string &asm_name,
                            Location location,
                            ModVarConstant isConstant,
                            ModVarSec inUniqueSection,
                            ModVarComdat inComdat,
                            ModVarVis isHiddenVisibility,
                            ModVarExtInit isExtInit,
                            ModVarGenDebug genDebug,
                            llvm::GlobalValue::LinkageTypes linkage,
                            llvm::Constant *initializer,
                            unsigned alignment)
{
  if (btype == errorType())
    return errorVariable_.get();

#if 0
  // FIXME: add code to insure non-zero size
  assert(datalayout().getTypeSizeInBits(btype->type()) != 0);
#endif

  // FIXME: at the moment the driver is enabling separate sections
  // for all variables, since there doesn't seem to be an easily
  // accessible hook for requesting a separate section for a single
  // variable.
  assert(inUniqueSection == MV_DefaultSection);

  llvm::Constant *init =
      (isExtInit == MV_ExternallyInitialized ? nullptr :
       llvm::Constant::getNullValue(btype->type()));
  std::string gname(asm_name.empty() ? name : asm_name);
  llvm::GlobalVariable *glob = new llvm::GlobalVariable(
      module(), btype->type(), isConstant == MV_Constant,
      linkage, init, gname);
  if (isHiddenVisibility == MV_HiddenVisibility)
    glob->setVisibility(llvm::GlobalValue::HiddenVisibility);
  if (alignment)
    glob->setAlignment(alignment);
  if (initializer)
    glob->setInitializer(initializer);
  if (inComdat == MV_InComdat) {
    assert(! gname.empty());
    glob->setComdat(module().getOrInsertComdat(gname));
  }
  if (isExtInit == MV_ExternallyInitialized) {
    assert(!init);
    glob->setExternallyInitialized(true);
  }

  bool addressTaken = true; // for now
  Bvariable *bv =
      new Bvariable(btype, location, gname, GlobalVar, addressTaken, glob);
  assert(valueVarMap_.find(bv->value()) == valueVarMap_.end());
  valueVarMap_[bv->value()] = bv;
  if (genDebug == MV_GenDebug && dibuildhelper() && !errorCount_) {
    bool exported = (isHiddenVisibility != MV_HiddenVisibility);
    dibuildhelper()->processGlobal(bv, exported);
  }
  return bv;
}

// Make a global variable.

Bvariable *Llvm_backend::global_variable(const std::string &var_name,
                                         const std::string &asm_name,
                                         Btype *btype,
                                         bool is_external,
                                         bool is_hidden,
                                         bool in_unique_section,
                                         Location location)
{
  llvm::GlobalValue::LinkageTypes linkage = llvm::GlobalValue::ExternalLinkage;

  ModVarSec inUniqSec =
      (in_unique_section ? MV_UniqueSection : MV_DefaultSection);
  ModVarVis varVis =
      (is_hidden ? MV_HiddenVisibility : MV_DefaultVisibility);
  ModVarExtInit extInit =
      (is_external ? MV_ExternallyInitialized : MV_NotExternallyInitialized);
  Bvariable *gvar =
      makeModuleVar(btype, var_name, asm_name, location,
                    MV_NonConstant, inUniqSec, MV_NotInComdat,
                    varVis, extInit, MV_GenDebug, linkage, nullptr);
  return gvar;
}

// Set the initial value of a global variable.

void Llvm_backend::global_variable_set_init(Bvariable *var, Bexpression *expr)
{
  if (var == errorVariable_.get() || expr == errorExpression())
    return;
  assert(llvm::isa<llvm::GlobalVariable>(var->value()));
  llvm::GlobalVariable *gvar = llvm::cast<llvm::GlobalVariable>(var->value());

  expr = materialize(expr);

  if (expr->compositeInitPending())
    expr = resolveCompositeInit(expr, gvar);

  assert(llvm::isa<llvm::Constant>(expr->value()));
  llvm::Constant *econ = llvm::cast<llvm::Constant>(expr->value());

  gvar->setInitializer(econ);
}

Bvariable *Llvm_backend::error_variable()
{
  errorCount_++;
  return errorVariable_.get();
}

// Make a local variable.

Bvariable *Llvm_backend::local_variable(Bfunction *function,
                                        const std::string &name,
                                        Btype *btype,
                                        bool is_address_taken,
                                        Location location)
{
  assert(function);
  if (btype == errorType() || function == errorFunction_.get())
    return errorVariable_.get();
  return function->localVariable(name, btype, is_address_taken, location);
}

// Make a function parameter variable.

Bvariable *Llvm_backend::parameter_variable(Bfunction *function,
                                            const std::string &name,
                                            Btype *btype, bool is_address_taken,
                                            Location location)
{
  assert(function);
  if (btype == errorType() || function == errorFunction_.get())
    return errorVariable_.get();
  return function->parameterVariable(name, btype,
                                     is_address_taken, location);
}

// Make a static chain variable.

Bvariable *Llvm_backend::static_chain_variable(Bfunction *function,
                                               const std::string &name,
                                               Btype *btype,
                                               Location location)
{
  if (function == errorFunction_.get() || btype == errorType())
    return errorVariable_.get();
  return function->staticChainVariable(name, btype, location);
}

// Make a temporary variable.

Bvariable *Llvm_backend::temporary_variable(Bfunction *function,
                                            Bblock *bblock,
                                            Btype *btype,
                                            Bexpression *binit,
                                            bool is_address_taken,
                                            Location location,
                                            Bstatement **pstatement)
{
  if (binit == errorExpression())
    return errorVariable_.get();
  std::string tname(namegen("tmpv"));
  Bvariable *tvar = local_variable(function, tname, btype,
                                   is_address_taken, location);
  if (tvar == errorVariable_.get())
    return tvar;
  tvar->markAsTemporary();
  Bstatement *is = init_statement(function, tvar, binit);
  *pstatement = is;
  return tvar;
}

// Create an implicit variable that is compiler-defined.  This is used when
// generating GC root variables and storing the values of a slice initializer.

Bvariable *Llvm_backend::implicit_variable(const std::string &name,
                                           const std::string &asm_name,
                                           Btype *btype,
                                           bool is_hidden,
                                           bool is_constant,
                                           bool is_common,
                                           int64_t ialignment)
{
  if (btype == errorType())
    return errorVariable_.get();

  // Vett alignment
  assert(ialignment >= 0);
  assert(ialignment < 1<<30);
  unsigned alignment = static_cast<unsigned>(ialignment);

  // Common + hidden makes no sense
  assert(!(is_hidden && is_common));

  llvm::GlobalValue::LinkageTypes linkage =
      (is_hidden ? llvm::GlobalValue::InternalLinkage :
       (is_common ? llvm::GlobalValue::WeakAnyLinkage
        : llvm::GlobalValue::ExternalLinkage));

  ModVarComdat inComdat = (is_common ? MV_InComdat : MV_NotInComdat);
  ModVarSec inUniqSec = MV_DefaultSection;
  ModVarVis varVis = MV_DefaultVisibility;
  ModVarConstant isConst = (is_constant ? MV_Constant : MV_NonConstant);
  ModVarExtInit extInit = MV_NotExternallyInitialized;

  Bvariable *gvar =
      makeModuleVar(btype, name, asm_name, Location(),
                    isConst, inUniqSec, inComdat,
                    varVis, extInit, MV_SkipDebug, linkage,
                    nullptr, alignment);
  return gvar;
}

// Set the initalizer for a variable created by implicit_variable.
// This is where we finish compiling the variable.

void Llvm_backend::implicit_variable_set_init(Bvariable *var,
                                              const std::string &,
                                              Btype *type,
                                              bool, bool, bool is_common,
                                              Bexpression *init)
{
  if (init != nullptr && init == errorExpression())
    return;
  if (var == errorVariable_.get())
    return;
  if (!init)
    init = zero_expression(type);
  global_variable_set_init(var, init);
}

// Return a reference to an implicit variable defined in another package.

Bvariable *Llvm_backend::implicit_variable_reference(const std::string &name,
                                                     const std::string &asmname,
                                                     Btype *btype)
{
  assert(false && "Llvm_backend::implicit_variable_reference not yet impl");
  return nullptr;
}

// Create a named immutable initialized data structure.

Bvariable *Llvm_backend::immutable_struct(const std::string &name,
                                          const std::string &asm_name,
                                          bool is_hidden,
                                          bool is_common,
                                          Btype *btype,
                                          Location location)
{
  if (btype == errorType())
    return errorVariable_.get();

  // Common + hidden makes no sense
  assert(!(is_hidden && is_common));

  llvm::GlobalValue::LinkageTypes linkage =
      (is_hidden ? llvm::GlobalValue::InternalLinkage :
       (is_common ? llvm::GlobalValue::WeakAnyLinkage
        : llvm::GlobalValue::ExternalLinkage));

  ModVarSec inUniqueSec = MV_DefaultSection;
  ModVarComdat inComdat = (is_common ? MV_InComdat : MV_NotInComdat);
  ModVarVis varVis = MV_DefaultVisibility;
  ModVarExtInit extInit = MV_NotExternallyInitialized;
  Bvariable *gvar =
      makeModuleVar(btype, name, asm_name, location,
                    MV_Constant, inUniqueSec, inComdat,
                    varVis, extInit, MV_SkipDebug, linkage, nullptr);
  return gvar;
}

// Set the initializer for a variable created by immutable_struct.
// This is where we finish compiling the variable.

void Llvm_backend::immutable_struct_set_init(Bvariable *var,
                                             const std::string &,
                                             bool is_hidden,
                                             bool is_common,
                                             Btype *,
                                             Location,
                                             Bexpression *initializer)
{
  if (var == errorVariable_.get() || initializer == errorExpression())
    return;

  initializer = materialize(initializer);

  assert(llvm::isa<llvm::GlobalVariable>(var->value()));
  llvm::GlobalVariable *gvar = llvm::cast<llvm::GlobalVariable>(var->value());
  assert(llvm::isa<llvm::Constant>(var->value()));
  llvm::Constant *econ = llvm::cast<llvm::Constant>(initializer->value());
  gvar->setInitializer(econ);
}

// Return a reference to an immutable initialized data structure
// defined in another package.

Bvariable *Llvm_backend::immutable_struct_reference(const std::string &name,
                                                    const std::string &asm_name,
                                                    Btype *btype,
                                                    Location location)
{
  if (btype == errorType())
    return errorVariable_.get();

  // Seen this already?
  std::string gname(asm_name.empty() ? name : asm_name);
  auto it = immutableStructRefs_.find(gname);
  if (it != immutableStructRefs_.end()) {
    // type should agree
    Bvariable *existing = it->second;
    assert(btype->type() == existing->btype()->type());
    return existing;
  }

  // Create new external global
  llvm::GlobalValue::LinkageTypes linkage = llvm::GlobalValue::ExternalLinkage;
  bool isConstant = true;
  llvm::Constant *init = nullptr;
  llvm::GlobalVariable *glob = new llvm::GlobalVariable(
      module(), btype->type(), isConstant, linkage, init, gname);
  Bvariable *bv =
      new Bvariable(btype, location, name, GlobalVar, false, glob);
  assert(valueVarMap_.find(bv->value()) == valueVarMap_.end());
  valueVarMap_[bv->value()] = bv;
  immutableStructRefs_[gname] = bv;

  return bv;
}

// Make a label.

Blabel *Llvm_backend::label(Bfunction *function,
                            const std::string &name,
                            Location location)
{
  assert(function);
  return function->newLabel(location);
}

// Make a statement which defines a label.

Bstatement *Llvm_backend::label_definition_statement(Blabel *label)
{
  Bfunction *function = label->function();
  Bstatement *st = nbuilder_.mkLabelDefStmt(function, label, label->location());
  function->registerLabelDefStatement(st, label);
  return st;
}

// Make a goto statement.

Bstatement *Llvm_backend::goto_statement(Blabel *label, Location location)
{
  Bfunction *function = label->function();
  return nbuilder_.mkGotoStmt(function, label, location);
}

// Get the address of a label.

Bexpression *Llvm_backend::label_address(Blabel *label, Location location)
{
  assert(label);

  // Reuse existing placeholder (if address already taken for this label), or
  // create new placeholder if needed.
  llvm::Value *pval = nullptr;
  if (label->placeholder()) {
    pval = label->placeholder();
  } else {
    Bfunction *fcn = label->function();
    pval = fcn->createLabelAddressPlaceholder(boolType());
    label->setPlaceholder(pval);
  }
  // Note: the expression we're creating will eventually be replaced
  // by an llvm::BlockAddress value, which will have type "i8*", so
  // use that type here so as to avoid having to insert conversions.
  Btype *ppt = pointerType(boolType());
  return nbuilder_.mkLabelAddress(ppt, pval, label, location);
}

Bfunction *Llvm_backend::error_function()
{
  errorCount_++;
  return errorFunction_.get();
}

// Declare or define a new function.

Bfunction *Llvm_backend::function(Btype *fntype, const std::string &name,
                                  const std::string &asm_name, bool is_visible,
                                  bool is_declaration, bool is_inlinable,
                                  bool disable_split_stack,
                                  bool in_unique_section, Location location)
{
  if (fntype == errorType())
    return errorFunction_.get();
  BFunctionType *ft = fntype->castToBFunctionType();
  assert(ft);
  llvm::FunctionType *fty = llvm::cast<llvm::FunctionType>(ft->type());

  // If this is a declaration, then look to see if we already have an existing
  // function with the same name, and reuse that if need be. Check to make
  // sure that the function types agree if we see a hit in the cache.
  std::string fns(!asm_name.empty() ? asm_name : name);
  if (is_declaration) {
    assert(ft);
    fcnNameAndType candidate(std::make_pair(ft, fns));
    auto it = fcnDeclMap_.find(candidate);
    if (it != fcnDeclMap_.end()) {
      Bfunction *found = it->second;
      return found;
    }
  }

  llvm::GlobalValue::LinkageTypes linkage = llvm::GlobalValue::ExternalLinkage;
  llvm::StringRef fn(fns);
  llvm::Constant *fcnValue = nullptr;
  if (! module_->getNamedValue(fn)) {
    llvm::Twine fnt(fns);
    llvm::Function *fcn = llvm::Function::Create(fty, linkage, fnt, module_);

    fcn->addFnAttr("disable-tail-calls", "true");

    // visibility
    if (!is_visible)
      fcn->setVisibility(llvm::GlobalValue::HiddenVisibility);

    // inline/noinline
    if (!is_inlinable)
      fcn->addFnAttr(llvm::Attribute::NoInline);

    // split-stack or nosplit
    if (! disable_split_stack)
      fcn->addFnAttr("split-stack");

    fcnValue = fcn;
  } else {

    // A function of the same name has already been created in this
    // module. Call a module helper to get a constant corresponding
    // to the original fcn bit-casted to the new type.
    fcnValue = module_->getOrInsertFunction(fn, fty);
  }

  BFunctionType *fcnType = fntype->castToBFunctionType();
  assert(fcnType);
  Bfunction *bfunc = new Bfunction(fcnValue, fcnType, name, asm_name, location,
                                   typeManager());

  // split-stack or nosplit
  if (disable_split_stack)
    bfunc->setSplitStack(Bfunction::NoSplit);

  // TODO: unique section support. llvm::GlobalObject has support for
  // setting COMDAT groups and section names, but there doesn't seem
  // to be an interface available to request a unique section on a
  // per-function basis (only a translation-unit-wide default).
  assert(!in_unique_section || is_declaration);

  if (is_declaration) {
    fcnNameAndType candidate(std::make_pair(ft, fns));
    fcnDeclMap_[candidate] = bfunc;
  }
  functions_.push_back(bfunc);
  return bfunc;
}

// Create a statement that runs all deferred calls for FUNCTION.  This should
// be a statement that looks like this in C++:
//   finish:
//     try { UNDEFER; } catch { CHECK_DEFER; goto finish; }

Bstatement *Llvm_backend::function_defer_statement(Bfunction *function,
                                                   Bexpression *undefer,
                                                   Bexpression *defer,
                                                   Location location)
{
  if (function == errorFunction_.get() ||
      undefer == errorExpression() ||
      defer == errorExpression())
    return errorStatement();

  undefer = materialize(undefer);
  defer = materialize(defer);

  Bstatement *defst = nbuilder_.mkDeferStmt(function, undefer, defer,
                                            location);
  return defst;
}

// Record PARAM_VARS as the variables to use for the parameters of FUNCTION.
// This will only be called for a function definition.

bool Llvm_backend::function_set_parameters(
    Bfunction *function, const std::vector<Bvariable *> &param_vars)
{
  // At the moment this is a no-op.
  return true;
}

//
// Helper class for assigning instructions to LLVM basic blocks
// and materializing control transfers.
//
// FIXME: convert to use new Bnode walk paradigm.
//
class GenBlocks {
public:
  GenBlocks(llvm::LLVMContext &context, Llvm_backend *be,
            Bfunction *function, Bnode *topNode,
            DIBuildHelper *diBuildHelper, llvm::BasicBlock *entryBlock);

  llvm::BasicBlock *walk(Bnode *node, llvm::BasicBlock *curblock);
  void finishFunction(llvm::BasicBlock *entry);

  Bfunction *function() { return function_; }
  llvm::BasicBlock *genIf(Bstatement *ifst,
                          llvm::BasicBlock *curblock);
  llvm::BasicBlock *genSwitch(Bstatement *swst,
                              llvm::BasicBlock *curblock);
  llvm::BasicBlock *genDefer(Bstatement *defst,
                             llvm::BasicBlock *curblock);
  llvm::BasicBlock *genReturn(Bstatement *rst,
                             llvm::BasicBlock *curblock);
  llvm::BasicBlock *genExcep(Bstatement *excepst,
                             llvm::BasicBlock *curblock);

 private:
  llvm::BasicBlock *mkLLVMBlock(const std::string &name,
                            unsigned expl = Llvm_backend::ChooseVer);
  llvm::BasicBlock *eraseBlockIfUnused(llvm::BasicBlock *bb);
  llvm::BasicBlock *getBlockForLabel(Blabel *lab);
  llvm::BasicBlock *walkExpr(llvm::BasicBlock *curblock, Bexpression *expr);
  std::pair<llvm::Instruction*, llvm::BasicBlock *>
  rewriteToMayThrowCall(llvm::CallInst *call,
                        llvm::BasicBlock *curblock);
  std::pair<llvm::Instruction*, llvm::BasicBlock *>
  postProcessInst(llvm::Instruction *inst,
                  llvm::BasicBlock *curblock);
  DIBuildHelper *dibuildhelper() const { return dibuildhelper_; }
  Llvm_linemap *linemap() { return be_->linemap(); }

 private:
  llvm::LLVMContext &context_;
  Llvm_backend *be_;
  Bfunction *function_;
  DIBuildHelper *dibuildhelper_;
  std::map<LabelId, llvm::BasicBlock *> labelmap_;
  std::vector<llvm::BasicBlock*> padBlockStack_;
  std::set<llvm::AllocaInst *> temporariesDiscovered_;
  std::vector<llvm::AllocaInst *> newTemporaries_;
  llvm::BasicBlock* finallyBlock_;
  Bstatement *cachedReturn_;
  bool emitOrphanedCode_;
};

GenBlocks::GenBlocks(llvm::LLVMContext &context,
                     Llvm_backend *be,
                     Bfunction *function,
                     Bnode *topNode,
                     DIBuildHelper *dibuildhelper,
                     llvm::BasicBlock *entryBlock)
    : context_(context), be_(be), function_(function),
      dibuildhelper_(dibuildhelper), finallyBlock_(nullptr),
      cachedReturn_(nullptr), emitOrphanedCode_(false)
{
  if (dibuildhelper_)
    dibuildhelper_->beginFunction(function, topNode, entryBlock);
}

void GenBlocks::finishFunction(llvm::BasicBlock *entry)
{
  function_->fixupProlog(entry, newTemporaries_);
  if (dibuildhelper_)
    dibuildhelper_->endFunction(function_);
}

llvm::BasicBlock *GenBlocks::mkLLVMBlock(const std::string &name,
                                         unsigned expl)
{
  std::string tname = be_->namegen(name, expl);
  llvm::Function *func = function()->function();
  return llvm::BasicBlock::Create(context_, tname, func);
}

llvm::BasicBlock *GenBlocks::getBlockForLabel(Blabel *lab) {
  auto it = labelmap_.find(lab->label());
  if (it != labelmap_.end())
    return it->second;
  llvm::BasicBlock *bb = mkLLVMBlock("label", lab->label());
  labelmap_[lab->label()] = bb;
  return bb;
}

// This helper routine takes a garden variety call instruction and
// rewrites it to an equivalent llvm::InvokeInst that may throw an
// exception (with associated explicit EH control flow). The helper is
// used to fix up calls that appear within the body or catch clauses
// of an EH statement (see Llvm_backend::exception_handler_statement).
// Redoing calls in this way (as opposed to creating the correct
// flavor of call from the get-go) is mildly hacky, but seems to be
// the most practical way to get the sort of call we need, given that
// at the point where Backend::call_expression is originally invoked
// we don't know whether the call will reside in a try block.

std::pair<llvm::Instruction*, llvm::BasicBlock *>
GenBlocks::rewriteToMayThrowCall(llvm::CallInst *call,
                                 llvm::BasicBlock *curblock)
{
  llvm::BasicBlock *padbb = padBlockStack_.back();
  llvm::Function *func = function()->function();

  // Create 'continue' block, where control will flow if
  // the call to the function does not result in an exception.
  llvm::BasicBlock *contbb =
      llvm::BasicBlock::Create(context_, be_->namegen("cont"), func);

  // Create a new InvokeInst that is equivalent (in terms of
  // target, operands, etc) to the CallInst 'call', but uses a
  // possibly-excepting call with landing pad.
  llvm::SmallVector<llvm::Value *, 8> args(call->arg_begin(), call->arg_end());
  llvm::InvokeInst *invcall =
      llvm::InvokeInst::Create(call->getCalledValue(),
                               contbb, padbb, args,
                               call->getName());

  // Rewrite uses of the original call's return value to be the new call's
  // return value.
  call->replaceAllUsesWith(invcall);

  // New call needs same attributes
  invcall->setAttributes(call->getAttributes());

  // Old call longer needed
  call->deleteValue();

  return std::make_pair(invcall, contbb);
}

// Hook to post-process an LLVM instruction immediately before it
// is assigned to a block.

std::pair<llvm::Instruction*, llvm::BasicBlock *>
GenBlocks::postProcessInst(llvm::Instruction *inst,
                           llvm::BasicBlock *curblock)
{
  for (llvm::Instruction::op_iterator oi = inst->op_begin(),
           oe = inst->op_end(); oi != oe; ++oi) {
    if (llvm::isa<llvm::AllocaInst>(*oi)) {
      llvm::AllocaInst *ai = llvm::cast<llvm::AllocaInst>(*oi);

      // If this alloca is associated with a temporary variable
      // that was manufactured at some point during IR construction,
      // then gather it up into a set to be inserted into the prolog
      // block.
      Bvariable *tvar = be_->nodeBuilder().adoptTemporaryVariable(ai);
      if (tvar) {
        temporariesDiscovered_.insert(ai);
        newTemporaries_.push_back(ai);
        delete tvar;
      }
    }
  }

  if (llvm::isa<llvm::CallInst>(inst) && !padBlockStack_.empty()) {
    llvm::CallInst *call = llvm::cast<llvm::CallInst>(inst);
    llvm::Function *func = call->getCalledFunction();
    if (!func || !func->isIntrinsic())
      return rewriteToMayThrowCall(call, curblock);
  }
  return std::make_pair(inst, curblock);
}

llvm::BasicBlock *GenBlocks::walkExpr(llvm::BasicBlock *curblock,
                                      Bexpression *expr)
{
  // Delete dead instructions before visiting the children,
  // as they may use values defined in the children. Uses
  // need to be deleted before deleting definition.
  if (!curblock)
    be_->nodeBuilder().destroy(expr, DelInstructions, false);

  // Visit children first
  const std::vector<Bnode *> &kids = expr->children();
  for (auto &child : kids)
    curblock = walk(child, curblock);

  // In case it becomes dead after visiting some child...
  if (!curblock)
    be_->nodeBuilder().destroy(expr, DelInstructions, false);

  // Now visit instructions for this expr
  // TODO: currently the control flow won't change from
  // live to dead in this loop. Handle it, especially
  // deallocate part of the instruction list, if it
  // becomes necessary.
  bool changed = false;
  std::vector<llvm::Instruction*> newinsts;
  for (auto originst : expr->instructions()) {
    auto pair = postProcessInst(originst, curblock);
    auto inst = pair.first;
    if (inst != originst)
      changed = true;
    if (dibuildhelper_)
      dibuildhelper_->processExprInst(expr, inst);
    curblock->getInstList().push_back(inst);
    curblock = pair.second;
    newinsts.push_back(inst);
  }
  if (changed)
    be_->nodeBuilder().updateInstructions(expr, newinsts);
  expr->clear();
  return curblock;
}

llvm::BasicBlock *GenBlocks::genIf(Bstatement *ifst,
                                   llvm::BasicBlock *curblock)
{
  assert(ifst->flavor() == N_IfStmt);

  Bexpression *cond = ifst->getIfStmtCondition();
  Bstatement *trueStmt = ifst->getIfStmtTrueBlock();
  Bstatement *falseStmt = ifst->getIfStmtFalseBlock();

  // Walk condition first
  curblock = walkExpr(curblock, cond);

  // Create true block
  llvm::BasicBlock *tblock = mkLLVMBlock("then");

  // Create fallthrough block
  llvm::BasicBlock *ft = mkLLVMBlock("fallthrough");

  // Create false block if present
  llvm::BasicBlock *fblock = ft;
  if (falseStmt)
    fblock = mkLLVMBlock("else");

  // Insert conditional branch into current block
  llvm::Value *cval = cond->value();
  llvm::BranchInst::Create(tblock, fblock, cval, curblock);

  // Visit true block
  llvm::BasicBlock *tsucc = walk(trueStmt, tblock);
  if (tsucc && ! tsucc->getTerminator())
    llvm::BranchInst::Create(ft, tsucc);

  // Walk false block if present
  if (falseStmt) {
    llvm::BasicBlock *fsucc = walk(falseStmt, fblock);
    if (fsucc && ! fsucc->getTerminator())
      llvm::BranchInst::Create(ft, fsucc);
  }

  // Remove fallthrough block if it was never used
  ft = eraseBlockIfUnused(ft);

  return ft;
}

llvm::BasicBlock *GenBlocks::genSwitch(Bstatement *swst,
                                       llvm::BasicBlock *curblock)
{
  assert(swst->flavor() == N_SwitchStmt);

  // Walk switch value first
  Bexpression *swval = swst->getSwitchStmtValue();
  curblock = walkExpr(curblock, swval);

  // Unpack switch
  unsigned ncases = swst->getSwitchStmtNumCases();

  // No need to walk switch value expressions -- they should all be constants.

  // Create blocks
  llvm::BasicBlock *defBB = nullptr;
  std::vector<llvm::BasicBlock *> blocks(ncases);
  for (unsigned idx = 0; idx < ncases; ++idx) {
    std::vector<Bexpression *> thiscase =
        swst->getSwitchStmtNthCase(idx);
    bool isDefault = (thiscase.size() == 0);
    std::string bname(isDefault ? "default" : "case");
    blocks[idx] = mkLLVMBlock(bname);
    if (isDefault) {
      assert(! defBB);
      defBB = blocks[idx];
    }
  }
  llvm::BasicBlock *epilogBB = mkLLVMBlock("epilog");
  if (!defBB)
    defBB = epilogBB;

  LIRBuilder builder(context_, llvm::ConstantFolder());

  // Walk statement/block
  for (unsigned idx = 0; idx < ncases; ++idx) {
    Bstatement *st = swst->getSwitchStmtNthStmt(idx);
    llvm::BasicBlock *newblock = walk(st, blocks[idx]);
    if (newblock && newblock->getTerminator() == nullptr) {
      // The front end inserts a goto statement at the end for
      // non-fallthrough cases. Fall through to the next case
      // otherwise.
      assert(idx < ncases-1);
      builder.SetInsertPoint(newblock);
      builder.CreateBr(blocks[idx+1]);
    }
  }

  // Create switch
  builder.SetInsertPoint(curblock);
  llvm::SwitchInst *swinst = builder.CreateSwitch(swval->value(), defBB);

  // Connect values with blocks
  for (unsigned idx = 0; idx < blocks.size(); ++idx) {
    std::vector<Bexpression *> thiscase =
        swst->getSwitchStmtNthCase(idx);
    for (auto &exp : thiscase) {
      llvm::ConstantInt *ci = llvm::cast<llvm::ConstantInt>(exp->value());
      swinst->addCase(ci, blocks[idx]);
    }
  }

  // Delete epilog if not needed
  epilogBB = eraseBlockIfUnused(epilogBB);

  return epilogBB;
}

// If specified BB has no predecessors, remove it and return nullptr.
// Returns unmodified BB if the block does have preds.

llvm::BasicBlock *GenBlocks::eraseBlockIfUnused(llvm::BasicBlock *bb)
{
  if (llvm::pred_begin(bb) == llvm::pred_end(bb)) {
    // Block should be empty in this case
    assert(bb->begin() == bb->end());
    bb->eraseFromParent();
    return nullptr;
  }
  return bb;
}

// In most cases a return statement is handled in canonical way,
// that is, any associated expressions are added to the current block
// (including an llvm::ReturnInst) and the current block is closed out.
//
// Special handling is needed when the return appears within a
// try/catch block (see Llvm_backend::exception_handler_statement)
// with a "finally" clause. In such cases we have to rewrite each
// return instruction into a jump to the finally block.
//
// HACK: we're taking advantage of the fact that the front end always
// stores return values to a local variable as opposed to returning
// those values directly (hence evern return sequence should always
// look like "return X" where X is a load from an appropriately typed
// variable. This fact permits us to copy a return from one place in
// the program to another while getting the same semantics.

llvm::BasicBlock *GenBlocks::genReturn(Bstatement *rst,
                                       llvm::BasicBlock *curblock)
{
  assert(rst->flavor() == N_ReturnStmt);

  Bexpression *re = rst->getReturnStmtExpr();

  if (finallyBlock_) {
    // Rewrite the return to a jump into the 'finally' block (see above).
    // Here we also cache away a return statement so as to relocate
    // it after the 'finally' block.
    if (cachedReturn_ == nullptr) {
      // Steal this return.
      cachedReturn_ = rst;
    } else {
      be_->nodeBuilder().destroy(re, DelInstructions);
    }
    llvm::BranchInst::Create(finallyBlock_, curblock);
  } else {
    // Walk return expression
    llvm::BasicBlock *bb = walkExpr(curblock, re);
    assert(curblock == bb);
  }
  // A return terminates the current block
  return nullptr;
}

llvm::BasicBlock *GenBlocks::genDefer(Bstatement *defst,
                                      llvm::BasicBlock *curblock)
{
  assert(defst->flavor() == N_DeferStmt);

  // Insure that current function has personality routine set.
  llvm::Function *func = function()->function();
  if (! func->hasPersonalityFn())
    func->setPersonalityFn(be_->personalityFunction());

  Bexpression *defcallex = defst->getDeferStmtDeferCall();
  Bexpression *undcallex = defst->getDeferStmtUndeferCall();

  // Finish bb (see the comments for Llvm_backend::function_defer_statement
  // as to why we use this name).
  llvm::BasicBlock *finbb =
      llvm::BasicBlock::Create(context_, be_->namegen("finish"), func);

  // Landing pad to which control will be transferred if an exception
  // is thrown when executing "undcallex".
  llvm::BasicBlock *padbb =
      llvm::BasicBlock::Create(context_, be_->namegen("pad"), func);

  // Catch BB will contain checkdefer (defercall) code.
  llvm::BasicBlock *catchbb =
      llvm::BasicBlock::Create(context_, be_->namegen("catch"), func);
  if (curblock)
    llvm::BranchInst::Create(finbb, curblock);
  curblock = finbb;

  // Push pad block onto stack. This will be an indication that any call
  // in the undcallex subtree should be converted into an invoke with
  // suitable landing pad.
  padBlockStack_.push_back(padbb);

  // Walk the undcall expression.
  curblock = walkExpr(curblock, undcallex);

  // Pop the pad block stack.
  padBlockStack_.pop_back();

  // Emit landing pad into pad block, followed by branch to catch block.
  llvm::LandingPadInst *padinst =
      llvm::LandingPadInst::Create(be_->landingPadExceptionType(),
                                   0, be_->namegen("ex"), padbb);
  padinst->addClause(llvm::Constant::getNullValue(be_->llvmPtrType()));
  llvm::BranchInst::Create(catchbb, padbb);

  llvm::BasicBlock *contbb = curblock;

  // Catch block containing defer call.
  curblock = catchbb;
  auto bb = walkExpr(curblock, defcallex);
  assert(bb == curblock);
  llvm::BranchInst::Create(finbb, catchbb);

  // Continue bb is final bb.
  return contbb;
}

llvm::BasicBlock *GenBlocks::genExcep(Bstatement *excepst,
                                      llvm::BasicBlock *curblock)
{
  assert(excepst->flavor() == N_ExcepStmt);

  // Insure that current function has personality set.
  llvm::Function *func = function()->function();
  if (! func->hasPersonalityFn())
    func->setPersonalityFn(be_->personalityFunction());

  Bstatement *body = excepst->getExcepStmtBody();
  assert(body);
  Bstatement *ifexception = excepst->getExcepStmtOnException();
  assert(ifexception);
  Bstatement *finally = excepst->getExcepStmtFinally(); // may be null

  // Create a landing pad block. This pad will be where control
  // will arrive if an exception is thrown within the "body" code.
  llvm::BasicBlock *padbb =
      llvm::BasicBlock::Create(context_, be_->namegen("pad"), func);

  // Create a "finally" BB, corresponding to where control will wind
  // up once both the body and (possibly) the exception clause are
  // complete. This will also be where we'll place any code in the
  // "finally" statement above.
  std::string cbbname(be_->namegen("finally"));
  llvm::BasicBlock *contbb =
      llvm::BasicBlock::Create(context_, cbbname, func);

  // Not expecting to see nested exception statements, assert if
  // this crops up.
  assert(padBlockStack_.empty());

  // If there is a finally block, then record the block for purposes
  // of return handling.
  finallyBlock_ = (finally ? contbb : nullptr);
  assert(!cachedReturn_);

  // Push first pad block onto stack. The presence of a non-empty pad
  // block stack will be an indication that any call in the body
  // subtree should be converted into an invoke using the pad.
  padBlockStack_.push_back(padbb);

  // Walk the body statement.
  curblock = walk(body, curblock);

  // Pop the pad block stack.
  padBlockStack_.pop_back();

  // If the body block ended without a return, then insert a jump
  // to the continue / or finally clause.
  if (curblock)
    llvm::BranchInst::Create(contbb, curblock);

  // Emit landing pad inst in pad block, followed by branch to catch bb.
  llvm::LandingPadInst *padinst =
      llvm::LandingPadInst::Create(be_->landingPadExceptionType(),
                                   0, be_->namegen("ex"), padbb);
  padinst->addClause(llvm::Constant::getNullValue(be_->llvmPtrType()));
    llvm::BasicBlock *catchbb =
      llvm::BasicBlock::Create(context_, be_->namegen("catch"), func);
  llvm::BranchInst::Create(catchbb, padbb);

  // Create second pad block. Here the idea is that if an exception
  // is thrown as a result of any call within the catch ("ifexception" code)
  // it will wind up at this pad.
  llvm::BasicBlock *catchpadbb =
      llvm::BasicBlock::Create(context_, be_->namegen("catchpad"), func);

  // Push second pad, walk exception stmt, then pop the pad.
  padBlockStack_.push_back(catchpadbb);
  auto bb = walk(ifexception, catchbb);
  if (bb)
    llvm::BranchInst::Create(contbb, bb);
  padBlockStack_.pop_back();

  // Return handling now complete.
  finallyBlock_ = nullptr;

  // Fix up second pad block, followed by branch to continue block.
  llvm::LandingPadInst *padinst2 =
      llvm::LandingPadInst::Create(be_->landingPadExceptionType(),
                                   0, be_->namegen("ex2"), catchpadbb);
  padinst2->addClause(llvm::Constant::getNullValue(be_->llvmPtrType()));
  llvm::BranchInst::Create(contbb, catchpadbb);

  // Handle finally statement where applicable.
  curblock = contbb;
  if (finally != nullptr) {
    curblock = walk(finally, curblock);
    if (cachedReturn_ != nullptr) {
      Bexpression *re = cachedReturn_->getReturnStmtExpr();
      llvm::BasicBlock *bb = walkExpr(curblock, re);
      assert(curblock == bb);
      cachedReturn_ = nullptr;
    }
  }

  return curblock;
}

llvm::BasicBlock *GenBlocks::walk(Bnode *node,
                                  llvm::BasicBlock *curblock)
{
  Bexpression *expr = node->castToBexpression();
  if (expr)
    return walkExpr(curblock, expr);
  llvm::Function *func = function()->function();
  Bstatement *stmt = node->castToBstatement();
  assert(stmt);
  switch (stmt->flavor()) {
    case N_ExprStmt: {
      curblock = walkExpr(curblock, stmt->getExprStmtExpr());
      break;
    }
    case N_BlockStmt: {
      Bblock *bblock = stmt->castToBblock();
      if (dibuildhelper_)
        dibuildhelper_->beginLexicalBlock(bblock);
      for (auto &st : stmt->getChildStmts())
        curblock = walk(st, curblock);
      if (dibuildhelper_)
        dibuildhelper_->endLexicalBlock(bblock);
      break;
    }
    case N_IfStmt: {
      curblock = genIf(stmt, curblock);
      break;
    }
    case N_SwitchStmt: {
      curblock = genSwitch(stmt, curblock);
      break;
    }
    case N_ReturnStmt: {
      curblock = genReturn(stmt, curblock);
      break;
    }
    case N_DeferStmt: {
      curblock = genDefer(stmt, curblock);
      break;
    }
    case N_ExcepStmt: {
      curblock = genExcep(stmt, curblock);
      break;
    }
    case N_GotoStmt: {
      llvm::BasicBlock *lbb = getBlockForLabel(stmt->getGotoStmtTargetLabel());
      if (curblock && ! curblock->getTerminator())
        llvm::BranchInst::Create(lbb, curblock);
      if (emitOrphanedCode_) {
        std::string n = be_->namegen("orphan");
        llvm::BasicBlock *orphan =
            llvm::BasicBlock::Create(context_, n, func, lbb);
        curblock = orphan;
      } else {
        curblock = nullptr;
      }
      break;
    }
    case N_LabelStmt: {
      Blabel *label = stmt->getLabelStmtDefinedLabel();
      llvm::BasicBlock *lbb = getBlockForLabel(label);
      if (label->placeholder())
        function()->replaceLabelAddressPlaceholder(label->placeholder(), lbb);
      if (curblock)
        llvm::BranchInst::Create(lbb, curblock);
      curblock = lbb;
      break;
    }
    default:
      assert(false && "not yet handled");
  }
  return curblock;
}

llvm::BasicBlock *Llvm_backend::genEntryBlock(Bfunction *bfunction) {
  llvm::Function *func = bfunction->function();
  llvm::BasicBlock *entry = llvm::BasicBlock::Create(context_, "entry", func);

  // Spill parameters/arguments, insert allocas for local vars
  bfunction->genProlog(entry);

  return entry;
}

void Llvm_backend::fixupEpilogBlock(Bfunction *bfunction,
                                   llvm::BasicBlock *epilog)
{
  // Append a return instruction if the block does not end with
  // a control transfer.
  if (epilog->empty() || !epilog->back().isTerminator()) {
    LIRBuilder builder(context_, llvm::ConstantFolder());
    llvm::Function *func = bfunction->function();
    llvm::Type *rtyp= func->getFunctionType()->getReturnType();
    llvm::ReturnInst *ri = nullptr;
    if (rtyp->isVoidTy()) {
      ri = builder.CreateRetVoid();
    } else {
      llvm::Value *zv = llvm::Constant::getNullValue(rtyp);
      ri = builder.CreateRet(zv);
    }
    epilog->getInstList().push_back(ri);
  }
}

// Set the function body for FUNCTION using the code in CODE_BLOCK.

bool Llvm_backend::function_set_body(Bfunction *function,
                                     Bstatement *code_stmt)
{
  if (function == errorFunction_.get())
    return true;

  // debugging
  if (traceLevel() > 1) {
    std::cerr << "\nStatement tree dump:\n";
    code_stmt->dump();
  }

  // Invoke the tree integrity checker. We do this even if
  // checkIntegrity_ is false so to deal with any repairable
  // sharing that the front end may have introduced.
  enforceTreeIntegrity(code_stmt);

  // Create and populate entry block
  llvm::BasicBlock *entryBlock = genEntryBlock(function);

  // Avoid debug meta-generation if errors seen
  DIBuildHelper *dibh = nullptr;
  if (createDebugMetaData_ && errorCount_ == 0)
    dibh = dibuildhelper();

  // Walk the code statements
  GenBlocks gb(context_, this, function, code_stmt,
               dibh, entryBlock);
  llvm::BasicBlock *block = gb.walk(code_stmt, entryBlock);
  gb.finishFunction(entryBlock);

  // Fix up epilog block if needed
  if (block)
    fixupEpilogBlock(function, block);

  // debugging
  if (traceLevel() > 0) {
    std::cerr << "LLVM function dump:\n";
    function->function()->dump();
  }

  return true;
}

// Write the definitions for all TYPE_DECLS, CONSTANT_DECLS,
// FUNCTION_DECLS, and VARIABLE_DECLS declared globally, as well as
// emit early debugging information.

void Llvm_backend::write_global_definitions(
    const std::vector<Btype *> &type_decls,
    const std::vector<Bexpression *> &constant_decls,
    const std::vector<Bfunction *> &function_decls,
    const std::vector<Bvariable *> &variable_decls) {

  finalizeExportData();

  // At the moment there isn't anything to do here with the
  // inputs we're being passed.

}

// Post-process export data to escape quotes, etc, writing bytes
// to the specified stringstream.
static void postProcessExportDataChunk(const char *bytes,
                                       unsigned int size,
                                       std::stringstream &ss)
{
  std::map<char, std::string> rewrites = { { '\\', "\\\\" },
                                           { '\0', "\\000" },
                                           { '\n', "\\n" },
                                           { '"', "\\\"" } };

  for (unsigned idx = 0; idx < size; ++idx) {
    const char byte = bytes[idx];
    auto it = rewrites.find(byte);
    if (it != rewrites.end())
      ss << it->second;
    else
      ss << byte;
  }
}

// Finalize export data.

void Llvm_backend::finalizeExportData()
{
  // Calling this here, for lack of a better spot
  if (errorCount_ == 0 && dibuildhelper())
    dibuildhelper_->finalize();

  assert(! exportDataFinalized_);
  exportDataFinalized_ = true;
  module().appendModuleInlineAsm("\t.text\n");

  if (traceLevel() > 1) {
    std::cerr << "Export data emitted:\n";
    std::cerr << module().getModuleInlineAsm();
  }
}

// This is called by the Go frontend proper to add data to the
// section containing Go export data.

void Llvm_backend::write_export_data(const char *bytes, unsigned int size)
{
  // FIXME: this is hacky and currently very ELF-specific. Better to
  // add real support in MC object file layer.

  assert(! exportDataFinalized_);

  if (! exportDataStarted_) {
    exportDataStarted_ = true;
    const char *preamble = "\t.section \".go_export\",\"e\",@progbits";
    module().appendModuleInlineAsm(preamble);
  }

  std::stringstream ss;
  ss << "\t.ascii \"";
  postProcessExportDataChunk(bytes, size, ss);
  ss << "\"\n";
  module().appendModuleInlineAsm(ss.str());
}


// Convert an identifier for use in an error message.
// TODO(tham): scan the identifier to determine if contains
// only ASCII or printable UTF-8, then perform character set
// conversions if needed.

const char *go_localize_identifier(const char *ident) { return ident; }

// Return a new backend generator.

Backend *go_get_backend(llvm::LLVMContext &context) {
  return new Llvm_backend(context, nullptr, nullptr);
}
