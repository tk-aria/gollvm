//===-- go-llvm-materialize.cpp - Llvm_backend materalize* methods  -------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// Llvm_backend methods relating to materialization of llvm values.
//
//===----------------------------------------------------------------------===//

#include "go-llvm.h"
#include "go-c.h"
#include "go-system.h"
#include "go-llvm-cabi-oracle.h"
#include "go-llvm-irbuilders.h"
#include "gogo.h"

#include "llvm/IR/Constants.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/DIBuilder.h"
#include "llvm/IR/GlobalValue.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Value.h"

Bexpression *Llvm_backend::materializeIndirect(Bexpression *indExpr)
{
  Location location = indExpr->location();
  Btype *btype = indExpr->btype();
  std::vector<Bexpression *> iexprs =
      nbuilder_.extractChildenAndDestroy(indExpr);
  assert(iexprs.size() == 1);
  Bexpression *expr = iexprs[0];

  const VarContext *vc = nullptr;
  if (expr->varExprPending()) {
    vc = &expr->varContext();
    // handle *&x
    if (vc->addrLevel() != 0) {
      Bexpression *rval = nbuilder_.mkDeref(btype, expr->value(), expr,
                                            location);
      rval->setVarExprPending(vc->lvalue(), vc->addrLevel() - 1);
      return rval;
    }
  }

  std::string tag(expr->tag().size() == 0 ? "deref" : expr->tag());
  Bexpression *rval = genLoad(expr, btype, location, tag);
  if (vc) {
    if (rval->varExprPending())
      rval->resetVarExprContext();
    rval->setVarExprPending(expr->varContext());
  }

  return rval;
}

Bexpression *Llvm_backend::materializeAddress(Bexpression *addrExpr)
{
  Location location = addrExpr->location();
  std::vector<Bexpression *> aexprs =
      nbuilder_.extractChildenAndDestroy(addrExpr);
  Bexpression *bexpr = aexprs[0];
  assert(aexprs.size() == 1);
  assert(bexpr->value());

  // Gofrontend tends to take the address of things that are already
  // pointer-like to begin with (for example, C strings and and
  // arrays). This presents wrinkles here, since an array type
  // in LLVM is already effectively a pointer (you can feed it
  // directly into a GEP as opposed to having to take the address of
  // it first).  Bypass the effects of the address operator if
  // this is the case. This is hacky, maybe I can come up with a
  // better solution for this issue(?).
  if (llvm::isa<llvm::ConstantArray>(bexpr->value()))
    return bexpr;
  if (bexpr->value()->getType() == stringType()->type() &&
      llvm::isa<llvm::Constant>(bexpr->value()))
    return bexpr;

  // If the value we're trying to take the address of is a composite
  // constant, we have to spill it to memory here in order for us to
  // take its address.
  llvm::Value *val = bexpr->value();
  if (bexpr->isConstant()) {
    llvm::Constant *cval = llvm::cast<llvm::Constant>(val);
    Bvariable *cv = genVarForConstant(cval, bexpr->btype());
    val = cv->value();
  }

  // Create new expression with proper type.
  Btype *pt = pointer_type(bexpr->btype());
  Bexpression *rval = nbuilder_.mkAddress(pt, val, bexpr, location);
  std::string adtag(bexpr->tag());
  adtag += ".ad";
  rval->setTag(adtag);
  const VarContext &vc = bexpr->varContext();
  rval->setVarExprPending(vc.lvalue(), vc.addrLevel() + 1);

  return rval;
}

Bexpression *Llvm_backend::materializeConversion(Bexpression *convExpr)
{
  Location location = convExpr->location();
  Btype *type = convExpr->btype();
  std::vector<Bexpression *> iexprs =
      nbuilder_.extractChildenAndDestroy(convExpr);
  assert(iexprs.size() == 1);
  Bexpression *expr = iexprs[0];

  // For composite-init-pending values, materialize a variable now.
  if (expr->compositeInitPending()) {
    assert(!expr->varExprPending());
    expr = resolveCompositeInit(expr, nullptr);
  }

  llvm::Value *val = expr->value();
  assert(val);
  llvm::Type *valType = val->getType();
  llvm::Type *toType = type->type();

  // In the varexpr pending case, decide what to do depending on whether
  // the var is in an lvalue or rvalue context. For something like
  //
  //     var y int32
  //     z = int64(y)
  //
  // we want to force the load of "y" before converting to int64. For
  // an lvalue context, the conversion will be applied to the pointed-to-type
  // as well as the value type.
  if (expr->varExprPending()) {
    bool lvalue = expr->varContext().lvalue();
    if (!lvalue) {
      expr = resolveVarContext(expr);
      val = expr->value();
      valType = val->getType();
    }
    if (lvalue || useCopyForLoadStore(type->type())) {
      llvm::Type *pet = llvm::PointerType::get(expr->btype()->type(),
                                               addressSpace_);
      if (valType == pet)
        toType = llvm::PointerType::get(toType, addressSpace_);
    }
  }

  // If the we're converting between two different Btypes that have the
  // same underlying LLVM type, then we can create a new Bexpression
  // for the conversion but not do anything else.
  if (toType == valType)
    return nbuilder_.mkConversion(type, expr->value(), expr, location);

  LIRBuilder builder(context_, llvm::ConstantFolder());
  Bexpression *rval;

  // Pointer type to pointer-sized-integer type. Comes up when
  // converting function pointer to function descriptor (during
  // creation of function descriptor vals) or constant array to
  // uintptr (as part of GC symbol initializer creation), and in other
  // places in FE-generated code (ex: array index checks).
  if (valType->isPointerTy() && toType == llvmIntegerType()) {
    std::string tname(namegen("pticast"));
    llvm::Value *pticast = builder.CreatePtrToInt(val, toType, tname);
    rval = nbuilder_.mkConversion(type, pticast, expr, location);
  }

  // Pointer-sized-integer type pointer type. This comes up
  // in type hash/compare functions.
  if (toType->isPointerTy() && valType == llvmIntegerType()) {
    std::string tname(namegen("itpcast"));
    llvm::Value *itpcast = builder.CreateIntToPtr(val, toType, tname);
    rval = nbuilder_.mkConversion(type, itpcast, expr, location);
  }

  // For pointer conversions (ex: *int32 => *int64) create an
  // appropriate bitcast.
  if (valType->isPointerTy() && toType->isPointerTy()) {
    std::string tag(namegen("cast"));
    llvm::Value *bitcast = builder.CreateBitCast(val, toType, tag);
    rval = nbuilder_.mkConversion(type, bitcast, expr, location);
  }

  // Integer-to-integer conversions
  if (valType->isIntegerTy() && toType->isIntegerTy()) {
    llvm::IntegerType *valIntTyp =
        llvm::cast<llvm::IntegerType>(valType);
    llvm::IntegerType *toIntTyp =
        llvm::cast<llvm::IntegerType>(toType);
    unsigned valbits = valIntTyp->getBitWidth();
    unsigned tobits = toIntTyp->getBitWidth();
    llvm::Value *conv = nullptr;
    if (tobits > valbits) {
      if (expr->btype()->type() == llvmBoolType() ||
          expr->btype()->castToBIntegerType()->isUnsigned())
        conv = builder.CreateZExt(val, toType, namegen("zext"));
      else
        conv = builder.CreateSExt(val, toType, namegen("sext"));
    } else {
      conv = builder.CreateTrunc(val, toType, namegen("trunc"));
    }
    rval = nbuilder_.mkConversion(type, conv, expr, location);
  }

  // Float -> float conversions
  if (toType->isFloatingPointTy() && valType->isFloatingPointTy()) {
    llvm::Value *conv = nullptr;
    if (toType == llvmFloatType() && valType == llvmDoubleType())
      conv = builder.CreateFPTrunc(val, toType, namegen("fptrunc"));
    else if (toType == llvmDoubleType() && valType == llvmFloatType())
      conv = builder.CreateFPExt(val, toType, namegen("fpext"));
    else
      assert(0 && "unexpected float type");
    rval =  nbuilder_.mkConversion(type, conv, expr, location);
  }

  // Float -> integer conversions
  if (toType->isIntegerTy() && valType->isFloatingPointTy()) {
    llvm::Value *conv = nullptr;
    if (type->castToBIntegerType()->isUnsigned())
      conv = builder.CreateFPToUI(val, toType, namegen("ftoui"));
    else
      conv = builder.CreateFPToSI(val, toType, namegen("ftosi"));
    rval = nbuilder_.mkConversion(type, conv, expr, location);
  }

  // Integer -> float conversions
  if (toType->isFloatingPointTy() && valType->isIntegerTy()) {
    llvm::Value *conv = nullptr;
    if (expr->btype()->castToBIntegerType()->isUnsigned())
      conv = builder.CreateUIToFP(val, toType, namegen("uitof"));
    else
      conv = builder.CreateSIToFP(val, toType, namegen("sitof"));
    rval = nbuilder_.mkConversion(type, conv, expr, location);
  }

  if (!rval)
    // This case not handled yet. In particular code needs to be
    // written to handle signed/unsigned, conversions between scalars
    // of various sizes and types.
    assert(false && "this flavor of conversion not yet handled");

  // Propagate pending var context if we didn't resolve it here.
  // This may happen for composite values.
  if (expr->varExprPending())
    rval->setVarExprPending(expr->varContext());

  return rval;
}

llvm::Value *Llvm_backend::makePointerOffsetGEP(llvm::PointerType *llpt,
                                                llvm::Value *idxval,
                                                llvm::Value *sptr)
{
  LIRBuilder builder(context_, llvm::ConstantFolder());
  llvm::SmallVector<llvm::Value *, 1> elems(1);
  elems[0] = idxval;
  llvm::Value *val = builder.CreateGEP(llpt->getElementType(), sptr, elems, namegen("ptroff"));
  return val;
}

llvm::Value *Llvm_backend::makeArrayIndexGEP(llvm::ArrayType *llat,
                                             llvm::Value *idxval,
                                             llvm::Value *sptr)
{
  LIRBuilder builder(context_, llvm::ConstantFolder());
  llvm::SmallVector<llvm::Value *, 2> elems(2);
  elems[0] = llvm::ConstantInt::get(llvmInt32Type(), 0);
  elems[1] = idxval;
  llvm::Value *val = builder.CreateGEP(llat, sptr, elems, namegen("index"));
  return val;
}

llvm::Value *Llvm_backend::makeFieldGEP(llvm::StructType *llst,
                                        unsigned fieldIndex,
                                        llvm::Value *sptr)
{
  assert(sptr->getType()->isPointerTy());
  llvm::PointerType *srcTyp = llvm::cast<llvm::PointerType>(sptr->getType());
  assert(srcTyp->getElementType()->isStructTy());
  LIRBuilder builder(context_, llvm::ConstantFolder());
  assert(fieldIndex < llst->getNumElements());
  std::string tag(namegen("field"));

  // Allow function-pointer and function-descriptor mismatch
  llvm::Type *llpst = llvm::PointerType::get(llst, addressSpace_);
  std::set<llvm::Type *> visited;
  if (llpst != srcTyp && fcnPointerCompatible(llpst, srcTyp, visited))
    llst = llvm::cast<llvm::StructType>(srcTyp->getElementType());

  llvm::Value *val =
      builder.CreateConstInBoundsGEP2_32(llst, sptr, 0, fieldIndex, tag);
  return val;
}

Bexpression *Llvm_backend::materializeStructField(Bexpression *fieldExpr)
{
  Location location = fieldExpr->location();
  const std::string ftag(fieldExpr->tag());
  unsigned index = fieldExpr->fieldIndex();
  std::vector<Bexpression *> fexprs =
      nbuilder_.extractChildenAndDestroy(fieldExpr);
  assert(fexprs.size() == 1);
  Bexpression *bstruct = fexprs[0];

  if (bstruct->compositeInitPending())
    bstruct = resolveCompositeInit(bstruct, nullptr);

  // Construct an appropriate GEP
  llvm::Type *llt = bstruct->btype()->type();
  assert(llt->isStructTy());
  llvm::StructType *llst = llvm::cast<llvm::StructType>(llt);
  llvm::Value *sval = bstruct->value();
  llvm::Value *fval;
  if (bstruct->isConstant())
    fval = llvm::cast<llvm::Constant>(sval)->getAggregateElement(index);
  else
    fval = makeFieldGEP(llst, index, sval);
  Btype *bft = elementTypeByIndex(bstruct->btype(), index);

  // Wrap result in a Bexpression
  Bexpression *rval = nbuilder_.mkStructField(bft, fval, bstruct,
                                              index, location);

  if (bstruct->varExprPending())
    rval->setVarExprPending(bstruct->varContext());

  std::string tag(bstruct->tag());
  tag += (ftag.empty() ? ".field" : ftag);
  rval->setTag(tag);

  // We're done
  return rval;
}

Bexpression *Llvm_backend::materializeCompound(Bexpression *comExpr)
{
  Location location = comExpr->location();
  std::vector<Bnode *> kids = nbuilder_.extractChildNodesAndDestroy(comExpr);
  assert(kids.size() == 2);
  Bstatement *bstat = kids[0]->castToBstatement();
  assert(bstat);
  Bexpression *bexpr = kids[1]->castToBexpression();
  assert(bexpr);

  bexpr = materialize(bexpr);

  // Compound expressions can be used to produce lvalues, so we don't
  // want to call resolve() on bexpr here.
  Bexpression *rval = nbuilder_.mkCompound(bstat, bexpr, bexpr->value(),
                                           location);
  if (bexpr->varExprPending())
    rval->setVarExprPending(bexpr->varContext());
  return rval;
}

Bexpression *Llvm_backend::materializeConditional(Bexpression *condExpr)
{
  Bfunction *function = condExpr->getFunction();
  Location location = condExpr->location();
  Btype *btype = condExpr->btype();
  std::vector<Bexpression *> cexprs =
      nbuilder_.extractChildenAndDestroy(condExpr);
  assert(cexprs.size() == 2 || cexprs.size() == 3);
  Bexpression *condition = cexprs[0];
  Bexpression *then_expr = cexprs[1];
  Bexpression *else_expr = (cexprs.size() == 3 ? cexprs[2] : nullptr);

  condition = resolveVarContext(condition);
  then_expr = resolve(then_expr);
  if (else_expr)
    else_expr = resolve(else_expr);

  std::vector<Bvariable *> novars;
  Bblock *thenBlock = nbuilder_.mkBlock(function, novars, location);
  Bblock *elseBlock = nullptr;
  Bvariable *tempv = nullptr;

  // FIXME: add lifetime intrinsics for temp var below.
  Bstatement *thenStmt = nullptr;
  if (!btype || then_expr->btype() == void_type())
    thenStmt = expression_statement(function, then_expr);
  else
    tempv = temporary_variable(function, nullptr,
                               btype, then_expr, false,
                               location, &thenStmt);
  nbuilder_.addStatementToBlock(thenBlock, thenStmt);

  if (else_expr) {
    Bstatement *elseStmt = nullptr;
    elseBlock = nbuilder_.mkBlock(function, novars, location);
    if (!btype || btype == void_type() || else_expr->btype() == void_type()) {
      elseStmt = expression_statement(function, else_expr);
    } else {
      // Capture "else_expr" into temporary. Type needs to agree with
      // then_expr if then_expr had non-void type.
      if (!tempv) {
        tempv = temporary_variable(function, nullptr,
                                   btype, else_expr, false,
                                   location, &elseStmt);
      } else {
        // Ideally it would be nice to assert that the types are
        // identical for if_expr and else_expr, but particularly for
        // pointer types we need to allow for some disagreement (ex:
        // nil_pointer_expression, which is untyped/polymorphic).
        // Assume that the type checking in the call to
        // assignment_statement will catch any problems.
        Bexpression *varExpr = var_expression(tempv, VE_lvalue, location);
        elseStmt = assignment_statement(function, varExpr, else_expr, location);
      }
    }
    nbuilder_.addStatementToBlock(elseBlock, elseStmt);
  }

  // Wrap up and return the result
  Bstatement *ifStmt = if_statement(function, condition,
                                    thenBlock, elseBlock, location);

  Bexpression *rval = (tempv ?
                       var_expression(tempv, VE_rvalue, location) :
                       nbuilder_.mkVoidValue(void_type()));
  Bexpression *result =
      materialize(compound_expression(ifStmt, rval, location));
  return result;
}

Bexpression *Llvm_backend::materializeUnary(Bexpression *unExpr)
{
  Operator op = unExpr->op();
  Location location = unExpr->location();
  std::vector<Bexpression *> uexprs =
      nbuilder_.extractChildenAndDestroy(unExpr);
  assert(uexprs.size() == 1);
  Bexpression *expr = uexprs[0];

  expr = resolveVarContext(expr);
  Btype *bt = expr->btype();

  switch (op) {
    case OPERATOR_MINUS: {
      assert(false && "should have been expanded away");
      break;
    }

    case OPERATOR_NOT: {
      LIRBuilder builder(context_, llvm::ConstantFolder());
      assert(isBooleanType(bt));

      // FIXME: is this additional compare-to-zero needed? Or can we be certain
      // that the value in question has a single bit set?
      Bexpression *bzero = zero_expression(bt);
      llvm::Value *cmp =
          builder.CreateICmpNE(expr->value(), bzero->value(), namegen("icmp"));
      Btype *lbt = makeAuxType(llvmBoolType());
      Bexpression *cmpex =
          nbuilder_.mkBinaryOp(OPERATOR_EQEQ, lbt, cmp, bzero, expr, location);
      llvm::Constant *one = llvm::ConstantInt::get(llvmBoolType(), 1);
      llvm::Value *xorex = builder.CreateXor(cmp, one, namegen("xor"));
      Bexpression *notex = nbuilder_.mkUnaryOp(op, lbt, xorex, cmpex, location);
      Bexpression *tobool = lateConvert(bool_type(), notex, location);
      return tobool;
    }
    case OPERATOR_XOR: {
      // ^x    bitwise complement    is m ^ x  with m = "all bits set to 1"
      //                             for unsigned x and  m = -1 for signed x
      assert(bt->type()->isIntegerTy());
      LIRBuilder builder(context_, llvm::ConstantFolder());
      llvm::Value *onesval = llvm::Constant::getAllOnesValue(bt->type());
      llvm::Value *xorExpr = builder.CreateXor(expr->value(), onesval,
                                               namegen("xor"));
      Bexpression *rval = nbuilder_.mkUnaryOp(op, bt, xorExpr, expr, location);
      return rval;
      break;
    }
    default:
      assert(false && "unexpected unary opcode");
  }
  return nullptr;
}

static llvm::CmpInst::Predicate compare_op_to_pred(Operator op,
                                                   llvm::Type *typ,
                                                   bool isUnsigned)
{
  bool isFloat = typ->isFloatingPointTy();

  if (isFloat) {
    switch (op) {
    case OPERATOR_EQEQ:
      return llvm::CmpInst::Predicate::FCMP_OEQ;
    case OPERATOR_NOTEQ:
      return llvm::CmpInst::Predicate::FCMP_ONE;
    case OPERATOR_LT:
      return llvm::CmpInst::Predicate::FCMP_OLT;
    case OPERATOR_LE:
      return llvm::CmpInst::Predicate::FCMP_OLE;
    case OPERATOR_GT:
      return llvm::CmpInst::Predicate::FCMP_OGT;
    case OPERATOR_GE:
      return llvm::CmpInst::Predicate::FCMP_OGE;
    default:
      break;
    }
  } else {
    switch (op) {
    case OPERATOR_EQEQ:
      return llvm::CmpInst::Predicate::ICMP_EQ;
    case OPERATOR_NOTEQ:
      return llvm::CmpInst::Predicate::ICMP_NE;
    case OPERATOR_LT:
      return (isUnsigned ? llvm::CmpInst::Predicate::ICMP_ULT
                         : llvm::CmpInst::Predicate::ICMP_SLT);
    case OPERATOR_LE:
      return (isUnsigned ? llvm::CmpInst::Predicate::ICMP_ULE
                         : llvm::CmpInst::Predicate::ICMP_SLE);
    case OPERATOR_GT:
      return (isUnsigned ? llvm::CmpInst::Predicate::ICMP_UGT
                         : llvm::CmpInst::Predicate::ICMP_SGT);
    case OPERATOR_GE:
      return (isUnsigned ? llvm::CmpInst::Predicate::ICMP_UGE
                         : llvm::CmpInst::Predicate::ICMP_SGE);
    default:
      break;
    }
  }
  assert(false);
  return llvm::CmpInst::BAD_ICMP_PREDICATE;
}

std::pair<llvm::Value *, llvm::Value *>
Llvm_backend::convertForBinary(Operator op,
                               Bexpression *left,
                               Bexpression *right)
{
  llvm::Value *leftVal = left->value();
  llvm::Value *rightVal = right->value();
  std::pair<llvm::Value *, llvm::Value *> rval =
      std::make_pair(leftVal, rightVal);

  llvm::Type *leftType = leftVal->getType();
  llvm::Type *rightType = rightVal->getType();
  if (leftType == rightType)
    return rval;

  // Case 1: nil op X
  if (llvm::isa<llvm::ConstantPointerNull>(leftVal) &&
      rightType->isPointerTy()) {
    BexprLIRBuilder builder(context_, left);
    std::string tag(namegen("cast"));
    llvm::Value *bitcast = builder.CreateBitCast(leftVal, rightType, tag);
    rval.first = bitcast;
    return rval;
  }

  // Case 2: X op nil
  if (llvm::isa<llvm::ConstantPointerNull>(rightVal) &&
      leftType->isPointerTy()) {
    BexprLIRBuilder builder(context_, right);
    std::string tag(namegen("cast"));
    llvm::Value *bitcast = builder.CreateBitCast(rightVal, leftType, tag);
    rval.second = bitcast;
    return rval;
  }

  // Case 3: shift with different sized operands (ex: int64(v) << uint8(3)).
  // Promote or demote shift amount operand to match width of left operand.
  if ((op == OPERATOR_LSHIFT || op == OPERATOR_RSHIFT) &&
      leftType != rightType) {
    BexprLIRBuilder builder(context_, right);
    llvm::IntegerType *leftITyp = llvm::cast<llvm::IntegerType>(leftType);
    llvm::IntegerType *rightITyp = llvm::cast<llvm::IntegerType>(rightType);
    llvm::Value *conv = nullptr;
    if (leftITyp->getBitWidth() > rightITyp->getBitWidth())
      conv = builder.CreateZExt(rightVal, leftType, namegen("zext"));
    else
      conv = builder.CreateTrunc(rightVal, leftType, namegen("trunc"));
    rval.second = conv;
    return rval;
  }

  // Case 4: circular pointer type (ex: type T *T; var p, q T; p == &q)
  if (leftType->isPointerTy() && rightType->isPointerTy()) {
    BPointerType *lbpt = left->btype()->castToBPointerType();
    Btype *lctypconv = circularTypeAddrConversion(lbpt->toType());
    if (lctypconv != nullptr) {
      std::string tag(namegen("cast"));
      BexprLIRBuilder builder(context_, left);
      llvm::Value *bitcast = builder.CreateBitCast(leftVal, lctypconv->type(), tag);
      rval.first = bitcast;
    }

    BPointerType *rbpt = right->btype()->castToBPointerType();
    Btype *rctypconv = circularTypeAddrConversion(rbpt->toType());
    if (rctypconv != nullptr) {
      std::string tag(namegen("cast"));
      BexprLIRBuilder builder(context_, right);
      llvm::Value *bitcast = builder.CreateBitCast(rightVal, rctypconv->type(), tag);
      rval.second = bitcast;
    }
  }

  return rval;
}

Bexpression *Llvm_backend::materializeBinary(Bexpression *binExpr)
{
  Operator op = binExpr->op();
  Location location = binExpr->location();
  std::vector<Bexpression *> bexprs =
      nbuilder_.extractChildenAndDestroy(binExpr);
  assert(bexprs.size() == 2);
  Bexpression *left = bexprs[0];
  Bexpression *right = bexprs[1];

  Btype *bltype = left->btype();
  Btype *brtype = right->btype();

  left = resolveVarContext(left);
  right = resolveVarContext(right);
  assert(left->value() && right->value());

  std::pair<llvm::Value *, llvm::Value *> converted =
      convertForBinary(op, left, right);
  llvm::Value *leftVal = converted.first;
  llvm::Value *rightVal = converted.second;
  llvm::Type *ltype = leftVal->getType();
  llvm::Type *rtype = rightVal->getType();
  assert(ltype == rtype);
  BIntegerType *blitype = bltype->castToBIntegerType();
  BIntegerType *britype = brtype->castToBIntegerType();
  assert((blitype == nullptr) == (britype == nullptr));
  bool isUnsigned = false;
  if (blitype) {
    if (op == OPERATOR_LSHIFT || op == OPERATOR_RSHIFT)
      assert(britype->isUnsigned());
    else
      assert(blitype->isUnsigned() == britype->isUnsigned());
    isUnsigned = blitype->isUnsigned();
  }
  LIRBuilder builder(context_, llvm::ConstantFolder());
  llvm::Value *val = nullptr;

  switch (op) {
  case OPERATOR_EQEQ:
  case OPERATOR_NOTEQ:
  case OPERATOR_LT:
  case OPERATOR_LE:
  case OPERATOR_GT:
  case OPERATOR_GE: {
    llvm::CmpInst::Predicate pred = compare_op_to_pred(op, ltype, isUnsigned);
    if (ltype->isFloatingPointTy())
      val = builder.CreateFCmp(pred, leftVal, rightVal, namegen("fcmp"));
    else
      val = builder.CreateICmp(pred, leftVal, rightVal, namegen("icmp"));
    Btype *bcmpt = makeAuxType(llvmBoolType());
    // gen compare...
    Bexpression *cmpex =
        nbuilder_.mkBinaryOp(op, bcmpt, val, left, right, location);
    // ... widen to go boolean type
    return lateConvert(bool_type(), cmpex, location);
  }
  case OPERATOR_MINUS: {
    if (ltype->isFloatingPointTy())
      val = builder.CreateFSub(leftVal, rightVal, namegen("fsub"));
    else
      val = builder.CreateSub(leftVal, rightVal, namegen("sub"));
    break;
  }
  case OPERATOR_PLUS: {
    if (ltype->isFloatingPointTy())
      val = builder.CreateFAdd(leftVal, rightVal, namegen("fadd"));
    else
      val = builder.CreateAdd(leftVal, rightVal, namegen("add"));
    break;
  }
  case OPERATOR_MULT: {
    if (ltype->isFloatingPointTy())
      val = builder.CreateFMul(leftVal, rightVal, namegen("fmul"));
    else
      val = builder.CreateMul(leftVal, rightVal, namegen("mul"));
    break;
  }
  case OPERATOR_MOD: {
    assert(! ltype->isFloatingPointTy());
    if (isUnsigned)
      val = builder.CreateURem(leftVal, rightVal, namegen("mod"));
    else
      val = builder.CreateSRem(leftVal, rightVal, namegen("mod"));
    break;
  }
  case OPERATOR_DIV: {
    if (ltype->isFloatingPointTy())
      val = builder.CreateFDiv(leftVal, rightVal, namegen("fdiv"));
    else if (isUnsigned)
      val = builder.CreateUDiv(leftVal, rightVal, namegen("div"));
    else
      val = builder.CreateSDiv(leftVal, rightVal, namegen("div"));
    break;
  }
  case OPERATOR_OROR:
    // Note that the FE will have already expanded out || in a control
    // flow context (short circuiting)

    // fall through...

  case OPERATOR_OR: {
    assert(!ltype->isFloatingPointTy());
    val = builder.CreateOr(leftVal, rightVal, namegen("ior"));
    break;
  }
  case OPERATOR_BITCLEAR:
    // Note that the FE already inserted a complement op to RHS. So
    // this is effectively an AND expression.
    // fall through...
  case OPERATOR_ANDAND:
    // Note that the FE will have already expanded out && in a control
    // flow context (short circuiting).

    // fall through...

  case OPERATOR_AND: {
    assert(!ltype->isFloatingPointTy());
    val = builder.CreateAnd(leftVal, rightVal, namegen("iand"));
    break;
  }
  case OPERATOR_XOR: {
    assert(!ltype->isFloatingPointTy() && !rtype->isFloatingPointTy());
    val = builder.CreateXor(leftVal, rightVal, namegen("xor"));
    break;
  }
  case OPERATOR_LSHIFT: {
    // Note that the FE already inserted conditionals for checking
    // large shift amounts. So this can simply lower to a shift
    // instruction.
    assert(!ltype->isFloatingPointTy() && !rtype->isFloatingPointTy());
    val = builder.CreateShl(leftVal, rightVal, namegen("shl"));
    break;
  }
  case OPERATOR_RSHIFT: {
    // Note that the FE already inserted conditionals for checking
    // large shift amounts. So this can simply lower to a shift
    // instruction.
    assert(!ltype->isFloatingPointTy() && !rtype->isFloatingPointTy());
    if (isUnsigned)
      val = builder.CreateLShr(leftVal, rightVal, namegen("shr"));
    else
      val = builder.CreateAShr(leftVal, rightVal, namegen("shr"));
    break;
  }
  default:
    std::cerr << "Op " << op << " unhandled\n";
    assert(false);
  }

  return nbuilder_.mkBinaryOp(op, bltype, val, left, right, location);
}

Bexpression *Llvm_backend::materializeComposite(Bexpression *comExpr)
{
  Location location = comExpr->location();
  Btype *btype = comExpr->btype();
  const std::vector<unsigned long> *indexes = nbuilder_.getIndices(comExpr);
  std::vector<Bexpression *> vals =
      nbuilder_.extractChildenAndDestroy(comExpr);

  llvm::Type *llt = btype->type();
  unsigned numElements = 0;
  assert(llt->isStructTy() || llt->isArrayTy());
  llvm::CompositeType *llct = nullptr;
  if (llt->isStructTy()) {
    llvm::StructType *llst = llvm::cast<llvm::StructType>(llt);
    numElements = llst->getNumElements();
    assert(vals.size() == numElements);
    llct = llst;
  } else {
    llvm::ArrayType *llat = llvm::cast<llvm::ArrayType>(llt);
    numElements = llat->getNumElements();
    llct = llat;
  }

  // Constant values?
  bool isConstant = valuesAreConstant(vals);
  if (isConstant)
    return makeConstCompositeExpr(btype, llct, numElements,
                                  indexes, vals, location);
  else
    return makeDelayedCompositeExpr(btype, llct, numElements,
                                    indexes, vals, location);
}

Bexpression *
Llvm_backend::makeDelayedCompositeExpr(Btype *btype,
                                       llvm::CompositeType *llct,
                                       unsigned numElements,
                                       const std::vector<unsigned long> *indexes,
                                       const std::vector<Bexpression *> &vals,
                                       Location location)
{
  std::vector<Bexpression *> init_vals(numElements);
  if (indexes) {
    unsigned long nvals = vals.size();
    unsigned long nindxs = indexes->size();
    std::set<unsigned long> touched;
    for (unsigned ii = 0; ii < nindxs; ++ii) {
      auto idx = (*indexes)[ii];
      if (numElements != nvals)
        touched.insert(idx);
      init_vals[idx] = vals[ii];
    }
    if (numElements != nvals) {
      for (unsigned long ii = 0; ii < numElements; ++ii) {
        Btype *bElemTyp = elementTypeByIndex(btype, ii);
        if (touched.find(ii) == touched.end())
          init_vals[ii] = zero_expression(bElemTyp);
      }
    }
  } else {
    init_vals = vals;
  }

  // Here the NULL value signals that we want to delay full instantiation
  // of this constant expression until we can identify the storage for it.
  llvm::Value *nilval = nullptr;
  Binstructions noInstructions;
  Bexpression *ccon = nbuilder_.mkComposite(btype, nilval, init_vals,
                                            noInstructions, location);
  return ccon;
}

Bexpression *
Llvm_backend::makeConstCompositeExpr(Btype *btype,
                                     llvm::CompositeType *llct,
                                     unsigned numElements,
                                     const std::vector<unsigned long> *indexes,
                                     const std::vector<Bexpression *> &vals,
                                     Location location)
{
  llvm::SmallVector<llvm::Constant *, 64> llvals(numElements);
  unsigned long nvals = vals.size();

  if (indexes) {
    std::set<unsigned long> touched;
    unsigned long nindxs = indexes->size();
    for (unsigned ii = 0; ii < nindxs; ++ii) {
      auto idx = (*indexes)[ii];
      if (numElements != nvals)
        touched.insert(idx);
      Bexpression *bex = vals[ii];
      llvm::Constant *con = llvm::cast<llvm::Constant>(bex->value());
      llvm::Type *elt = llct->getTypeAtIndex(ii);
      if (elt != con->getType())
        con = llvm::ConstantExpr::getBitCast(con, elt);
      llvals[idx] = con;
    }
    if (numElements != nvals) {
      for (unsigned long ii = 0; ii < numElements; ++ii) {
        if (touched.find(ii) == touched.end()) {
          llvm::Type *elt = llct->getTypeAtIndex(ii);
          llvals[ii] = llvm::Constant::getNullValue(elt);
        }
      }
    }
  } else {
    for (unsigned long ii = 0; ii < numElements; ++ii) {
      llvm::Constant *con = llvm::cast<llvm::Constant>(vals[ii]->value());
      llvm::Type *elt = llct->getTypeAtIndex(ii);
      if (elt != con->getType())
        con = llvm::ConstantExpr::getBitCast(con, elt);
      llvals[ii] = con;
    }
   }

  llvm::Value *scon;
  if (llct->isStructTy()) {
    llvm::StructType *llst = llvm::cast<llvm::StructType>(llct);
    scon = llvm::ConstantStruct::get(llst, llvals);
  } else {
    llvm::ArrayType *llat = llvm::cast<llvm::ArrayType>(llct);
    scon = llvm::ConstantArray::get(llat, llvals);
  }

  Binstructions noInstructions;
  Bexpression *bcon = nbuilder_.mkComposite(btype, scon, vals,
                                            noInstructions, location);
  return makeGlobalExpression(bcon, scon, btype, location);
}

Bexpression *Llvm_backend::materializePointerOffset(Bexpression *ptroffExpr)
{
  Location location = ptroffExpr->location();
  std::vector<Bexpression *> cexprs =
      nbuilder_.extractChildenAndDestroy(ptroffExpr);
  assert(cexprs.size() == 2);
  Bexpression *base = cexprs[0];
  Bexpression *index = cexprs[1];

  // Resolve index expression
  index = resolveVarContext(index);

  // When a pointer offset expression appears in a left-hand-side (assignment)
  // context, the expected semantics are that location to be written is the
  // one pointer to by the result of the pointer offset, meaning that we want
  // to propagate any "lvalue-ness" found in 'base' up into the result
  // expression (as opposed to delaying a load from 'base' itself).
  //
  // To achieve this effect, the code below essentially hides away the
  // lvalue context on 'base' and then re-establishes it on 'rval' after the
  // GEP. This is a painful hack, it would be nice to have a clean way
  // to do this.
  VarContext vc;
  bool setLHS = false;
  if (base->varExprPending() && base->varContext().lvalue()) {
    setLHS = true;
    base->resetVarExprContext();
    base->setVarExprPending(false, 0);
  }
  base = resolveVarContext(base);

  // Construct an appropriate GEP
  llvm::PointerType *llpt =
      llvm::cast<llvm::PointerType>(base->btype()->type());
  llvm::Value *gep =
      makePointerOffsetGEP(llpt, index->value(), base->value());

  // Wrap in a Bexpression
  Bexpression *rval = nbuilder_.mkPointerOffset(base->btype(), gep, base,
                                                index, location);

  // Re-establish lvalue context (as described above)
  if (setLHS)
    rval->setVarExprPending(true, 1);

  std::string tag(base->tag());
  tag += ".ptroff";
  rval->setTag(tag);

  // We're done
  return rval;
}

Bexpression *Llvm_backend::materializeArrayIndex(Bexpression *arindExpr)
{
  Location location = arindExpr->location();
  std::vector<Bexpression *> cexprs =
      nbuilder_.extractChildenAndDestroy(arindExpr);
  assert(cexprs.size() == 2);
  Bexpression *barray = cexprs[0];
  Bexpression *index = cexprs[1];

  if (barray->compositeInitPending())
    barray = resolveCompositeInit(barray, nullptr);

  index = resolveVarContext(index);

  // Construct an appropriate GEP
  llvm::ArrayType *llat =
      llvm::cast<llvm::ArrayType>(barray->btype()->type());
  llvm::Value *aval = barray->value();
  llvm::Value *ival = index->value();
  llvm::Value *eval = nullptr;
  bool pending = false;
  if (barray->isConstant()) {
    if (index->isConstant())
      eval = llvm::cast<llvm::Constant>(aval)->getAggregateElement(llvm::cast<llvm::Constant>(ival));
    else {
      // Constant array with non-constant index. Put the array
      // into a temp var and load from there.
      llvm::Constant *cval = llvm::cast<llvm::Constant>(aval);
      Bvariable *cv = genVarForConstant(cval, barray->btype());
      aval = cv->value();
      pending = true;
    }
  }
  if (!eval)
    eval = makeArrayIndexGEP(llat, ival, aval);

  Btype *bet = elementTypeByIndex(barray->btype(), 0);

  // Wrap in a Bexpression
  Bexpression *rval = nbuilder_.mkArrayIndex(bet, eval, barray, index, location);
  if (pending)
    rval->setVarExprPending(false, 0);
  if (barray->varExprPending())
    rval->setVarExprPending(barray->varContext());

  std::string tag(barray->tag());
  tag += ".index";
  rval->setTag(tag);

  // We're done
  return rval;
}

struct GenCallState {
  CABIOracle oracle;
  Binstructions instructions;
  BinstructionsLIRBuilder builder;
  std::vector<Bexpression *> resolvedArgs;
  llvm::SmallVector<llvm::Value *, 16> llargs;
  llvm::Value *chainVal;
  llvm::Value *sretTemp;
  BFunctionType *calleeFcnType;
  Bfunction *callerFcn;

  GenCallState(llvm::LLVMContext &context,
               Bfunction *callerFunc,
               BFunctionType *calleeFcnTyp,
               TypeManager *tm)
      : oracle(calleeFcnTyp, tm),
        instructions(),
        builder(context, &instructions),
        chainVal(nullptr),
        sretTemp(nullptr),
        calleeFcnType(calleeFcnTyp),
        callerFcn(callerFunc) { }
};

static bool needSretTemp(const CABIParamInfo &returnInfo,
                         BFunctionType *calleeFcnTyp)
{
  if (returnInfo.disp() == ParmIgnore)
    return false;
  if (returnInfo.disp() == ParmIndirect)
    return true;
  if (returnInfo.abiType()->isAggregateType())
    return true;
  if (calleeFcnTyp->resultType()->type()->isAggregateType())
    return true;
  return false;
}

void Llvm_backend::genCallProlog(GenCallState &state)
{
  const CABIParamInfo &returnInfo = state.oracle.returnInfo();
  if (needSretTemp(returnInfo, state.calleeFcnType)) {
    assert(state.sretTemp == nullptr);
    std::string tname(namegen("sret.actual"));
    Btype *resTyp = state.calleeFcnType->resultType();
    assert(state.callerFcn);
    state.sretTemp = state.callerFcn->createTemporary(resTyp, tname);
    if (returnInfo.disp() == ParmIndirect)
      state.llargs.push_back(state.sretTemp);
  }

  // Chain param if needed
  const CABIParamInfo &chainInfo = state.oracle.chainInfo();
  if (chainInfo.disp() != ParmIgnore) {
    assert(chainInfo.disp() == ParmDirect);
    llvm::Value *cval = state.chainVal;
    if (cval == nullptr)
      cval = llvm::UndefValue::get(llvmPtrType());
    state.llargs.push_back(cval);
  }
}

void
Llvm_backend::genCallMarshallArgs(const std::vector<Bexpression *> &fn_args,
                                  GenCallState &state)
{
  const std::vector<Btype *> &paramTypes = state.calleeFcnType->paramTypes();
  for (unsigned idx = 0; idx < fn_args.size(); ++idx) {
    const CABIParamInfo &paramInfo = state.oracle.paramInfo(idx);

    if (paramInfo.attr() == AttrNest)
      continue;

    // For arguments not passed by value, no call to resolveVarContext
    // (we want var address, not var value).
    if (paramInfo.disp() == ParmIndirect) {
      Bexpression *fnarg = fn_args[idx];
      if (fnarg->compositeInitPending())
        fnarg = resolveCompositeInit(fnarg, nullptr);
      state.resolvedArgs.push_back(fnarg);
      llvm::Value *val = fnarg->value();
      assert(val);
      // spill a constant arg to memory if needed
      if (fnarg->isConstant()) {
        llvm::Constant *cval = llvm::cast<llvm::Constant>(val);
        Bvariable *cv = genVarForConstant(cval, fn_args[idx]->btype());
        val = cv->value();
      }
      assert(val->getType()->isPointerTy());
      state.llargs.push_back(val);
      continue;
    }

    // Resolve argument
    Varexpr_context ctx = varContextDisp(fn_args[idx]);
    if (paramInfo.abiTypes().size() == 2)
      ctx = VE_lvalue;
    Bexpression *resarg = resolve(fn_args[idx], ctx);
    state.resolvedArgs.push_back(resarg);

    if (paramInfo.disp() == ParmIgnore)
      continue;

    // At this point we're passing an argument directly,
    // as opposed to in memory.
    assert(paramInfo.disp() == ParmDirect);

    llvm::Value *val = resarg->value();

    BinstructionsLIRBuilder &builder = state.builder;
    if (paramInfo.abiTypes().size() == 1) {
      if (ctx == VE_lvalue) {
        // Passing single-eightbyte struct or array directly.
        if (resarg->isConstant()) {
          // If the value we're passing is a composite constant, we have to
          // spill it to memory here in order for the casts below to work.
          llvm::Constant *cval = llvm::cast<llvm::Constant>(val);
          Bvariable *cv = genVarForConstant(cval, resarg->btype());
          val = cv->value();
        }
        std::string castname(namegen("cast"));
        llvm::Type *ptv = makeLLVMPointerType(paramInfo.abiType());
        llvm::Value *bitcast = builder.CreateBitCast(val, ptv, castname);
        std::string ltag(namegen("ld"));
        llvm::Value *ld = builder.CreateLoad(bitcast, ltag);
        state.llargs.push_back(ld);
        continue;
      }
      // Passing a single 8-byte-or-less argument.

      // Apply any necessary pointer type conversions.
      if (val->getType()->isPointerTy() && ctx == VE_rvalue) {
        llvm::FunctionType *llft =
            llvm::cast<llvm::FunctionType>(state.calleeFcnType->type());
        llvm::Type *paramTyp = llft->getParamType(paramInfo.sigOffset());
        val = convertForAssignment(resarg, paramTyp);
      }

      // Apply any necessary sign-extensions or zero-extensions.
      if (paramInfo.abiType()->isIntegerTy()) {
        if (paramInfo.attr() == AttrZext)
          val = builder.CreateZExt(val, paramInfo.abiType(), namegen("zext"));
        else if (paramInfo.attr() == AttrSext)
          val = builder.CreateZExt(val, paramInfo.abiType(), namegen("sext"));
      }
      state.llargs.push_back(val);
      continue;
    }

    // This now corresponds to the case of passing the contents of
    // a small structure via two pieces / params.
    assert(paramInfo.abiTypes().size() == 2);
    assert(paramInfo.attr() == AttrNone);
    assert(ctx == VE_lvalue);

    // Create a struct type of the appropriate shape
    llvm::Type *llst = paramInfo.computeABIStructType(typeManager());
    llvm::Type *ptst = makeLLVMPointerType(llst);

    // If the value we're passing is a composite constant, we have to
    // spill it to memory here in order for the casts below to work.
    // Note that the spill is not needed if the value corresponds to a
    // delated load of a composite variable (in which case it will
    // already be an address). For example, if resarg corresponds to
    // an anonymous constant value like [2]float64{10.0,10.0} then we
    // need to spill, whereas if we're dealing with a reference to a
    // named global constant, there is no need to spill.
    if (resarg->isConstant() && val->getType() == resarg->btype()->type()) {
      llvm::Constant *cval = llvm::cast<llvm::Constant>(val);
      Bvariable *cv = genVarForConstant(cval, resarg->btype());
      val = cv->value();
    }

    // Cast the value to the struct type
    std::string tag(namegen("cast"));
    llvm::Value *bitcast = builder.CreateBitCast(val, ptst, tag);

    // Load up each field
    std::string ftag0(namegen("field0"));
    llvm::Value *field0gep =
        builder.CreateConstInBoundsGEP2_32(llst, bitcast, 0, 0, ftag0);
    std::string ltag0(namegen("ld"));
    llvm::Value *ld0 = builder.CreateLoad(field0gep, ltag0);
    state.llargs.push_back(ld0);

    std::string ftag1(namegen("field1"));
    llvm::Value *field1gep =
        builder.CreateConstInBoundsGEP2_32(llst, bitcast, 0, 1, ftag1);
    std::string ltag1(namegen("ld"));
    llvm::Value *ld1 = builder.CreateLoad(field1gep, ltag1);
    state.llargs.push_back(ld1);
  }
}

void Llvm_backend::genCallAttributes(GenCallState &state, llvm::CallInst *call)
{
  // Sret attribute if needed
  const CABIParamInfo &returnInfo = state.oracle.returnInfo();
  if (returnInfo.disp() == ParmIndirect)
    call->addAttribute(1, llvm::Attribute::StructRet);

  // Nest attribute if needed
  const CABIParamInfo &chainInfo = state.oracle.chainInfo();
  if (chainInfo.disp() != ParmIgnore)
    call->addAttribute(chainInfo.sigOffset()+1, llvm::Attribute::Nest);

  // Remainder of param attributes
  const std::vector<Btype *> &paramTypes = state.calleeFcnType->paramTypes();
  for (unsigned idx = 0; idx < paramTypes.size(); ++idx) {
    const CABIParamInfo &paramInfo = state.oracle.paramInfo(idx);
    if (paramInfo.disp() == ParmIgnore)
      continue;
    assert(paramInfo.attr() != AttrNest);
    assert(paramInfo.attr() != AttrStructReturn);
    unsigned off = paramInfo.sigOffset() + 1;
    if (paramInfo.attr() == AttrByVal)
      call->addAttribute(off, llvm::Attribute::ByVal);
    else if (paramInfo.attr() == AttrZext)
      call->addAttribute(off, llvm::Attribute::ZExt);
    else if (paramInfo.attr() == AttrSext)
      call->addAttribute(off, llvm::Attribute::SExt);
  }
}

void Llvm_backend::genCallEpilog(GenCallState &state,
                                 llvm::Instruction *callInst,
                                 Bexpression *callExpr)
{
  const CABIParamInfo &returnInfo = state.oracle.returnInfo();


  if (needSretTemp(returnInfo, state.calleeFcnType)) {
    assert(state.sretTemp);
    assert(callExpr->value() == state.sretTemp);
    callExpr->setVarExprPending(VE_rvalue, 0);

    if (returnInfo.disp() == ParmDirect) {
      // The call is returning something by value that doesn't match
      // the expected abstract result type of the function. Cast the
      // sret storage location to a pointer to the abi type and store
      // the ABI return value into it.
      BinstructionsLIRBuilder builder(context_, callExpr);
      llvm::Type *rt = (returnInfo.abiTypes().size() == 1 ?
                        returnInfo.abiType()  :
                        returnInfo.computeABIStructType(typeManager()));
      llvm::Type *ptrt = makeLLVMPointerType(rt);
      std::string castname(namegen("cast"));
      llvm::Value *bitcast = builder.CreateBitCast(state.sretTemp,
                                                   ptrt, castname);
      std::string stname(namegen("st"));
      builder.CreateStore(callInst, bitcast);
    }
  }
}

Bexpression *Llvm_backend::materializeCall(Bexpression *callExpr)
{
  Location location = callExpr->location();
  Bfunction *caller = callExpr->getFunction();
  std::vector<Bexpression *> cexprs =
      nbuilder_.extractChildenAndDestroy(callExpr);
  Bexpression *fn_expr = cexprs[0];
  Bexpression *chain_expr = cexprs[1];
  std::vector<Bexpression *> fn_args;
  for (unsigned idx = 2; idx < cexprs.size(); ++idx) {
    fn_args.push_back(cexprs[idx]);
  }

  // Resolve fcn. Expect pointer-to-function type here.
  fn_expr = resolveVarContext(fn_expr);
  assert(fn_expr->btype()->type()->isPointerTy());
  Btype *rbtype = functionReturnType(fn_expr->btype());

  // Collect function type and param types
  BPointerType *bpft = fn_expr->btype()->castToBPointerType();
  BFunctionType *calleeFcnTyp = bpft->toType()->castToBFunctionType();
  assert(calleeFcnTyp);

  GenCallState state(context_, caller, calleeFcnTyp, typeManager());

  // Static chain expression if applicable
  if (chain_expr->btype() != void_type()) {
    chain_expr = resolveVarContext(chain_expr);
    assert(chain_expr->btype()->type()->isPointerTy());
    Btype *bpt = makeAuxType(llvmPtrType());
    chain_expr = lateConvert(bpt, chain_expr, location);
    assert(chain_expr->value() != nullptr);
    state.chainVal = chain_expr->value();
  }

  // Set up for call (including creation of return tmp if needed)
  genCallProlog(state);

  // Unpack / resolve / marshall arguments
  genCallMarshallArgs(fn_args, state);

  // Create the actual call instruction
  llvm::FunctionType *llft =
      llvm::cast<llvm::FunctionType>(calleeFcnTyp->type());
  bool isvoid = llft->getReturnType()->isVoidTy();
  std::string callname(isvoid ? "" : namegen("call"));
  llvm::CallInst *call =
      state.builder.CreateCall(llft, fn_expr->value(),
                               state.llargs, callname);
  genCallAttributes(state, call);

  llvm::Value *callValue = (state.sretTemp ? state.sretTemp : call);
  Bexpression *rval =
      nbuilder_.mkCall(rbtype, callValue, caller, fn_expr, chain_expr,
                       state.resolvedArgs, state.instructions, location);
  state.instructions.clear();

  genCallEpilog(state, call, rval);

  return rval;
}

llvm::Value *
Llvm_backend::convertForAssignment(Bexpression *src,
                                   llvm::Type *dstToType)
{
  if (src->value()->getType() == dstToType)
    return src->value();

  llvm::Function *dummyFcn = errorFunction_->function();
  BlockLIRBuilder builder(dummyFcn, this);
  llvm::Value *val = convertForAssignment(src->btype(), src->value(),
                                          dstToType, &builder);
  src->appendInstructions(builder.instructions());
  return val;
}

llvm::Value *
Llvm_backend::convertForAssignment(Btype *srcBType,
                                   llvm::Value *srcVal,
                                   llvm::Type *dstToType,
                                   BlockLIRBuilder *builder)
{
  llvm::Type *srcType = srcBType->type();

  if (dstToType == srcType)
    return srcVal;

  // Case 1: handle discrepancies between representations of function
  // descriptors. All front end function descriptor types are structs
  // with a single field, however this field can sometimes be a pointer
  // to function, and sometimes it can be of uintptr type.
  bool srcPtrToFD = isPtrToFuncDescriptorType(srcType);
  bool dstPtrToFD = isPtrToFuncDescriptorType(dstToType);
  if (srcPtrToFD && dstPtrToFD) {
    std::string tag(namegen("cast"));
    llvm::Value *bitcast = builder->CreateBitCast(srcVal, dstToType, tag);
    return bitcast;
  }

  // Case 2: handle circular function types.
  bool dstCircFunc = isCircularFunctionType(dstToType);
  bool srcFuncPtr = isPtrToFuncType(srcType);
  if ((srcPtrToFD || srcFuncPtr) && dstCircFunc) {
    std::string tag(namegen("cast"));
    llvm::Value *bitcast = builder->CreateBitCast(srcVal, dstToType, tag);
    return bitcast;
  }

  // Case 3: handle raw function pointer assignment (frontend will
  // sometimes take a function pointer and assign it to "void *" without
  // an explicit conversion).
  bool dstPtrToVoid = isPtrToVoidType(dstToType);
  if (dstPtrToVoid && srcFuncPtr) {
    std::string tag(namegen("cast"));
    llvm::Value *bitcast = builder->CreateBitCast(srcVal, dstToType, tag);
    return bitcast;
  }

  // Case 4: in some cases when calling a function (for example, __gogo)
  // the front end will pass a raw function vale in place of a function
  // descriptor. Allow this case.
  if (dstPtrToFD && srcFuncPtr) {
    std::string tag(namegen("cast"));
    llvm::Value *bitcast = builder->CreateBitCast(srcVal, dstToType, tag);
    return bitcast;
  }

  // Case 5: handle polymorphic nil pointer expressions-- these are
  // generated without a type initially, so we need to convert them
  // to the appropriate type if they appear in an assignment context.
  if (srcVal == nil_pointer_expression()->value()) {
    std::string tag(namegen("cast"));
    llvm::Value *bitcast = builder->CreateBitCast(srcVal, dstToType, tag);
    return bitcast;
  }

  // Case 6: the code that creates interface values stores pointers of
  // various flavors into i8*. Ideally it would be better to insert
  // forced type conversions in the FE, but for now we allow this case.
  bool srcPtrToIface = isPtrToIfaceStructType(srcType);
  if (dstPtrToVoid && srcPtrToIface) {
    std::string tag(namegen("cast"));
    llvm::Value *bitcast = builder->CreateBitCast(srcVal, dstToType, tag);
    return bitcast;
  }

  // Case 7: when creating slice values it's common for the frontend
  // to mix pointers and arrays, e.g. assign "[3 x i64]*" to "i64**".
  // Allow this sort of conversion.
  llvm::Type *elt =
      (dstToType->isPointerTy() ?
       llvm::cast<llvm::PointerType>(dstToType)->getElementType() : nullptr);
  if (elt && isPtrToArrayOf(srcType, elt)) {
    std::string tag(namegen("cast"));
    llvm::Value *bitcast = builder->CreateBitCast(srcVal, dstToType, tag);
    return bitcast;
  }

  // Case 8: also when creating slice values it's common for the
  // frontend to assign pointer-to-X to unsafe.Pointer (and vice versa)
  // without an explicit cast. Allow this for now.
  if ((dstToType == llvmPtrType() && llvm::isa<llvm::PointerType>(srcType)) ||
      (srcType == llvmPtrType() && llvm::isa<llvm::PointerType>(dstToType))) {
    std::string tag(namegen("cast"));
    llvm::Value *bitcast = builder->CreateBitCast(srcVal, dstToType, tag);
    return bitcast;
  }

  // Case 9: related to case 1 above, interface value expressions can
  // contain C function pointers stored as "void *" instead of
  // concrete pointer-to-function values. For example, consider a
  // value V1 of the form
  //
  //       { { T1*, void* }, O* }
  //
  // where T1 is a type descriptor and O is an object; this value can
  // sometimes be assigned to a location of type
  //
  //       { { T1*, F* }, O* }
  //
  // where F is a concrete C pointer-to-function (as opposed to "void*").
  // Allow conversions of this sort for now.
  std::set<llvm::Type *> visited;
  if (fcnPointerCompatible(dstToType, srcType, visited)) {
    std::string tag(namegen("cast"));
    llvm::Value *bitcast = builder->CreateBitCast(srcVal, dstToType, tag);
    return bitcast;
  }

  // Case 10: circular pointer type (ex: type T *T; var p T; p = &p)
  BPointerType *srcbpt = srcBType->castToBPointerType();
  if (srcbpt) {
    Btype *ctypconv = circularTypeAddrConversion(srcbpt->toType());
    if (ctypconv != nullptr && ctypconv->type() == dstToType) {
      std::string tag(namegen("cast"));
      llvm::Value *bitcast = builder->CreateBitCast(srcVal, dstToType, tag);
      return bitcast;
    }
  }

  return srcVal;
}

// Walk the specified expression and invoke setVarExprPending on
// each var expression, with correct lvalue/rvalue tag depending on
// context.

class VarContextVisitor {
 public:
  VarContextVisitor(Bexpression *top,
                    Varexpr_context lvalueContext,
                    bool dumpDiffs)
      : dumpDiffs_(dumpDiffs)
  {
    if (lvalueContext == VE_lvalue && isMem(top))
      setLvalue(top);
  }

  std::pair< std::pair<VisitDisp, VisitChildDisp>, Bnode *>
  visitChildPre(Bnode *parent, Bnode *child) {

    Bexpression *eparent = parent->castToBexpression();
    assert(eparent != nullptr);

    // Don't descend into stmts or non-var nodes with values.
    Bexpression *echild = child->castToBexpression();
    if (child->isStmt() || (echild->value() != nullptr &&
                            echild->flavor() != N_Var))
      return std::make_pair(std::make_pair(ContinueWalk, SkipChild), child);

    // Propagate lvalue property down from parent to child through a memory
    // operation such as a fieldref or arrayindex if applicable. For example,
    // for the tree build from go code "x.f1[y]" such as
    //
    //             array_index
    //             /      \.
    //        field      var(y)
    //        /
    //      var(x)
    //
    // If the top node (array_index) is in a left-hand-side position,
    // then we want to flag "field" and "var(x)" nodes as being in an
    // LHS context also, but not the "var(y)" node (it is not being
    // assigned).
    assert(echild != nullptr);
    if (isLvalue(eparent) && isMem(echild)) {
      Bexpression *cmem = memArg(eparent);
      assert(cmem != nullptr);
      if (cmem == echild)
        setLvalue(echild);
    }
    return std::make_pair(std::make_pair(ContinueWalk, VisitChild), child);
  }

  // Apply "var pending" tags to var exprs bottom up, once downward propagation
  // of lvalue context is complete.
  std::pair<VisitDisp, Bnode *> visitNodePost(Bnode *node)
  {
    Bexpression *expr = node->castToBexpression();
    if (expr == nullptr || (expr->value() && expr->flavor() != N_Var))
      return std::make_pair(ContinueWalk, expr);

    if (expr->flavor() == N_Var)
      setVarExprPending(expr, isLvalue(expr), 0);

    // Debugging only. This is intended to provide a way to compare the
    // tags applied by the frontend vs the tags this visitor generates.
    if (dumpDiffs_)
      dumpDiff(expr);

    auto it = varcontext_.find(expr);
    if (it != varcontext_.end()) {
      VarContext vc(it->second);
      bool lvalue = vc.lvalue();
      if (expr->varExprPending()) {
        assert(vc.equal(expr->varContext()));
      } else {
        expr->setVarExprPending(lvalue, 0);
      }
    }

    return std::make_pair(ContinueWalk, expr);
  }

  // boilerplate
  std::pair<VisitDisp, Bnode *> visitNodePre(Bnode *node) {
    return std::make_pair(ContinueWalk, node);
  }

  // boilerplate
  std::pair<VisitDisp, Bnode *> visitChildPost(Bnode *parent, Bnode *child) {
      return std::make_pair(ContinueWalk, child);
  }

 private:

  // Debugging only.
  void dumpDiff(Bexpression *expr)
  {
    if (expr->value() && expr->flavor() != N_Var)
      return;

    VarContext oldvc;
    if (expr->varExprPending())
      oldvc = expr->varContext();

    VarContext newvc;
    auto nit = varcontext_.find(expr);
    if (nit != varcontext_.end())
      newvc = nit->second;

    if (newvc.equal(oldvc))
      return;

    std::cerr << "Expr " << expr->id()
              << " " << expr->flavstr() << " ";
    if (oldvc.pending()) {
      std::cerr << "old VC(" << (oldvc.pending() ? "true" : "false")
                << "," << (oldvc.lvalue() ? "lval" : "rval")
                << "," << oldvc.addrLevel() << ") ";
    }
    if (newvc.pending()) {
      std::cerr << "new VC(" << (newvc.pending() ? "true" : "false")
                << "," << (newvc.lvalue() ? "lval" : "rval")
                << "," << newvc.addrLevel() << ") ";
    }
    std::cerr << "\n";
    expr->dump();
    std::cerr << "\n";
  }

  Bexpression *memArg(Bexpression *expr) {
    const std::vector<Bnode *> &kids = expr->children();
    switch(expr->flavor()) {
      case N_StructField:
      case N_ArrayIndex:
      case N_Address:
      case N_Deref:
      case N_Conversion:
      case N_PointerOffset:
        return kids[0]->castToBexpression();
      default:
        return nullptr;
    }
  }

  bool isMem(Bexpression *expr) {
    switch(expr->flavor()) {
      case N_Var:
      case N_StructField:
      case N_Address:
      case N_Deref:
      case N_Conversion:
      case N_ArrayIndex:
      case N_PointerOffset:
        return true;
      default:
        return false;
    }
  }

  void setLvalue(Bexpression *expr) {
    assert(lvalue_.find(expr) == lvalue_.end());
    lvalue_.insert(expr);
  }

  bool isLvalue(Bexpression *expr) {
    return lvalue_.find(expr) != lvalue_.end();
  }

  void setVarExprPending(Bexpression *expr, bool lvalue, unsigned addrLevel) {
    assert(expr);
    VarContext vc(lvalue, addrLevel);
    varcontext_[expr] = vc;
  }

 private:
  std::set<Bnode *> lvalue_;
  std::map<Bexpression *, VarContext> varcontext_;
  bool dumpDiffs_;
};

// Helper visitor class for expression folding; removes redundant
// nodes in preparation for materialization.

class FoldVisitor {
 public:
  FoldVisitor(Llvm_backend *be) : be_(be) { }

  std::pair<VisitDisp, Bnode *> visitNodePost(Bnode *node) {
    Bexpression *expr = node->castToBexpression();
    if (!expr)
      return std::make_pair(ContinueWalk, node);
    if (expr && expr->value())
      return std::make_pair(ContinueWalk, node);

    // Fold addr(deref(X)) => X
    if (node->flavor() == N_Address) {
      std::vector<Bexpression *> akids = expr->getChildExprs();
      if (akids[0]->flavor() == N_Deref) {
        Bexpression *deref = akids[0];

        // Extract children and delete first the addr node, then the
        // deref node. Order is important; if we delete the deref first
        // then the integrity visitor will wind up trying to access the
        // deleted deref.
        be_->nodeBuilder().extractChildenAndDestroy(expr);
        std::vector<Bexpression *> dkids =
            be_->nodeBuilder().extractChildenAndDestroy(deref);

        // Return result
        expr = dkids[0];
      }
    }
    return std::make_pair(ContinueWalk, expr);
  }

  // If child is a statement (ex: compound expr) or if child
  // already has an LLVM value, then prune walk at this node.
  std::pair< std::pair<VisitDisp, VisitChildDisp>, Bnode *>
  visitChildPre(Bnode *parent, Bnode *child) {
    Bexpression *echild = child->castToBexpression();
    if (child->isStmt() || echild->value() != nullptr)
      return std::make_pair(std::make_pair(ContinueWalk, SkipChild), child);
    return std::make_pair(std::make_pair(ContinueWalk, VisitChild), child);
  }

  // Boilerplate
  std::pair<VisitDisp, Bnode *> visitNodePre(Bnode *node) {
    return std::make_pair(ContinueWalk, node);
  }
  std::pair<VisitDisp, Bnode *> visitChildPost(Bnode *parent, Bnode *child) {
    return std::make_pair(ContinueWalk, child);
  }

 private:
  Llvm_backend *be_;
};

class MaterializeVisitor {
 public:
  MaterializeVisitor(Llvm_backend *be) : be_(be) { }

  std::pair<VisitDisp, Bnode *> visitNodePre(Bnode *node) {
    return std::make_pair(ContinueWalk, node);
  }
  std::pair<VisitDisp, Bnode *> visitNodePost(Bnode *node) {
    Bexpression *expr = node->castToBexpression();
    if (expr && expr->value())
      return std::make_pair(ContinueWalk, node);
    switch(node->flavor()) {
      case N_EmptyStmt:
      case N_LabelStmt:
      case N_GotoStmt:
      case N_ExprStmt:
      case N_ReturnStmt:
      case N_DeferStmt:
      case N_IfStmt:
      case N_ExcepStmt:
      case N_BlockStmt:
      case N_SwitchStmt: {
        return std::make_pair(ContinueWalk, node);
      }
      case N_Error:
      case N_Const:
      case N_Var:
      case N_FcnAddress:
      case N_LabelAddress: {
        break;
      }
      case N_Conversion: {
        expr = be_->materializeConversion(expr);
        break;
      }
      case N_Deref: {
        expr = be_->materializeIndirect(expr);
        break;
      }
      case N_Address: {
        expr = be_->materializeAddress(expr);
        break;
      }
      case N_UnaryOp: {
        expr = be_->materializeUnary(expr);
        break;
      }
      case N_StructField: {
        expr = be_->materializeStructField(expr);
        break;
      }
      case N_BinaryOp: {
        expr = be_->materializeBinary(expr);
        break;
      }
      case N_Compound: {
        expr = be_->materializeCompound(expr);
        break;
      }
      case N_ArrayIndex: {
        expr = be_->materializeArrayIndex(expr);
        break;
      }
      case N_PointerOffset: {
        expr = be_->materializePointerOffset(expr);
        break;
      }
      case N_Composite: {
        expr = be_->materializeComposite(expr);
        break;
      }
      case N_Call: {
        expr = be_->materializeCall(expr);
        break;
      }
      case N_Conditional: {
        expr = be_->materializeConditional(expr);
        break;
      }
    }
    assert(expr->value() != nullptr ||
           expr->flavor() == N_Composite ||
           expr->btype() == be_->void_type());
    return std::make_pair(ContinueWalk, expr);
  }

  // If child is a statement (ex: compound expr), then no need to
  // visit subtree (should already be materialized). Similarly if
  // child already has an LLVM value, then prune walk at this node
  std::pair< std::pair<VisitDisp, VisitChildDisp>, Bnode *>
  visitChildPre(Bnode *parent, Bnode *child) {
    Bexpression *echild = child->castToBexpression();
    if (child->isStmt() || echild->value() != nullptr)
      return std::make_pair(std::make_pair(ContinueWalk, SkipChild), child);
    return std::make_pair(std::make_pair(ContinueWalk, VisitChild), child);
  }

  std::pair<VisitDisp, Bnode *> visitChildPost(Bnode *parent, Bnode *child) {
    return std::make_pair(ContinueWalk, child);
  }
 private:
  Llvm_backend *be_;
};

Bexpression *Llvm_backend::materialize(Bexpression *expr,
                                       Varexpr_context lvalueContext)

{
  // Repair any node sharing at this point-- the materializer
  // assumes that any node it visits can be destroyed/replaced
  // without impacting some other portion of the tree.
  enforceTreeIntegrity(expr);

  // TODO:
  // - don't really need a treewalk in the non-LHS case to apply
  //   can context tags; it would be simpler to only do the walk
  //   in the LHS case (and just apply var context tags when
  //   var expression is initially created)
  // - an extension of the above: pass in the LHS/RHS context
  //   and propagate recursively during the materialize() walk,
  //   instead of having a separate tree-walk here.

  // Locate and tag var expressions within the tree, selecting LHS or
  // RHS context as appropriate. Needs to be done after the call above
  // so as to insure that there are no share var expressions.
  VarContextVisitor vcvis(expr, lvalueContext, false);
  update_walk_nodes(expr, vcvis);

  // Perform some basic folding operations. This is easier to do
  // here so as not to worry about sharing.
  FoldVisitor fvis(this);
  Bnode *folded = update_walk_nodes(expr, fvis);
  expr = folded->castToBexpression();

  // Walk to materialize llvm values.
  MaterializeVisitor mvis(this);
  Bnode *materialized = update_walk_nodes(expr, mvis);
  return materialized->castToBexpression();
}

Bexpression *Llvm_backend::lateConvert(Btype *type,
                                       Bexpression *expr,
                                       Location loc)
{
  return materialize(convert_expression(type, expr, loc));
}
