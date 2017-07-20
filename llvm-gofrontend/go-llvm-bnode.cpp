//===-- go-llvm-bnode.cpp - implementation of 'Bnode' class ---------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// Methods for class Bnode and related classes.
//
//===----------------------------------------------------------------------===//

#include "go-llvm-btype.h"
#include "go-llvm-bnode.h"
#include "go-llvm-bvariable.h"
#include "go-llvm-bexpression.h"
#include "go-llvm-bstatement.h"
#include "go-llvm-tree-integrity.h"
#include "go-system.h"

#include "llvm/Support/raw_ostream.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Value.h"

// Table of Bnode properties

enum StmtDisp : unsigned char { IsStmt, IsExpr };

struct BnodePropVals {
  const char *str;
  unsigned numChildren;
  StmtDisp stmt;
};

static constexpr unsigned Variadic = 0xffffffff;

static BnodePropVals BnodeProperties[] = {

  /* N_Error */         {  "error", 0, IsExpr },
  /* N_Const */         {  "const", 0, IsExpr },
  /* N_Var */           {  "var", 0, IsExpr },
  /* N_FcnAddress */    {  "fcn", 0, IsExpr },
  /* N_LabelAddress */  {  "labelad", 0, IsExpr },
  /* N_Conversion */    {  "conv", 1, IsExpr },
  /* N_Deref */         {  "deref", 1, IsExpr },
  /* N_Address */       {  "addr", 1, IsExpr },
  /* N_UnaryOp */       {  "unary", 1, IsExpr },
  /* N_StructField */   {  "field", 1, IsExpr },
  /* N_BinaryOp */      {  "binary", 2, IsExpr },
  /* N_Compound */      {  "compound", 2, IsExpr },
  /* N_ArrayIndex */    {  "arindex", 2, IsExpr },
  /* N_PointerOffset */ {  "ptroff", 2, IsExpr },
  /* N_Composite */     {  "composite", Variadic, IsExpr },
  /* N_Call */          {  "call", Variadic, IsExpr },
  /* N_Conditional */   {  "conditional", Variadic, IsExpr },

  /* N_EmptyStmt */     {  "empty", 0, IsStmt },
  /* N_LabelStmt */     {  "label", 0, IsStmt },
  /* N_GotoStmt */      {  "goto", 0, IsStmt },
  /* N_ExprStmt */      {  "exprst", 1, IsStmt },
  /* N_ReturnStmt */    {  "return", 1, IsStmt },
  /* N_DeferStmt */     {  "defer", 2, IsStmt },
  /* N_IfStmt */        {  "ifstmt", 3, IsStmt },
  /* N_ExcepStmt */     {  "excepstmt", 3, IsStmt },
  /* N_BlockStmt */     {  "block", Variadic, IsStmt },
  /* N_SwitchStmt */    {  "switch", Variadic, IsStmt }
};

Bnode::Bnode(NodeFlavor flavor, const std::vector<Bnode *> &kids, Location loc)
    : kids_(kids)
    , location_(loc)
    , flavor_(flavor)
    , id_(0xfeedface)
    , flags_(0)
{
  memset(&u, '\0', sizeof(u));
  assert(BnodeProperties[flavor].numChildren == Variadic ||
         BnodeProperties[flavor].numChildren == kids.size());
}

Bnode::Bnode(const Bnode &src)
    : location_(src.location_)
    , flavor_(src.flavor_)
    , id_(0xfeedface)
    , flags_(0)
{
  assert(! isStmt());
  memcpy(&u, &src.u, sizeof(u));
}

Bexpression *Bnode::castToBexpression() const {
  if (isStmt())
    return nullptr;
  return const_cast<Bexpression*>(static_cast<const Bexpression*>(this));
}

Bstatement *Bnode::castToBstatement() const {
  if (!isStmt())
    return nullptr;
  return const_cast<Bstatement*>(static_cast<const Bstatement*>(this));
}

Bblock *Bnode::castToBblock() const {
  if (flavor() != N_BlockStmt)
    return nullptr;
  return const_cast<Bblock*>(static_cast<const Bblock*>(this));
}

void Bnode::replaceChild(unsigned idx, Bnode *newchild)
{
  assert(idx < kids_.size());
  kids_[idx] = newchild;
}

static const char *opToString(Operator op)
{
  switch(op) {
    case OPERATOR_INVALID: return "<invalid>";
    case OPERATOR_OROR: return "||";    // ||
    case OPERATOR_ANDAND: return "&&";  // &&
    case OPERATOR_EQEQ: return "==";    // ==
    case OPERATOR_NOTEQ: return "!=";   // !=
    case OPERATOR_LT: return "<";       // <
    case OPERATOR_LE: return "<=";      // <=
    case OPERATOR_GT: return ">";       // >
    case OPERATOR_GE: return ">=";      // >=
    case OPERATOR_PLUS: return "+";     // +
    case OPERATOR_MINUS: return "-";    // -
    case OPERATOR_OR: return "|";       // |
    case OPERATOR_XOR: return "^";      // ^
    case OPERATOR_MULT: return "*";     // *
    case OPERATOR_DIV: return "/";      // /
    case OPERATOR_MOD: return "%";      // %
    case OPERATOR_LSHIFT: return "<<";  // <<
    case OPERATOR_RSHIFT: return ">>";  // >>
    case OPERATOR_AND: return "&";      // &
    case OPERATOR_NOT: return "!";      // !
    case OPERATOR_EQ: return "=";       // =
    case OPERATOR_BITCLEAR: return "&^";// &^
    default: break;
  }
  assert(false && "operator unhandled");
  return "";
}

const char *Bnode::flavstr() const {
  if (flavor() == N_UnaryOp || flavor() == N_BinaryOp)
    return opToString(u.op);
  return BnodeProperties[flavor()].str;
}

static void indent(llvm::raw_ostream &os, unsigned ilevel) {
  for (unsigned i = 0; i < ilevel; ++i)
    os << " ";
}

void Bnode::dump()
{
  std::string s;
  llvm::raw_string_ostream os(s);
  osdump(os, 0, nullptr, false);
  std::cerr << os.str();
}

void Bnode::osdump(llvm::raw_ostream &os, unsigned ilevel,
                   Llvm_linemap *linemap, bool terse)
{
  if (! terse) {
    if (linemap) {
      indent(os, ilevel);
      os << linemap->to_string(location()) << "\n";
      linemap = nullptr; // just want top-level src, not src for all kids
    }
  }

  // Basic description of node
  indent(os, ilevel);
  os << flavstr() << ": ";

  // Additional info
  const Bexpression *expr = castToBexpression();
  if (expr) {
    // expression type
    os << "[ btype: ";
    expr->btype()->osdump(os, 0);
    os << " ] ";
  }
  switch(flavor()) {
    case N_Error: {
      os << "\n";
      return;
    }
    case N_Const: {
      assert(expr);
      if (expr->value())
        expr->value()->print(os);
      else
        os << "<nil const>";
      os << "\n";
      return;
    }
    case N_Composite: {
      assert(expr);
      if (expr->value() && llvm::isa<llvm::Constant>(expr->value())) {
        expr->value()->print(os);
        os << "\n";
        return;
      }
      break;
    }
    case N_FcnAddress: {
      assert(expr);
      if (llvm::isa<llvm::Function>(expr->value())) {
        llvm::Function *f = llvm::cast<llvm::Function>(expr->value());
        os << f->getName() << "\n";
        return;
      }
      break;
    }
    case N_LabelAddress: {
      assert(expr);
      os << "label_addr " << u.label->label();
      break;
    }
    case N_Var: {
      os << "'" << u.var->name() << "' type: ";
      u.var->btype()->osdump(os, 0);
      break;
    }
    case N_StructField: {
      os << "field " << u.fieldIndex;
      break;
    }
    case N_BlockStmt: {
      Bblock *block = castToBblock();
      if (block->vars().size()) {
        os << " { ";
        unsigned tmps = 0;
        for (auto &v : block->vars()) {
          if (v->isTemporary()) {
            tmps += 1;
            continue;
          }
          os << v->name() << " ";
        }
        os << "} ";
        if (tmps)
          os << tmps << " tmps";
      }
      break;
    }
    case N_LabelStmt:
    case N_GotoStmt: {
      os << "label " << u.label->label();
      break;
    }
    default: break;
  }
  if (!terse && expr && expr->varExprPending()) {
    const VarContext &vc = expr->varContext();
    os << " [VP lvalue=" <<  (vc.lvalue() ? "yes" : "no")
       << " addrLevel=" << vc.addrLevel() << "]";
  }
  os << "\n";
  if (expr)
    expr->dumpInstructions(os, ilevel, linemap, terse);

  // Now children
  for (auto &kid : kids_)
    kid->osdump(os, ilevel + 2, linemap, terse);
}

void BnodeBuilder::destroy(Bnode *node, WhichDel which, bool recursive)
{
  std::set<Bnode *> visited;
  destroyRec(node, which, recursive, visited);
}

void BnodeBuilder::destroyRec(Bnode *node,
                              WhichDel which,
                              bool recursive,
                              std::set<Bnode *> &visited)
{
  if (visited.find(node) != visited.end())
    return;
  visited.insert(node);
  if (which != DelWrappers) {
    Bexpression *expr = node->castToBexpression();
    if (expr) {
      unsigned idx = 0;
      for (auto inst : expr->instructions()) {
        integrityVisitor_->unsetParent(inst, expr, idx);
        inst->dropAllReferences();
      }
      for (auto inst : expr->instructions())
        inst->deleteValue();
      expr->clear();
    }
  }
  if (recursive)
    for (auto &kid : node->kids_)
      destroyRec(kid, which, true, visited);
  if (which != DelInstructions)
    freeNode(node);
}

SwitchDescriptor *Bnode::getSwitchCases()
{
  assert(flavor() == N_SwitchStmt);
  assert(u.swcases);
  return u.swcases;
}

Blabel *Bnode::label() const
{
  assert(flavor() == N_LabelStmt || flavor() == N_GotoStmt ||
         flavor() == N_LabelAddress);
  return u.label;
}

Operator Bnode::op() const
{
  assert(flavor() == N_UnaryOp || flavor() == N_BinaryOp);
  return u.op;
}

Bvariable *Bnode::var() const
{
  assert(flavor() == N_Var);
  return u.var;
}

unsigned Bnode::fieldIndex() const
{
  assert(flavor() == N_StructField);
  return u.fieldIndex;
}

Bfunction *Bnode::getFunction() const
{
  assert(flavor() == N_FcnAddress ||
         flavor() == N_Call ||
         flavor() == N_Conditional);
  return u.func;
}

int Bnode::getIndices() const
{
  assert(flavor() == N_Composite);
  return u.indices;
}

//......................................................................

BnodeBuilder::BnodeBuilder(Llvm_backend *be)
    : integrityVisitor_(new IntegrityVisitor(be, TreeIntegCtl(DumpPointers, DontReportRepairableSharing, IncrementalMode)))
    , checkIntegrity_(true)
{
}

BnodeBuilder::~BnodeBuilder()
{
  freeStmts();
  for (auto &expr : earchive_) {
    if (expr)
      delete expr;
  }
  for (auto ivpair : tempvars_) {
    delete ivpair.first;
    delete ivpair.second;
  }
}

void BnodeBuilder::freeStmts()
{
  for (auto &stmt : sarchive_) {
    if (stmt)
      delete stmt;
  }
  sarchive_.clear();
  for (auto &c : swcases_)
    delete c;
  swcases_.clear();
}

void BnodeBuilder::freeNode(Bnode *node)
{
  assert(node);
  Bexpression *expr = node->castToBexpression();
  if (expr) {
    earchive_[expr->id()] = nullptr;
    if (expr->id() == earchive_.size()-1)
      earchive_.pop_back();
    delete expr;
  } else {
    Bstatement *stmt = node->castToBstatement();
    sarchive_[stmt->id()] = nullptr;
    if (stmt->id() == sarchive_.size()-1)
      sarchive_.pop_back();
    delete stmt;
  }
}

void BnodeBuilder::checkTreeInteg(Bnode *node)
{
  if (checkIntegrity_ && !integrityVisitor_->examine(node)) {
    std::cerr << integrityVisitor_->msg();
    assert(false);
  }
}

Bexpression *BnodeBuilder::archive(Bexpression *expr)
{
  expr->id_ = earchive_.size();
  earchive_.push_back(expr);
  checkTreeInteg(expr);
  return expr;
}

Bstatement *BnodeBuilder::archive(Bstatement *stmt)
{
  stmt->id_ = sarchive_.size();
  sarchive_.push_back(stmt);
  checkTreeInteg(stmt);
  return stmt;
}

Bblock *BnodeBuilder::archive(Bblock *bb)
{
  Bstatement *stmt = bb;
  return static_cast<Bblock*>(archive(stmt));
}

Bexpression *BnodeBuilder::mkError(Btype *errortype)
{
  std::vector<Bnode *> kids;
  Location loc;
  llvm::Value *noval = nullptr;
  return archive(new Bexpression(N_Error, kids, noval, errortype, loc));
}

Bexpression *BnodeBuilder::mkConst(Btype *btype, llvm::Value *value)
{
  assert(btype);
  assert(value);
  std::vector<Bnode *> kids;
  Location loc;
  return archive(new Bexpression(N_Const, kids, value, btype, loc));
}

Bexpression *BnodeBuilder::mkVoidValue(Btype *btype)
{
  assert(btype);
  std::vector<Bnode *> kids;
  Location loc;
  return archive(new Bexpression(N_Const, kids, nullptr, btype, loc));
}

Bexpression *BnodeBuilder::mkVar(Bvariable *var, Location loc)
{
  assert(var);
  Btype *vt = var->btype();
  std::vector<Bnode *> kids;
  Bexpression *rval =
      new Bexpression(N_Var, kids, var->value(), vt, loc);
  rval->u.var = var;
  return archive(rval);
}

Bvariable *BnodeBuilder::mkTempVar(Btype *varType,
                                   Location loc,
                                   const std::string &name)
{
  assert(varType);
  llvm::AllocaInst *inst = new llvm::AllocaInst(varType->type(), 0);
  if (! name.empty())
    inst->setName(name);
  Bvariable *tvar = new Bvariable(varType, loc, name, LocalVar, true, inst);
  tempvars_[inst] = tvar;
  tvar->markAsTemporary();
  return tvar;
}

Bvariable *BnodeBuilder::adoptTemporaryVariable(llvm::AllocaInst *alloca)
{
  assert(alloca);
  auto mit = tempvars_.find(alloca);
  if (mit == tempvars_.end())
    return nullptr;
  Bvariable *ret = mit->second;
  tempvars_.erase(mit);
  return ret;
}

// This is somewhat unpleasant but necessary due to the way LLVM's
// IRBuilder works. If you do something like
//
//    newinst = builder.CreateOr(left, right)
//
// and it turns out that 'right' is the constant zero, then the
// builder will just return left (meaning that we don't want the
// instruction to be appended to the new Bexpression).

void BnodeBuilder::appendInstIfNeeded(Bexpression *rval, llvm::Value *val)
{
  if (val == nullptr || !llvm::isa<llvm::Instruction>(val))
    return;
  llvm::Instruction *inst = llvm::cast<llvm::Instruction>(val);
  for (auto &kid : rval->kids_) {
    Bexpression *expr = kid->castToBexpression();
    if (expr && expr->value() == inst)
      return;
  }
  rval->appendInstruction(inst);
}

Bexpression *BnodeBuilder::mkBinaryOp(Operator op, Btype *typ, llvm::Value *val,
                                      Bexpression *left, Bexpression *right,
                                      Location loc)
{
  assert(left);
  assert(right);
  std::vector<Bnode *> kids = { left, right };
  Bexpression *rval =
      new Bexpression(N_BinaryOp, kids, val, typ, loc);
  if (val)
    appendInstIfNeeded(rval, val);
  rval->u.op = op;
  return archive(rval);
}

Bexpression *BnodeBuilder::mkBinaryOp(Operator op, Btype *typ, llvm::Value *val,
                                      Bexpression *left, Bexpression *right,
                                      Binstructions &instructions, Location loc)
{
  assert(left);
  assert(right);
  std::vector<Bnode *> kids = { left, right };
  Bexpression *rval =
      new Bexpression(N_BinaryOp, kids, val, typ, loc);
  for (auto &inst : instructions.instructions())
    rval->appendInstruction(inst);
  rval->u.op = op;
  return archive(rval);
}

Bexpression *BnodeBuilder::mkUnaryOp(Operator op, Btype *typ, llvm::Value *val,
                                     Bexpression *src, Location loc)
{
  assert(src);
  std::vector<Bnode *> kids = { src };
  Bexpression *rval =
      new Bexpression(N_UnaryOp, kids, val, typ, loc);
  rval->u.op = op;
  appendInstIfNeeded(rval, val);
  return archive(rval);
}

Bexpression *BnodeBuilder::mkConversion(Btype *typ, llvm::Value *val,
                                        Bexpression *src, Location loc)
{
  std::vector<Bnode *> kids = { src };
  Bexpression *rval =
      new Bexpression(N_Conversion, kids, val, typ, loc);
  appendInstIfNeeded(rval, val);
  return archive(rval);
}

Bexpression *BnodeBuilder::mkAddress(Btype *typ, llvm::Value *val,
                                     Bexpression *src, Location loc)
{
  std::vector<Bnode *> kids = { src };
  Bexpression *rval = new Bexpression(N_Address, kids, val, typ, loc);
  return archive(rval);
}

Bexpression *BnodeBuilder::mkFcnAddress(Btype *typ, llvm::Value *val,
                                        Bfunction *func, Location loc)
{
  std::vector<Bnode *> kids;
  Bexpression *rval = new Bexpression(N_FcnAddress, kids, val, typ, loc);
  rval->u.func = func;
  return archive(rval);
}

Bexpression *BnodeBuilder::mkLabelAddress(Btype *typ,
                                          llvm::Value *val,
                                          Blabel *label,
                                          Location loc)
{
  std::vector<Bnode *> kids;
  Bexpression *rval = new Bexpression(N_LabelAddress, kids, val, typ, loc);
  rval->u.label = label;
  return archive(rval);
}

Bexpression *BnodeBuilder::mkDeref(Btype *typ, llvm::Value *val,
                                   Bexpression *src, Location loc)
{
  std::vector<Bnode *> kids = { src };
  Bexpression *rval = new Bexpression(N_Deref, kids, val, typ, loc);
  return archive(rval);
}

Bexpression *
BnodeBuilder::mkIndexedComposite(Btype *btype,
                                 llvm::Value *value,
                                 const std::vector<Bexpression *> &vals,
                                  const std::vector<unsigned long> &indices,
                                 Binstructions &instructions,
                                 Location loc)
{
  std::vector<Bnode *> kids;
  for (auto &v : vals)
    kids.push_back(v);
  Bexpression *rval =
      new Bexpression(N_Composite, kids, value, btype, loc);
  for (auto &inst : instructions.instructions())
    rval->appendInstruction(inst);
  indexvecs_.push_back(indices);
  rval->u.indices = indexvecs_.size() - 1;
  return archive(rval);
}

Bexpression *
BnodeBuilder::mkComposite(Btype *btype,
                          llvm::Value *value,
                          const std::vector<Bexpression *> &vals,
                          Binstructions &instructions,
                          Location loc)
{
  std::vector<Bnode *> kids;
  for (auto &v : vals)
    kids.push_back(v);
  Bexpression *rval =
      new Bexpression(N_Composite, kids, value, btype, loc);
  for (auto &inst : instructions.instructions())
    rval->appendInstruction(inst);
  rval->u.indices = -1;
  return archive(rval);
}

Bexpression *BnodeBuilder::mkStructField(Btype *typ,
                                         llvm::Value *val,
                                         Bexpression *structval,
                                         unsigned fieldIndex,
                                         Location loc)
{
  std::vector<Bnode *> kids = { structval };
  Bexpression *rval =
      new Bexpression(N_StructField, kids, val, typ, loc);
  appendInstIfNeeded(rval, val);
  rval->u.fieldIndex = fieldIndex;
  return archive(rval);
}

Bexpression *BnodeBuilder::mkArrayIndex(Btype *typ,
                                        llvm::Value *val,
                                        Bexpression *arval,
                                        Bexpression *index,
                                        Location loc)
{
  std::vector<Bnode *> kids = { arval, index };
  Bexpression *rval =
      new Bexpression(N_ArrayIndex, kids, val, typ, loc);
  appendInstIfNeeded(rval, val);
  return archive(rval);
}

Bexpression *BnodeBuilder::mkPointerOffset(Btype *typ,
                                           llvm::Value *val,
                                           Bexpression *ptr,
                                           Bexpression *offset,
                                           Location loc)
{
  std::vector<Bnode *> kids = { ptr, offset };
  Bexpression *rval =
      new Bexpression(N_PointerOffset, kids, val, typ, loc);
  appendInstIfNeeded(rval, val);
  return archive(rval);
}

Bexpression *BnodeBuilder::mkCompound(Bstatement *st,
                                      Bexpression *expr,
                                      llvm::Value *val,
                                      Location loc)
{
  std::vector<Bnode *> kids = { st, expr };
  Bexpression *rval =
      new Bexpression(N_Compound, kids, val, expr->btype(), loc);
  rval->setTag(expr->tag());
  return archive(rval);
}

Bexpression *BnodeBuilder::mkCall(Btype *btype,
                                  llvm::Value *val,
                                  Bfunction *caller,
                                  Bexpression *fnExpr,
                                  Bexpression *chainExpr,
                                  const std::vector<Bexpression *> &args,
                                  Binstructions &instructions,
                                  Location loc)
{
  std::vector<Bnode *> kids;
  kids.push_back(fnExpr);
  kids.push_back(chainExpr);
  for (auto &a : args)
    kids.push_back(a);
  Bexpression *rval =
      new Bexpression(N_Call, kids, val, btype, loc);
  bool found = false;
  for (auto &inst : instructions.instructions()) {
    if (inst == val)
      found = true;
    rval->appendInstruction(inst);
  }
  if (!found && val && !llvm::isa<llvm::AllocaInst>(val)) {
    assert(llvm::isa<llvm::Instruction>(val));
    rval->appendInstruction(llvm::cast<llvm::Instruction>(val));
  }
  rval->u.func = caller;
  return archive(rval);
}

Bexpression *BnodeBuilder::mkConditional(Bfunction *function,
                                         Btype *btype,
                                         Bexpression *condition,
                                         Bexpression *then_expr,
                                         Bexpression *else_expr,
                                         Location loc)
{
  std::vector<Bnode *> kids;
  kids.push_back(condition);
  kids.push_back(then_expr);
  if (else_expr)
    kids.push_back(else_expr);
  Bexpression *rval =
      new Bexpression(N_Conditional, kids, nullptr, btype, loc);
  rval->u.func = function;
  return archive(rval);
}

Bstatement *BnodeBuilder::mkErrorStmt()
{
  assert(! errorStatement_.get());
  std::vector<Bnode *> kids;
  errorStatement_.reset(new Bstatement(N_Error, nullptr, kids, Location()));
  return errorStatement_.get();
}

Bstatement *BnodeBuilder::mkExprStmt(Bfunction *func,
                                     Bexpression *expr,
                                     Location loc)
{
  std::vector<Bnode *> kids = { expr };
  Bstatement *rval = new Bstatement(N_ExprStmt, func, kids, loc);
  return archive(rval);
}

Bstatement *BnodeBuilder::mkReturn(Bfunction *func,
                                   Bexpression *returnVal,
                                   Location loc)
{
  std::vector<Bnode *> kids = { returnVal };
  Bstatement *rval = new Bstatement(N_ReturnStmt, func, kids, loc);
  return archive(rval);
}

Bstatement *BnodeBuilder::mkLabelDefStmt(Bfunction *func,
                                         Blabel *label,
                                         Location loc)
{
  std::vector<Bnode *> kids;
  Bstatement *rval = new Bstatement(N_LabelStmt, func, kids, loc);
  rval->u.label = label;
  return archive(rval);
}

Bstatement *BnodeBuilder::mkGotoStmt(Bfunction *func,
                                     Blabel *label,
                                     Location loc)
{
  std::vector<Bnode *> kids;
  Bstatement *rval = new Bstatement(N_GotoStmt, func, kids, loc);
  rval->u.label = label;
  return archive(rval);
}

Bstatement *BnodeBuilder::mkIfStmt(Bfunction *func,
                                   Bexpression *cond, Bblock *trueBlock,
                                   Bblock *falseBlock, Location loc)
{
  if (falseBlock == nullptr)
    falseBlock = mkBlock(func, std::vector<Bvariable *>(), loc);
  std::vector<Bnode *> kids = { cond, trueBlock, falseBlock };
  Bstatement *rval = new Bstatement(N_IfStmt, func, kids, loc);
  return archive(rval);
}

Bstatement *BnodeBuilder::mkDeferStmt(Bfunction *func,
                                      Bexpression *undefer,
                                      Bexpression *defer,
                                      Location loc)
{
  assert(func);
  assert(undefer);
  assert(defer);
  std::vector<Bnode *> kids = { undefer, defer };
  Bstatement *rval = new Bstatement(N_DeferStmt, func, kids, loc);
  return archive(rval);
}

Bstatement *BnodeBuilder::mkExcepStmt(Bfunction *func,
                                      Bstatement *body,
                                      Bstatement *onexception,
                                      Bstatement *finally,
                                      Location loc)
{
  assert(body);
  assert(onexception);
  assert(finally);
  std::vector<Bnode *> kids = { body, onexception, finally };
  Bstatement *rval = new Bstatement(N_ExcepStmt, func, kids, loc);
  return archive(rval);
}

SwitchDescriptor::SwitchDescriptor(const std::vector<std::vector<Bexpression *> > &vals)
{
  // Determine child index of first stmt
  unsigned stidx = 1;
  for (auto &vvec : vals)
    stidx += vvec.size();
  // Construct case descriptors
  unsigned idx = 1;
  for (auto &vvec : vals) {
    cases_.push_back(SwitchCaseDesc(idx, vvec.size(), stidx++));
    idx += vvec.size();
  }
}

Bstatement *BnodeBuilder::mkSwitchStmt(Bfunction *func,
                                       Bexpression *swvalue,
                                       const std::vector<std::vector<Bexpression *> > &vals,
                                       const std::vector<Bstatement *> &stmts,
                                       Location loc)
{
  std::vector<Bnode *> kids = { swvalue };
  for (auto &vvec : vals)
    for (auto &v : vvec)
      kids.push_back(v);
  for (auto &st : stmts)
    kids.push_back(st);
  SwitchDescriptor *d = new SwitchDescriptor(vals);
  swcases_.push_back(d);
  Bstatement *rval = new Bstatement(N_SwitchStmt, func, kids, loc);
  rval->u.swcases = d;
  return archive(rval);

}



Bblock *BnodeBuilder::mkBlock(Bfunction *func,
                              const std::vector<Bvariable *> &vars,
                              Location loc)
{
  Bblock *rval = new Bblock(func, vars, loc);
  return archive(rval);
}

void BnodeBuilder::addStatementToBlock(Bblock *block, Bstatement *st)
{
  assert(block);
  assert(st);
  block->kids_.push_back(st);
}

Bexpression *
BnodeBuilder::cloneSub(Bexpression *expr,
                       std::map<llvm::Value *, llvm::Value *> &vm)
{
  assert(expr);
  std::vector<Bnode *> newChildren;
  for (auto &c : expr->children()) {
    assert(c);
    Bexpression *ce = c->castToBexpression();
    assert(ce);
    Bexpression *clc = cloneSubtree(ce);
    newChildren.push_back(clc);
  }
  Bexpression *res = new Bexpression(*expr);
  res->kids_ = newChildren;
  archive(res);

  llvm::Value *iv = expr->value();
  llvm::Value *newv = nullptr;
  for (auto inst : expr->instructions()) {
    if (inst == iv) {
      assert(! newv);
      newv = inst;
    }
    llvm::Instruction *icl = inst->clone();
    unsigned nops = inst->getNumOperands();
    for (unsigned idx = 0; idx < nops; ++idx) {
      llvm::Value *v = inst->getOperand(idx);
      auto it = vm.find(v);
      if (it != vm.end()) {
        llvm::Value *cv = it->second;
        icl->setOperand(idx, cv);
      }
    }
    icl->setName(inst->getName());
    res->appendInstruction(icl);
  }
  if (newv)
    res->setValue(newv);
  return res;
}

Bexpression *BnodeBuilder::cloneSubtree(Bexpression *expr) {
  std::map<llvm::Value *, llvm::Value *> vm;
  return cloneSub(expr, vm);
}

std::vector<Bexpression *>
BnodeBuilder::extractChildenAndDestroy(Bexpression *expr)
{
  assert(expr);
  assert(expr->value() == nullptr);
  assert(expr->instructions().empty());
  std::vector<Bexpression *> orphanExprs;
  std::vector<Bnode *> orphanNodes = extractChildNodesAndDestroy(expr);
  for (auto k : orphanNodes) {
    Bexpression *ekid = k->castToBexpression();
    assert(ekid);
    orphanExprs.push_back(ekid);
  }
  return orphanExprs;
}

std::vector<Bnode *>
BnodeBuilder::extractChildNodesAndDestroy(Bnode *node)
{
  std::vector<Bnode *> orphans;
  assert(node);
  for (unsigned idx = 0; idx < node->kids_.size(); ++idx) {
    Bnode *kid = node->kids_[idx];
    integrityVisitor_->unsetParent(kid, node, idx);
    orphans.push_back(kid);
  }
  integrityVisitor_->deletePending(node);
  Bexpression *expr = node->castToBexpression();
  assert(expr); // statements not yet supported, could be if needed
  freeNode(expr);
  return orphans;
}

const std::vector<unsigned long> *BnodeBuilder::getIndices(Bexpression *expr) const
{
  int i = expr->getIndices();
  if (i < 0)
    return nullptr;
  assert((unsigned)i < indexvecs_.size());
  return &indexvecs_[i];
}
