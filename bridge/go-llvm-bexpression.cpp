//===-- go-llvm-bexpression.cpp - implementation of 'Bexpression' class ---===//
//
// Copyright 2018 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
//
//===----------------------------------------------------------------------===//
//
// Methods for class Bexpression.
//
//===----------------------------------------------------------------------===//

#include "go-llvm-btype.h"
#include "go-llvm-bexpression.h"
#include "go-system.h"

#include "llvm/Support/raw_ostream.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Value.h"

static void indent(llvm::raw_ostream &os, unsigned ilevel) {
  for (unsigned i = 0; i < ilevel; ++i)
    os << " ";
}

bool Binstructions::isValidInst(llvm::Instruction *inst)
{
  if (llvm::isa<llvm::AllocaInst>(inst))
    return false;
  return true;
}

std::vector<llvm::Instruction *>
Binstructions::extractInstsAfter(llvm::Instruction *inst)
{
  auto it = std::find(instructions_.begin(), instructions_.end(), inst);
  assert(it != instructions_.end());
  std::vector<llvm::Instruction *> rval(it, instructions_.end());
  if (it != instructions_.end())
    instructions_.erase(it, instructions_.end());
  return rval;
}

Bexpression::Bexpression(NodeFlavor fl, const std::vector<Bnode *> &kids,
                         llvm::Value *val, Btype *typ, Location loc)
    : Bnode(fl, kids, loc)
    , value_(val)
    , btype_(typ)
{
}

Bexpression::Bexpression(const Bexpression &src)
    : Bnode(src)
    , Binstructions()
    , value_(src.value_)
    , btype_(src.btype_)
    , varContext_(src.varContext_)
{
}

Bexpression::~Bexpression()
{
}

bool Bexpression::varExprPending() const
{
  return varContext_.pending();
}

const VarContext &Bexpression::varContext() const
{
  return varContext_;
}

void Bexpression::setVarExprPending(bool lvalue, unsigned addrLevel)
{
  varContext_.setPending(lvalue, addrLevel);
}

void Bexpression::setVarExprPending(const VarContext &src)
{
  assert(src.pending());
  varContext_ = src;
}

void Bexpression::resetVarExprContext()
{
  varContext_.reset();
}

bool Bexpression::compositeInitPending() const
{
  return flavor() == N_Composite && value() == nullptr;
}

const std::vector<Bexpression *> Bexpression::getChildExprs() const
{
  std::vector<Bexpression *> rval;
  for (auto &k : children()) {
    Bexpression *e = k->castToBexpression();
    assert(e);
    rval.push_back(e);
  }
  return rval;
}

void Bexpression::setValue(llvm::Value *val)
{
  assert(val);
  value_ = val;
}

bool Bexpression::isConstant()
{
  if (!value_)
    return false;
  if (!llvm::isa<llvm::Constant>(value_))
    return false;

  // Not a constant if there is a pending load
  if (value_->getType()->isPointerTy() &&
      value_->getType()->getPointerElementType() ==
        btype()->type())
    return false;

  // In some cases, even the underlying value is an
  // llvm::Constant, the expression may be not. For
  // example, a var expression for a global variable,
  // its value is an llvm::GlobalVariable, which is an
  // llvm::Constant, but this is generally not what
  // we want. Extra checking is needed.
  switch (flavor()) {
  case N_Var:
    return false;
  case N_Deref:
    return false;
  case N_Conversion: {
    std::vector<Bexpression*> kids = getChildExprs();
    assert(kids.size() == 1);
    return kids[0]->isConstant();
  }
  case N_StructField: {
    std::vector<Bexpression*> kids = getChildExprs();
    assert(kids.size() == 1);
    return kids[0]->isConstant();
  }
  case N_ArrayIndex: {
    std::vector<Bexpression*> kids = getChildExprs();
    assert(kids.size() == 2);
    return kids[0]->isConstant() && kids[1]->isConstant();
  }
  default:
    return true;
  }
}

void Bexpression::srcDump(Llvm_linemap *linemap)
{
  std::string s;
  llvm::raw_string_ostream os(s);
  osdump(os, 0, linemap, false);
  std::cerr << os.str();
}

void Bexpression::dumpInstructions(llvm::raw_ostream &os, unsigned ilevel,
                                   Llvm_linemap *linemap, bool terse) const {
  bool hitValue = false;
  for (auto inst : instructions()) {
    indent(os, ilevel);
    char c = ' ';
    if (inst == value()) {
      c = '*';
      hitValue = true;
    }
    if (! terse)
      os << c;
    inst->print(os);
    os << "\n";
  }
  if (!terse && !hitValue) {
    indent(os, ilevel);
    if (value())
      value()->print(os);
    else
      os << "<nil value>";
    os << "\n";
  }
}
