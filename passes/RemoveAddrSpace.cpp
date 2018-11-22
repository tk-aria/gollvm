//===--- GoStackMap.cpp ---------------------------------------------------===//
//
// Copyright 2018 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
//
//===----------------------------------------------------------------------===//
//
// LLVM backend pass to remove addrspacecast instructions
// in static initializers (because codegen cannot handle
// them).
//
//===----------------------------------------------------------------------===//

#include "GollvmPasses.h"

#include "llvm/ADT/DenseSet.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/GlobalVariable.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/Pass.h"
#include "llvm/PassRegistry.h"

using namespace llvm;

namespace {

class RemoveAddrSpacePass : public ModulePass {
 public:
  static char ID;

  RemoveAddrSpacePass() : ModulePass(ID), DL("") {}

  RemoveAddrSpacePass(const DataLayout &DL) : ModulePass(ID), DL(DL) {
    initializeRemoveAddrSpacePassPass(
        *PassRegistry::getPassRegistry());
  }

  bool runOnModule(Module &M) override;

 private:
  Type *IntTy; // type of uintptr_t, used to cast pointer to/from integer
  DataLayout DL; // data layout without non-integral pointer, passed in from the driver
  DenseSet<Constant*> Visited; // handle circular references

  // Replace addrspacecast in static initializer C with a pair
  // of ptr-to-int and int-to-ptr casts, as codegen cannot handle
  // addrspacecast in static initializer.
  void removeAddrSpaceCast(Constant *C);
};

} // namespace

char RemoveAddrSpacePass::ID = 0;
INITIALIZE_PASS(RemoveAddrSpacePass, "remove-addrspacecast",
                "Remove addrspacecast instructions", false,
                false)
ModulePass *llvm::createRemoveAddrSpacePass(const DataLayout &DL) {
  return new RemoveAddrSpacePass(DL);
}

void
RemoveAddrSpacePass::removeAddrSpaceCast(Constant *C) {
  if (Visited.count(C))
    return;
  Visited.insert(C);

  ConstantExpr *CE = dyn_cast<ConstantExpr>(C);
  if (CE && CE->getOpcode() == Instruction::AddrSpaceCast) {
    Constant *Op = CE->getOperand(0);
    Constant *New = ConstantExpr::getIntToPtr(
        ConstantExpr::getPtrToInt(Op, IntTy), CE->getType());
    CE->replaceAllUsesWith(New);
    Visited.erase(CE);
    CE->destroyConstant();
    C = New;
  }

  unsigned N = C->getNumOperands();
  for (unsigned Idx = 0; Idx < N; Idx++) {
    Constant *Op = cast<Constant>(C->getOperand(Idx));
    removeAddrSpaceCast(Op);
  }
}

bool
RemoveAddrSpacePass::runOnModule(Module &M) {
  // At this point we no longer need non-integral pointers.
  // Set data layout back to default.
  M.setDataLayout(DL);

  IntTy = Type::getInt64Ty(M.getContext());
  for (GlobalVariable &GV : M.globals())
    if (GV.hasInitializer())
      removeAddrSpaceCast(GV.getInitializer());
  return true;
}
