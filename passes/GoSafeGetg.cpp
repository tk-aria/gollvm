//===--- GoSafeGetg.cpp ---------------------------------------------------===//
//
// Copyright 2019 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
//
//===----------------------------------------------------------------------===//
//
// LLVM backend pass to make sure inlined getg's are
// safe. Specifically, make sure the TLS address is not
// cached across a thread switch.
//
//===----------------------------------------------------------------------===//

#include "GollvmPasses.h"

#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/GlobalVariable.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Module.h"
#include "llvm/Pass.h"
#include "llvm/PassRegistry.h"
#include "llvm/Support/Debug.h"

using namespace llvm;

namespace {

class GoSafeGetg : public ModulePass {
 public:
  static char ID;

  GoSafeGetg() : ModulePass(ID) {
    initializeGoSafeGetgPass(*PassRegistry::getPassRegistry());
  }

  bool runOnModule(Module &M) override;
};

}  // namespace

char GoSafeGetg::ID = 0;
INITIALIZE_PASS(GoSafeGetg, "go-safegetg",
                "Ensure Go getg's are safe", false,
                false)
ModulePass *llvm::createGoSafeGetgPass() { return new GoSafeGetg(); }

// In the runtime g is a thread-local variable. The backend may
// choose to cache the TLS address in a register or on stack.
// If a thread switch happens, the cache will become invalid.
// Specifically, within a function,
//
//   load g
//   call mcall(...)
//   load g
//
// may be compiled to
//
//   leaq    g@TLS, %rdi
//   call    __tls_get_addr
//   movq    %rax, %rbx     // cache in a callee-save register %rbx
//   ... use g in %rax ...
//   call    foo
//   ... use g in %rbx ...
//
// This is incorrect if a thread switch happens at the call of foo.
// Currently, there seems no way to tell the backend to disable
// or invalidate the cache.
//
// In LLVM, this happens in SelectionDAG, where it CSEs the TLS
// address for multiple loads in the same basic block. It does not
// CSE the TLS address across multiple basic blocks. So we are
// safe if we can ensure there is at most one load of g in a block.
// This function looks for second load of g in each block, and, if
// found, replace it with a call of runtime.getg (i.e. undoing the
// inlining).
//
bool
GoSafeGetg::runOnModule(Module &M) {
  GlobalVariable *GV = M.getGlobalVariable("runtime.g");
  if (!GV)
    return false; // no access of g, nothing to do

  bool Changed = false;
  for (Function &F : M) {
    SmallVector<Instruction*, 2> ToDel;

    for (BasicBlock &BB : F) {
      bool HasGetg = false;
      bool HasCall = false; // whether we have seen a call after a getg
      for (Instruction &I : BB) {
        if (LoadInst *LI = dyn_cast<LoadInst>(&I)) {
          if (LI->getPointerOperand()->stripPointerCasts() == GV) {
            HasGetg = true;
            if (!HasCall)
              continue;

            // There is a getg and a call before this getg.
            // We replace the second one with a call.
            IRBuilder<> Builder(&I);
            FunctionCallee GetgFn =
                M.getOrInsertFunction("runtime.getg", I.getType());
            Instruction *Call = Builder.CreateCall(GetgFn);
            I.replaceAllUsesWith(Call);
            ToDel.push_back(&I);
            Changed = true;
            continue;
          }
        } else {
          // We should not see a use of g that is not a load.
          for (Value *O : I.operands())
            if (O->stripPointerCasts() == GV)
              report_fatal_error("non-load use of runtime.g: " +
                                 I.getName() + " ( in function " +
                                 F.getName() + ")");
        }

        if (HasGetg && !HasCall)
          if (CallInst *CI = dyn_cast<CallInst>(&I)) {
            if (Function *Fn = CI->getCalledFunction())
              if (Fn->isIntrinsic())
                continue; // intrinsics are ok
            HasCall = true;
          }
      }
    }

    for (Instruction* I : ToDel)
      I->eraseFromParent();
  }

  return Changed;
}
