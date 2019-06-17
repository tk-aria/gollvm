//===--- GoMemops.cpp ----------------------------------------------------===//
//
// Copyright 2019 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
//
//===----------------------------------------------------------------------===//
//
// Replace calls to libc memmove, memcpy, and memset with
// wrappers __go_memmove, etc., for better split-stack
// performance.
//
//===----------------------------------------------------------------------===//

#include "GollvmPasses.h"

#include "llvm/CodeGen/MachineFunctionPass.h"
#include "llvm/CodeGen/Passes.h"
#include "llvm/CodeGen/TargetInstrInfo.h"
#include "llvm/PassRegistry.h"
#include "llvm/Target/TargetMachine.h"

using namespace llvm;

namespace {

class GoMemops : public MachineFunctionPass {
 public:
  static char ID;

  GoMemops() : MachineFunctionPass(ID) {
    initializeGoMemopsPass(*PassRegistry::getPassRegistry());
  }

  void getAnalysisUsage(AnalysisUsage &AU) const override {
    AU.setPreservesAll();
    MachineFunctionPass::getAnalysisUsage(AU);
  }

  bool runOnMachineFunction(MachineFunction &MF) override;

 private:
};

}  // namespace

char GoMemops::ID = 0;
INITIALIZE_PASS(GoMemops, "go-memops",
                "Replace memmove calls with split-stack wrappers", false,
                false)
FunctionPass *llvm::createGoMemopsPass() { return new GoMemops(); }

bool
GoMemops::runOnMachineFunction(MachineFunction &MF) {
  for (MachineBasicBlock &MBB : MF)
    for (MachineInstr &MI : MBB)
      if (MI.isCall() && MI.getOperand(0).isSymbol()) {
         const char *Name = MI.getOperand(0).getSymbolName();
         const char *NewName;
         if (strcmp(Name, "memmove") == 0)
           NewName = "__go_memmove";
         else if (strcmp(Name, "memcpy") == 0)
           NewName = "__go_memcpy";
         else if (strcmp(Name, "memset") == 0)
           NewName = "__go_memset";
         else
           continue;

         unsigned Flags = MI.getOperand(0).getTargetFlags();
         MI.getOperand(0).ChangeToES(NewName, Flags);
      }

  return true;
}
