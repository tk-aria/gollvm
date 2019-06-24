//===--- GoWrappers.cpp ---------------------------------------------------===//
//
// Copyright 2019 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
//
//===----------------------------------------------------------------------===//
//
// Replace calls to some libc functions (e.g. memmove)
// with wrappers __go_memmove, etc., for better split-stack
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

static cl::opt<bool> Disabled("disable-go-wrappers",
                              cl::desc("Disable Go wrappers pass"),
                              cl::init(false), cl::Hidden);

namespace {

class GoWrappers : public MachineFunctionPass {
 public:
  static char ID;

  GoWrappers() : MachineFunctionPass(ID) {
    initializeGoWrappersPass(*PassRegistry::getPassRegistry());
  }

  void getAnalysisUsage(AnalysisUsage &AU) const override {
    AU.setPreservesAll();
    MachineFunctionPass::getAnalysisUsage(AU);
  }

  bool runOnMachineFunction(MachineFunction &MF) override;

 private:
};

}  // namespace

char GoWrappers::ID = 0;
INITIALIZE_PASS(GoWrappers, "go-wrappers",
                "Replace libc calls with split-stack wrappers", false,
                false)
FunctionPass *llvm::createGoWrappersPass() { return new GoWrappers(); }

bool
GoWrappers::runOnMachineFunction(MachineFunction &MF) {
  if (Disabled)
    return false;

  if (!MF.shouldSplitStack())
    return false;

  for (MachineBasicBlock &MBB : MF)
    for (MachineInstr &MI : MBB)
      if (MI.isCall()) {
        // memmove is an external symbol, whereas _Unwind_Resume
        // is a global address. We handle both cases. It is fine
        // that we always replace with external symbol.
        const char *Name;
        if (MI.getOperand(0).isSymbol())
          Name = MI.getOperand(0).getSymbolName();
        else if (MI.getOperand(0).isGlobal())
          Name = MI.getOperand(0).getGlobal()->getName().str().c_str();
        else
          continue;

        const char *NewName;
        if (strcmp(Name, "memmove") == 0)
          NewName = "__go_memmove";
        else if (strcmp(Name, "memcpy") == 0)
          NewName = "__go_memcpy";
        else if (strcmp(Name, "memset") == 0)
          NewName = "__go_memset";
        else if (strcmp(Name, "memcmp") == 0)
          NewName = "__go_memcmp";
        else if (strcmp(Name, "_Unwind_Resume") == 0)
          NewName = "__go_Unwind_Resume";
        else
          continue;

        unsigned Flags = MI.getOperand(0).getTargetFlags();
        MI.getOperand(0).ChangeToES(NewName, Flags);
      }

  return true;
}
