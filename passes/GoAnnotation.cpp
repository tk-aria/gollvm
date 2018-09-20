//===--- GoAnnotation.cpp -------------------------------------------------===//
//
// Copyright 2018 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
//
//===----------------------------------------------------------------------===//
//
// LLVM backend pass to attach auxiliary information to the
// exception table, for the use of Go stack maps.
//
//===----------------------------------------------------------------------===//

#include "GoStackMap.h"
#include "GollvmPasses.h"

#include "llvm/CodeGen/MachineFunctionPass.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineModuleInfo.h"
#include "llvm/CodeGen/Passes.h"
#include "llvm/CodeGen/TargetInstrInfo.h"
#include "llvm/PassRegistry.h"
#include "llvm/Target/TargetMachine.h"

using namespace llvm;

namespace {

class GoAnnotation : public MachineFunctionPass {
 public:
  static char ID;

  GoAnnotation() : MachineFunctionPass(ID) {
    initializeGoAnnotationPass(*PassRegistry::getPassRegistry());
  }

  void getAnalysisUsage(AnalysisUsage &AU) const override {
    AU.setPreservesAll();
    MachineFunctionPass::getAnalysisUsage(AU);
  }

  bool runOnMachineFunction(MachineFunction &MF) override;

 private:
};

}  // namespace

char GoAnnotation::ID = 0;
INITIALIZE_PASS(GoAnnotation, "go-annotation",
                "Add annotations for Go code", false,
                false)
FunctionPass *llvm::createGoAnnotationPass() { return new GoAnnotation(); }

bool
GoAnnotation::runOnMachineFunction(MachineFunction &MF) {
  // Create a dummy landing pad entry at the function entry PC,
  // with a sentinel value, to mark this as a Go function.

  MCContext &Context = MF.getContext();
  const TargetInstrInfo *TII = MF.getSubtarget().getInstrInfo();

  MachineBasicBlock &EntryBB = MF.front();
  auto MBBI = EntryBB.begin();
  DebugLoc DL = EntryBB.findDebugLoc(MBBI);

  MCSymbol *EntryLabel = Context.createTempSymbol();
  BuildMI(EntryBB, MBBI, DL, TII->get(TargetOpcode::EH_LABEL)).addSym(EntryLabel);

  // Besides begin/end labels and typeID, other fields of LPI
  // does not really matter. We set fields that are required
  // to make EHStreamer happy.
  LandingPadInfo &LPI = MF.getOrCreateLandingPadInfo(&EntryBB);
  LPI.BeginLabels.push_back(EntryLabel);
  LPI.EndLabels.push_back(EntryLabel); // 0 size, so it doesn't capture any exception.
  LPI.LandingPadLabel = EntryLabel;

  const Function &F = MF.getFunction();
  const Module *M = F.getParent();
  GlobalValue *X = M->getGlobalVariable(GO_FUNC_SYM,
                                        /* AllowInternal */ true);
  assert(X);
  unsigned TypeID = MF.getTypeIDFor(X);
  LPI.TypeIds.push_back(TypeID);

  return true;
}
