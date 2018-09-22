//===- GoStatepoints.h - --------------------------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file provides interface to the "Go statepoints" pass.
//
// Rewrite call/invoke instructions so as to record live variables on
// stack for the use of garbage collector.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_GOLLVM_PASSES_GOSTATEPOINTS_H
#define LLVM_GOLLVM_PASSES_GOSTATEPOINTS_H

#include "llvm/IR/PassManager.h"

namespace llvm {

class DominatorTree;
class Function;
class Module;
class TargetTransformInfo;
class TargetLibraryInfo;

struct GoStatepoints : public PassInfoMixin<GoStatepoints> {
  PreservedAnalyses run(Module &M, ModuleAnalysisManager &AM);

  bool runOnFunction(Function &F, DominatorTree &, TargetTransformInfo &,
                     const TargetLibraryInfo &);
};

} // namespace llvm

#endif // LLVM_GOLLVM_PASSES_GOSTATEPOINTS_H
