//===--- GollvmPasses.h - Gollvm specific backend passes ------------------===//
//
// Copyright 2018 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_GOLLVM_PASSES_GOLLVMPASSES_H
#define LLVM_GOLLVM_PASSES_GOLLVMPASSES_H

#include "llvm/ADT/SmallVector.h"

namespace llvm {

class DataLayout;
class FunctionPass;
class ModulePass;
class PassRegistry;
class Type;
class Value;

void initializeGoAnnotationPass(PassRegistry&);
void initializeGoStatepointsLegacyPassPass(PassRegistry&);
void initializeRemoveAddrSpacePassPass(PassRegistry&);

FunctionPass *createGoAnnotationPass();
ModulePass *createGoStatepointsLegacyPass();
ModulePass *createRemoveAddrSpacePass(const DataLayout&);

void linkGoGC();
void linkGoGCPrinter();

} // namespace llvm

namespace gollvm {
namespace passes {

// Helper functions.

// Whether a type contains pointer.
bool hasPointer(llvm::Type *);

// Compute the pointer bitmap for type T, stored into Words.
void getPtrBitmapForType(llvm::Type *T, const llvm::DataLayout &DL,
                         llvm::SmallVectorImpl<llvm::Value *> &Words);

} // namespace passes
} // namespace gollvm

#endif
