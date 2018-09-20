//===--- GollvmPasses.h - Gollvm specific backend passes ------------------===//
//
// Copyright 2018 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_GOLLVM_PASSES_GOLLVMPASSES_H
#define LLVM_GOLLVM_PASSES_GOLLVMPASSES_H

namespace llvm {

class PassRegistry;
class FunctionPass;

void initializeGoAnnotationPass(PassRegistry&);

FunctionPass *createGoAnnotationPass();

} // namespace llvm

#endif
