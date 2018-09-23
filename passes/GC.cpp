//===--- GC.cpp -----------------------------------------------------------===//
//
// Copyright 2018 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
//
//===----------------------------------------------------------------------===//
//
// GC strategy for Go runtime.
//
//===----------------------------------------------------------------------===//

#include "GollvmPasses.h"

#include "llvm/CodeGen/GCStrategy.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/Support/Casting.h"

using namespace llvm;

namespace {

class GoGC : public GCStrategy {
public:
  GoGC() {
    UseStatepoints = true;
    InitRoots = false; // not using gc.root

    // TODO: write barrier?
  }

  Optional<bool> isGCManagedPointer(const Type *Ty) const override {
    return isa<PointerType>(Ty);
  }
};

GCRegistry::Add<GoGC> X("go", "Go garbage collector.");

} // namespace

void llvm::linkGoGC() {}
