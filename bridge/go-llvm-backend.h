//===-- go-llvm-backend.h - Backend class public interfaces  --------------===//
//
// Copyright 2018 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
//
//===----------------------------------------------------------------------===//
//
// Public interfaces for LLVM implementation of Backend class.
//
//===----------------------------------------------------------------------===//

#ifndef GO_LLVM_BACKEND_H
#define GO_LLVM_BACKEND_H

#include "go-llvm.h"

#include "llvm/IR/LLVMContext.h"

class Backend;

extern Backend *go_get_backend(llvm::LLVMContext &Context, llvm::CallingConv::ID cconv);

#endif // !defined(GO_LLVM_BACKEND_H)
