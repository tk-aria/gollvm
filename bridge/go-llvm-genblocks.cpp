//===-- go-llvm-genblocks.cpp - definition of GenBlock classes ------------===//
//
// Copyright 2018 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
//
//===----------------------------------------------------------------------===//
//
// Implementation of GenBlockVisitor class and related helper routines.
//
//===----------------------------------------------------------------------===//

#include "go-llvm-genblocks.h"

llvm::BasicBlock *genblocks(llvm::LLVMContext &context,
                            Llvm_backend *be,
                            Bfunction *func,
                            Bstatement *code_stmt)
{
  GenBlocksVisitor gb(context, be, func);
  simple_walk_nodes(code_stmt, gb);
  //return gb.getBlock(code_stmt);
  return nullptr;
}
