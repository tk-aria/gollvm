//===-- go-llvm-irbuilders.cpp - 'BlockLIRBuilder' class methods ----------===//
//
// Copyright 2018 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
//
//===----------------------------------------------------------------------===//
//
// Methods for class BlockLIRBuilder.
//
//===----------------------------------------------------------------------===//

#include "go-llvm-irbuilders.h"
#include "namegen.h"

BlockLIRBuilder::BlockLIRBuilder(llvm::Function *dummyFcn,
                                 NameGen *namegen)
    : LIRBuilder(dummyFcn->getContext(), llvm::ConstantFolder()),
      dummyBlock_(llvm::BasicBlock::Create(dummyFcn->getContext(), "",
                                           dummyFcn)),
      namegen_(namegen)
{
  SetInsertPoint(dummyBlock_.get());
}

BlockLIRBuilder::~BlockLIRBuilder()
{
  assert(dummyBlock_->getInstList().empty());
  dummyBlock_->removeFromParent();
}

std::vector<llvm::Instruction*> BlockLIRBuilder::instructions()
{
  std::vector<llvm::Instruction*> rv;
  for (auto &i : dummyBlock_->getInstList())
    rv.push_back(&i);
  for (auto &i : rv) {
    i->removeFromParent();
    // hack: irbuilder likes to create unnamed bitcasts
    if (i->isCast() && i->getName() == "")
      i->setName(namegen_->namegen("cast"));
  }
  return rv;
}
