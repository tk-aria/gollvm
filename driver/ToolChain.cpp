//===-- ToolChain.cpp -----------------------------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// Gollvm driver helper class ToolChain methods.
//
//===----------------------------------------------------------------------===//

#include "llvm/Support/raw_ostream.h"

#include "Action.h"
#include "Driver.h"
#include "Tool.h"
#include "ToolChain.h"

namespace gollvm {
namespace driver {

ToolChain::ToolChain(Driver &driver,
                     const llvm::Triple &targetTriple)
    : driver_(driver),
      triple_(targetTriple)
{
}

ToolChain::~ToolChain()
{
}

Tool *ToolChain::getCompiler()
{
  if (compiler_.get() == nullptr)
    compiler_.reset(buildCompiler());
  return compiler_.get();
}

Tool *ToolChain::getAssembler()
{
  if (assembler_.get() == nullptr)
    assembler_.reset(buildAssembler());
  return assembler_.get();
}

Tool *ToolChain::getLinker()
{
  if (linker_.get() == nullptr)
    linker_.reset(buildLinker());
  return linker_.get();
}

Tool *ToolChain::getTool(Action *act)
{
  assert(act != nullptr);
  switch(act->type()) {
    case Action::A_Compile:
      return getCompiler();
    case Action::A_Assemble:
      return getAssembler();
    case Action::A_Link:
      return getLinker();
    default:
      assert(false);
      return nullptr;
  }
  return nullptr;
}

std::string ToolChain::getFilePath(const char *afile)
{
  return driver_.getFilePath(afile, thisToolChain());
}

std::string ToolChain::getProgramPath(const char *atool)
{
  return driver_.getProgramPath(atool, thisToolChain());
}

} // end namespace driver
} // end namespace gollvm
