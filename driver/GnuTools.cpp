//===-- GnuTools.cpp ------------------------------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// Implementations of gnutools Assembler and Linker classes.
//
//===----------------------------------------------------------------------===//

#include "GnuTools.h"

#include "Compilation.h"
#include "Driver.h"
#include "ToolChain.h"

#include "llvm/Option/ArgList.h"
#include "llvm/Support/Path.h"

using namespace gollvm::driver;

namespace gnutools {

Assembler::Assembler(gollvm::driver::ToolChain &tc)
    : ExternalTool("gnu-assembler", tc)
{
}

bool Assembler::constructCommand(Compilation &compilation,
                                 const Action &jobAction,
                                 const ArtifactList &inputArtifacts,
                                 const Artifact &output)
{
  // implementation to appear in a subsequent patch
  return false;
}

Linker::Linker(gollvm::driver::ToolChain &tc)
    : ExternalTool("gnu-linker", tc)
{
}

bool Linker::constructCommand(Compilation &compilation,
                              const Action &jobAction,
                              const ArtifactList &inputArtifacts,
                              const Artifact &output)
{
  // implementation to appear in a subsequent patch
  return false;
}

} // end namespace gnutools
