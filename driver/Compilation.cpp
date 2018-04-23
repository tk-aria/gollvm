//===-- Compilation.cpp ---------------------------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// Gollvm driver helper class Compilation methods.
//
//===----------------------------------------------------------------------===//

#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Signals.h"
#include "llvm/Support/raw_ostream.h"

#include "Compilation.h"

#include "Artifact.h"
#include "Command.h"
#include "Driver.h"

namespace gollvm {
namespace driver {

Compilation::Compilation(Driver &driver,
                         ToolChain &toolchain)
    : driver_(driver),
      toolchain_(toolchain)
{
}

Compilation::~Compilation()
{
  // Clean temp files unless -save-temps is in effect
  if (driver_.args().hasArg(gollvm::options::OPT_save_temps))
    return;
  for (auto tf : tempFileNames_)
    llvm::sys::fs::remove(tf);
}

llvm::opt::OptTable &Compilation::opts() {
  return driver_.opts();
}

llvm::opt::InputArgList &Compilation::args()
{
  return driver_.args();
}

std::string Compilation::firstFileBase()
{
  for (llvm::opt::Arg *arg : driver_.args()) {
    if (arg->getOption().getKind() == llvm::opt::Option::InputClass) {
      std::string firstFile(arg->getValue());
      size_t dotindex = firstFile.find_last_of(".");
      if (dotindex == std::string::npos)
        continue;
      return firstFile.substr(0, dotindex);
    }
  }
  assert(false);
  return "internal_error";
}

Artifact *Compilation::newArgArtifact(llvm::opt::Arg *arg)
{
  // to be implemented in a later patch
  return nullptr;
}

Artifact *Compilation::newFileArtifact(const char *tempfilename)
{
  // to be implemented in a later patch
  return nullptr;
}

Artifact *Compilation::createOutputFileArtifact(Action *act)
{
  // to be implemented in a later patch
  return nullptr;
}

llvm::Optional<Artifact*> Compilation::createTemporaryFileArtifact(Action *act)
{
  // to be implemented in a later patch
  return llvm::None;
}

void Compilation::addCommand(const Action &srcAction,
                             const Tool &creatingTool,
                             const char *executable,
                             llvm::opt::ArgStringList &args)
{
  ownedCommands_.push_back(llvm::make_unique<Command>(srcAction,
                                                      creatingTool,
                                                      executable,
                                                      args));
  commands_.push_back(ownedCommands_.back().get());
}

bool Compilation::executeCommands()
{
  // to be implemented in a later patch
  return false;
}

} // end namespace driver
} // end namespace gollvm
