//===-- Compilation.cpp ---------------------------------------------------===//
//
// Copyright 2018 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
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

#include <sstream>

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
      if (!strcmp(arg->getValue(), "-"))
        return "-";
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
  ownedArtifacts_.push_back(std::unique_ptr<Artifact>(new Artifact(arg)));
  return ownedArtifacts_.back().get();
}

Artifact *Compilation::createFakeFileArtifact(Action *act)
{
  std::stringstream s;
  s << "/tmp/out." << act->getName() << "." << paths_.size();
  std::string path(s.str());
  paths_.push_back(std::string(path));
  const char *fn = paths_.back().c_str();
  ownedArtifacts_.push_back(std::unique_ptr<Artifact>(new Artifact(fn)));
  return ownedArtifacts_.back().get();
}

Artifact *Compilation::newFileArtifact(const char *path, bool isTempFile)
{
  paths_.push_back(std::string(path));
  const char *fn = paths_.back().c_str();
  if (isTempFile)
    tempFileNames_.push_back(fn);
  ownedArtifacts_.push_back(std::unique_ptr<Artifact>(new Artifact(fn)));
  return ownedArtifacts_.back().get();
}

Artifact *Compilation::createOutputFileArtifact(Action *act)
{
  llvm::opt::InputArgList &args = driver_.args();

  // Honor -o if present
  llvm::opt::Arg *outf = args.getLastArg(gollvm::options::OPT_o);
  if (outf)
    return newArgArtifact(outf);

  // No -o: construct output file name.
  std::string ofn;
  if (act->type() == Action::A_Link) {
    assert(! args.hasArg(gollvm::options::OPT_emit_llvm));
    ofn = "a.out";
  } else {
    ofn = firstFileBase();
    if (args.hasArg(gollvm::options::OPT_emit_llvm)) {
      if (args.hasArg(gollvm::options::OPT_S))
        ofn += ".ll";
      else
        ofn += ".bc";
    } else {
      if (args.hasArg(gollvm::options::OPT_S))
        ofn += ".s";
      else
        ofn += ".o";
    }
  }

  outFileName_ = ofn;
  return newFileArtifact(ofn.c_str(), false);
}

llvm::Optional<Artifact*> Compilation::createTemporaryFileArtifact(Action *act)
{
  llvm::SmallString<128> tempFileName;
  std::error_code tfcEC =
      llvm::sys::fs::createTemporaryFile(act->getName(),
                                         act->resultFileSuffix(),
                                         tempFileName);
  if (tfcEC) {
    llvm::errs() << driver_.progname() << ": error: "
                 << tfcEC.message() << "\n";
    return llvm::None;
  }
  return newFileArtifact(tempFileName.c_str(), true);
}

void Compilation::addCommand(const Action &srcAction,
                             const Tool &creatingTool,
                             const char *executable,
                             llvm::opt::ArgStringList &args)
{
  ownedCommands_.push_back(std::make_unique<Command>(srcAction,
                                                     creatingTool,
                                                     executable,
                                                     args));
  commands_.push_back(ownedCommands_.back().get());
}

bool Compilation::executeCommands()
{
  llvm::opt::ArgList &args = driver().args();

  bool hashHashHash = args.hasArg(gollvm::options::OPT__HASH_HASH_HASH);
  for (auto cmd : commands_) {

    // Support -v and/or -###
    if (hashHashHash || args.hasArg(gollvm::options::OPT_v))
      cmd->print(llvm::errs(), hashHashHash);

    // Support -###
    if (hashHashHash)
      continue;

    // Execute.
    std::string errMsg;
    int rc = cmd->execute(&errMsg);
    if (rc != 0) {
      if (!errMsg.empty())
        llvm::errs() << errMsg << "\n";
      return false;
    }
  }

  return true;
}

} // end namespace driver
} // end namespace gollvm
