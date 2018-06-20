//===-- GnuTools.h --------------------------------------------------------===//
//
// Copyright 2018 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
//
//===----------------------------------------------------------------------===//
//
// Declares classes for implementations of GNU linker/assembler tools
//
//===----------------------------------------------------------------------===//

#ifndef GOLLVM_DRIVER_GNUTOOLS_H
#define GOLLVM_DRIVER_GNUTOOLS_H

#include "Tool.h"
#include "Artifact.h"

#include "llvm/Option/ArgList.h"

namespace gollvm {
namespace driver {
class Action;
class Artifact;
class Compilation;
class ToolChain;
}
}
using gollvm::driver::Action;
using gollvm::driver::Artifact;
using gollvm::driver::ArtifactList;
using gollvm::driver::Compilation;
using gollvm::driver::ExternalTool;
using gollvm::driver::ToolChain;

namespace gnutools {

class Assembler : public ExternalTool {
 public:
  Assembler(gollvm::driver::ToolChain &tc);

  bool constructCommand(Compilation &compilation,
                        const Action &jobAction,
                        const ArtifactList &inputArtifacts,
                        const Artifact &output);
};

class Linker : public ExternalTool {
 public:
  Linker(gollvm::driver::ToolChain &tc);

  bool constructCommand(Compilation &compilation,
                        const Action &jobAction,
                        const ArtifactList &inputArtifacts,
                        const Artifact &output);
 private:
  void addBeginFiles(llvm::opt::ArgStringList &cmdArgs);
  void addEndFiles(llvm::opt::ArgStringList &cmdArgs);
  void addLDM(llvm::opt::ArgStringList &cmdArgs);
  void addSysLibsStatic(llvm::opt::ArgList &args,
                        llvm::opt::ArgStringList &cmdArgs);
  void addSysLibsShared(llvm::opt::ArgList &args,
                        llvm::opt::ArgStringList &cmdArgs);
  void addLibGcc(llvm::opt::ArgList &args,
                 llvm::opt::ArgStringList &cmdArgs);
  void addSharedAndOrStaticFlags(llvm::opt::ArgStringList &cmdArgs);
  void addFilePathArgs(llvm::opt::ArgStringList &cmdArgs);
};

} // end namespace gnutools

#endif // GOLLVM_DRIVER_GNUTOOLS_H
