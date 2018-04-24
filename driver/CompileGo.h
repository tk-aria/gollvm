//===-- CompileGo.h -------------------------------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// Defines the CompileGo class (helper for driver functionality).
//
//===----------------------------------------------------------------------===//

#ifndef GOLLVM_DRIVER_COMPILEGO_H
#define GOLLVM_DRIVER_COMPILEGO_H

#include "Tool.h"

namespace gollvm {
namespace driver {

class ToolChain;
class Compilation;
class Action;
class Artifact;
class CompileGoImpl;

// Concrete go compiler tool. This tool is used by the driver to carry
// out "compile" actions, e.g. "compile this set of Go files into
// assembly".

class CompileGo : public InternalTool {
 public:
  CompileGo(ToolChain &tc);
  ~CompileGo();

  // Perform compilation.
  bool performAction(Compilation &compilation,
                     const Action &jobAction,
                     const ArtifactList &inputArtifacts,
                     const Artifact &output) override;

 private:
  std::unique_ptr<CompileGoImpl> impl_;
};

} // end namespace driver
} // end namespace gollvm

#endif // GOLLVM_DRIVER_COMPILEGO_H
