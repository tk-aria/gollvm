//===-- IntegAssembler.h --------------------------------------------------===//
//
// Copyright 2020 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
//
//===----------------------------------------------------------------------===//
//
// Defines the IntegAssembler class (helper for driver functionality).
//
//===----------------------------------------------------------------------===//

#ifndef GOLLVM_DRIVER_INTEGASSEMBLER_H
#define GOLLVM_DRIVER_INTEGASSEMBLER_H

#include "Tool.h"

namespace gollvm {
namespace driver {

class ToolChain;
class Compilation;
class Action;
class Artifact;
class IntegAssemblerImpl;

// Integrated assembler tool. This tool is used by the driver to carry
// out "assemble" actions when -fintegrated-as is in effect, e.g. "compile
// this assembly file down to an object".

class IntegAssembler : public InternalTool {
 public:
  IntegAssembler(ToolChain &tc, const std::string &executablePath);
  ~IntegAssembler();

  // Perform compilation.
  bool performAction(Compilation &compilation,
                     const Action &jobAction,
                     const ArtifactList &inputArtifacts,
                     const Artifact &output) override;

 private:
  std::unique_ptr<IntegAssemblerImpl> impl_;
};

} // end namespace driver
} // end namespace gollvm

#endif // GOLLVM_DRIVER_INTEGASSEMBLER_H
