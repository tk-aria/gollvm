//===-- Tool.h ------------------------------------------------------------===//
//
// Copyright 2018 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
//
//===----------------------------------------------------------------------===//
//
// Defines the Tool class (helper for driver functionality).
//
//===----------------------------------------------------------------------===//

#ifndef GOLLVM_DRIVER_TOOL_H
#define GOLLVM_DRIVER_TOOL_H

#include "llvm/ADT/SmallVector.h"
#include "Artifact.h"

namespace gollvm {
namespace driver {

class Action;
class Compilation;
class ExternalTool;
class InternalTool;
class ToolChain;

// Abstract class representing a compilation helper "tool", such as an
// assembler or linker. A tool reads a set of file inputs and produces
// an output file. Tool objects are either "external" or "internal"; for
// the latter the tool itself is capable or performing an action,
// for the former the tool works by constructing a command line for some
// external program to carry out the action.

class Tool {
 public:
  enum ToolClass { External, Internal };
  Tool(const char *name, ToolChain &tc, ToolClass klass);
  virtual ~Tool();

  ToolChain &toolchain() { return toolchain_; }
  const char *name() const { return name_; }

  inline InternalTool *castToInternalTool();
  inline const InternalTool *castToInternalTool() const;
  inline ExternalTool *castToExternalTool();
  inline const ExternalTool *castToExternalTool() const;

 private:
  const char *name_;
  ToolChain &toolchain_;
  ToolClass klass_;
};

// An "internal" tool -- carries out the steps needs to perform some
// action (such as compilation or assembly) within the driver itself
// (as opposed to via invocation of an external program).

class InternalTool : public Tool {
 public:
  InternalTool(const char *name, ToolChain &tc);
  virtual ~InternalTool();

  // Given a specific action, perform the action now (as opposed to
  // invoking an external program to perform the action). Return value
  // is true for success, false otherwise.
  virtual bool performAction(Compilation &compilation,
                             const Action &jobAction,
                             const ArtifactList &inputArtifacts,
                             const Artifact &output) = 0;
 private:
};

// An "external" tool -- carries out the steps needs to perform
// an action via invocation of a program external to the driver.

class ExternalTool : public Tool {
 public:
  ExternalTool(const char *name, ToolChain &tc);
  virtual ~ExternalTool();

  // Given a specific action, add a new Command to 'compilation' to
  // carry out that action via the invocation of an external program
  // (such as a linker or assembler). Return value is true for
  // success, false otherwise.
  virtual bool constructCommand(Compilation &compilation,
                                const Action &jobAction,
                                const ArtifactList &inputArtifacts,
                                const Artifact &output) = 0;

 private:
};

inline InternalTool *Tool::castToInternalTool() {
  return klass_ == Internal ? static_cast<InternalTool*>(this) : nullptr;
}

inline const InternalTool *Tool::castToInternalTool() const {
  return klass_ == Internal ? static_cast<const InternalTool*>(this) : nullptr;
}

inline ExternalTool *Tool::castToExternalTool() {
  return klass_ == External ? static_cast<ExternalTool*>(this) : nullptr;
}

inline const ExternalTool *Tool::castToExternalTool() const {
  return klass_ == External ? static_cast<const ExternalTool*>(this) : nullptr;
}

} // end namespace driver
} // end namespace gollvm

#endif // GOLLVM_DRIVER_TOOL_H
