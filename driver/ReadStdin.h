//===-- ReadStdin.h -------------------------------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// Defines the ReadStdin class (helper for driver functionality).
//
//===----------------------------------------------------------------------===//

#ifndef GOLLVM_DRIVER_READSTDIN_H
#define GOLLVM_DRIVER_READSTDIN_H

#include "Tool.h"

namespace gollvm {
namespace driver {

class ToolChain;
class Compilation;
class Action;
class Artifact;

// This class encapsulates the reading of standard input during a compilation
// that includes the pseudo-input flag "-". The driver handles this case
// by creating a tool (ReadStdin) that consumes standard input and emits
// a temporary file, which is then piped through the remainder of the compiler
// as usual.  See also the notes in the command parsing code relating to
// handling of and "-x c".

class ReadStdin : public InternalTool {
 public:

  // If mustBeEmpty is set, then issue an error if stdin has any contents.
  ReadStdin(ToolChain &tc, bool mustBeEmpty);
  ~ReadStdin();

  // Perform actoin (reading stdin)
  bool performAction(Compilation &compilation,
                     const Action &jobAction,
                     const ArtifactList &inputArtifacts,
                     const Artifact &output) override;

 private:
  bool mustBeEmpty_;
};

} // end namespace driver
} // end namespace gollvm

#endif // GOLLVM_DRIVER_COMPILEGO_H
