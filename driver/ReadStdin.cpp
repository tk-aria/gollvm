//===-- ReadStdin.cpp -----------------------------------------------------===//
//
// Copyright 2018 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
//
//===----------------------------------------------------------------------===//
//
// Gollvm driver helper class "ReadStdin" methods.
//
//===----------------------------------------------------------------------===//

#include "ReadStdin.h"

#include "Compilation.h"
#include "Driver.h"

#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/raw_ostream.h"

namespace gollvm {
namespace driver {


ReadStdin::ReadStdin(ToolChain &tc, bool mustBeEmpty)
    : InternalTool("stdinreader", tc),
      mustBeEmpty_(mustBeEmpty)
{
}

ReadStdin::~ReadStdin()
{
}

bool ReadStdin::performAction(Compilation &compilation,
                              const Action &jobAction,
                              const ArtifactList &inputArtifacts,
                              const Artifact &output)
{
  assert(inputArtifacts.empty());

  // Read in standard input, the LLVM way...
  llvm::ErrorOr<std::unique_ptr<llvm::MemoryBuffer>> stdinBufferOrErr =
      llvm::MemoryBuffer::getSTDIN();
  if (std::error_code errc = stdinBufferOrErr.getError()) {
    llvm::errs() << compilation.driver().progname()
                 << ": error reading stdin: " << errc.message();
    return false;
  }
  std::unique_ptr<llvm::MemoryBuffer> stdinBuf =
      std::move(stdinBufferOrErr.get());

  // Enforce the "must be empty" requirement if appropriate.
  if (mustBeEmpty_ && stdinBuf->getBufferSize() != 0) {
    llvm::errs() << compilation.driver().progname()
                 << ": unsupported language for -x, "
                 << "stdin be empty\n";
    return false;
  }

  // Emit to the output artifact.
  std::error_code errc;
  llvm::raw_fd_ostream ostr(output.file(), errc,
                            llvm::sys::fs::OpenFlags::F_None);
  if (errc) {
    llvm::errs() << compilation.driver().progname()
                 << ": cannot open " << output.file() << " for writing: "
                 << errc.message();
    return false;
  }

  ostr.write(stdinBuf->getBufferStart(), stdinBuf->getBufferSize());
  return true;
}

} // end namespace driver
} // end namespace gollvm
