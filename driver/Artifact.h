//===-- Artifact.h --------------------------------------------------------===//
//
// Copyright 2018 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
//
//===----------------------------------------------------------------------===//
//
// Defines the Artifact class (helper for driver functionality).
//
//===----------------------------------------------------------------------===//

#ifndef GOLLVM_DRIVER_ARTIFACT_H
#define GOLLVM_DRIVER_ARTIFACT_H

#include "llvm/ADT/SmallVector.h"

namespace llvm {
namespace opt {
class Arg;
}
}

namespace gollvm {
namespace driver {

// An artifact is a file produced or consumed by some compilation step.
// Artifacts may correspond to user-specified files (command line arg
// for example) or temporary files created by some intermediate phase
// in the compilation.

class Artifact {
 public:
  enum Type {
    A_Argument,
    A_TempFile,
    A_Empty
  };

  // Empty artifact (unusable as is)
  Artifact() : type_(A_Empty) { u.arg = nullptr; }

  // Construct an artifact from a command line arg.
  explicit Artifact(llvm::opt::Arg *arg)
      : type_(A_Argument) { u.arg = arg; }

  // Construct an artifact given a temp file path.
  explicit Artifact(const char *tempfilepath)
      : type_(A_TempFile) { u.file = tempfilepath; }

  // Type of input
  Type type() const { return type_; }

  // File for input
  const char *file() const;

  // Return input argument if type is A_Argument, null otherwise.
  llvm::opt::Arg *arg();

  // Debugging
  std::string toString();
  void dump();

 private:
  Type type_;
  union {
    llvm::opt::Arg *arg;
    const char *file;
  } u;
};

// A list of artifacts.
typedef llvm::SmallVector<Artifact *, 3> ArtifactList;

} // end namespace driver
} // end namespace gollvm

#endif // GOLLVM_DRIVER_ARTIFACT_H
