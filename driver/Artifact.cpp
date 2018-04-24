//===-- Artifact.cpp ------------------------------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// Gollvm driver helper class Artifact methods.
//
//===----------------------------------------------------------------------===//

#include "Artifact.h"
#include "llvm/Option/Arg.h"

#include "llvm/Support/raw_ostream.h"

#include <sstream>

namespace gollvm {
namespace driver {

const char *Artifact::file() const
{
  assert(type_ == A_Argument || type_ == A_TempFile);
  return (type_ == A_Argument ?
          u.arg->getValue() : u.file);
}

llvm::opt::Arg *Artifact::arg()
{
  return (type_ == A_Argument ? u.arg : nullptr);
}

std::string Artifact::toString()
{
  std::stringstream ss;
  ss << "Artifact ";
  if (type_ == A_Argument)
    ss << "arg(" << u.arg->getValue() << ")";
  else
    ss << "file(" << u.file << ")";
  return ss.str();
}

void Artifact::dump()
{
  llvm::errs() << toString() << "\n";
}

} // end namespace driver
} // end namespace gollvm
