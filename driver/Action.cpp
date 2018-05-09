//===-- Action.cpp ------------------------------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// Gollvm driver helper class Action methods.
//
//===----------------------------------------------------------------------===//

#include "Action.h"
#include "Artifact.h"

#include "llvm/Support/raw_ostream.h"

namespace gollvm {
namespace driver {

const char *Action::getName() const
{
  switch (type_) {
    case A_ReadStdin: return "readstdin";
    case A_InputFile: return "inputfile";
    case A_Compile: return "compile";
    case A_Assemble: return "assemble";
    case A_Link: return "link";
    default:
      assert(false);
      return "<unknown action type>";
  }
  return nullptr;
}

const char *Action::resultFileSuffix() const
{
  switch (type_) {
    case A_ReadStdin: {
      const ReadStdinAction *rsia = this->castToReadStdinAction();
      return rsia->suffix();
    }
    case A_InputFile: return "i";
    case A_Compile: return "s";
    case A_Assemble: return "o";
    case A_Link: return "e";
    default:
      assert(false);
      return "x";
  }
  return nullptr;
}

void Action::dump()
{
  llvm::errs() << "Action " << getName() << " inputs:\n";
  for (auto inp : inputs()) {
    llvm::errs() << "  " << ((void*) inp) << " " << inp->getName() << " ";
    InputAction *ia = inp->castToInputAction();
    if (ia)
      llvm::errs() << ia->input()->toString();
    llvm::errs() << "\n";
  }
}

} // end namespace driver
} // end namespace gollvm
