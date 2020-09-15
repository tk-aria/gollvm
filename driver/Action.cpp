//===-- Action.cpp --------------------------------------------------------===//
//
// Copyright 2018 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
//
//===----------------------------------------------------------------------===//
//
// Gollvm driver helper class Action methods.
//
//===----------------------------------------------------------------------===//

#include "Action.h"
#include "Artifact.h"

#include "llvm/Support/raw_ostream.h"

#include <sstream>

namespace gollvm {
namespace driver {

const char *Action::getName() const
{
  switch (type_) {
    case A_ReadStdin: return "readstdin";
    case A_InputFile: return "inputfile";
    case A_CompileAndAssemble: return "compile+assemble";
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

std::string Action::toString()
{
  std::stringstream s;
  s << "Action " << getName() << std::endl << "  inputs:\n";
  for (auto inp : inputs()) {
    s << "    " << inp->getName() << " ";
    InputAction *ia = inp->castToInputAction();
    if (ia)
      s << ia->input()->toString();
    s << "\n";
  }
  return s.str();
}

void Action::dump()
{
  llvm::errs() << toString();
}

} // end namespace driver
} // end namespace gollvm
