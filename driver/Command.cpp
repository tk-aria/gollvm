//===-- Command.cpp -------------------------------------------------------===//
//
// Copyright 2018 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
//
//===----------------------------------------------------------------------===//
//
// Gollvm driver helper class Command methods.
//
//===----------------------------------------------------------------------===//

#include "Command.h"

#include "llvm/Option/ArgList.h"
#include "llvm/Support/Program.h"

namespace gollvm {
namespace driver {

Command::Command(const Action &srcAction,
                 const Tool &creator,
                 const char *executable,
                 llvm::opt::ArgStringList &args)
    : action_(srcAction),
      creator_(creator),
      executable_(executable),
      arguments_(args)
{
}

int Command::execute(std::string *errMsg)
{
  std::vector<llvm::StringRef> argv;
  size_t n = arguments_.size() - 1;
  argv.reserve(n);
  for (size_t i = 0; i < n; ++i)
    argv.push_back(arguments_[i]);
  return llvm::sys::ExecuteAndWait(executable_,
                                   argv,
                                   /*env=*/llvm::None,
                                   /*Redirects*/{},
                                   /*secondsToWait=*/0,
                                   /*memoryLimit=*/0,
                                   errMsg);
}

void Command::print(llvm::raw_ostream &os, bool quoteArgs)
{
  os << " " << executable_;
  const char *qu = (quoteArgs ? "\"" : "");
  bool first = true;
  for (auto arg : arguments_) {
    if (first) {
      first = false;
      continue;
    }
    if (arg != nullptr)
      os << " "  << qu << arg << qu;
  }
  os << "\n";
}

} // end namespace driver
} // end namespace gollvm
