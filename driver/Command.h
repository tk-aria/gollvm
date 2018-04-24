//===-- Command.h ----------------------------------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// Defines the Command class (helper for driver functionality).
//
//===----------------------------------------------------------------------===//

#ifndef GOLLVM_DRIVER_COMMAND_H
#define GOLLVM_DRIVER_COMMAND_H

#include <string>
#include "llvm/ADT/SmallVector.h"
#include "llvm/Option/ArgList.h"

namespace llvm {
class raw_ostream;
}

namespace gollvm {
namespace driver {

class Command;
class Action;
class Tool;

// Command lists contain pointers to actions owned by a Compilation.
typedef llvm::SmallVector<Command*, 3> CommandList;

// A Command is a container for the specific command line to be used
// for invoking an external tool to carry out an Action (a specific
// compilation step (for example, the exact set of strings needed to
// exec the assembler). Commands are created by Tools; Commands are
// stored in and owned by a Compilation.

class Command {
 public:
  Command(const Action &srcAction,
          const Tool &creator,
          const char *executable,
          llvm::opt::ArgStringList &args);

  // Execute the command. Returns 0 on success, non-zero on error.
  int execute(std::string *errMsg);

  // Print to string
  void print(llvm::raw_ostream &OS, bool quoteArgs);

  // Print for debugging
  void dbgPrint();

 private:
  const Action &action_;
  const Tool &creator_;
  const char *executable_;
  llvm::opt::ArgStringList arguments_;
};

} // end namespace driver
} // end namespace gollvm

#endif // GOLLVM_DRIVER_COMMAND_H
