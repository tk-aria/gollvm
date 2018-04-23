//===-- Compilation.h -----------------------------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// Defines the Compilation class (helper for driver functionality).
//
//===----------------------------------------------------------------------===//

#ifndef GOLLVM_DRIVER_COMPILATION_H
#define GOLLVM_DRIVER_COMPILATION_H

#include <string>
#include "Action.h"
#include "Command.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/Option/OptTable.h"
#include "llvm/Option/ArgList.h"

namespace gollvm {
namespace driver {

class Command;
class Driver;
class Tool;
class ToolChain;

// Compilation class: this is primarily a container for state
// related to the compilation: driver, toolchain, list of actions,
// list of commands, temporary files, etc.

class Compilation {
 public:
  Compilation(Driver &driver,
              ToolChain &toolChain);
  ~Compilation();

  // Execute queued commands to invoke external tools.
  // Return is true for success, false for error;
  bool executeCommands();

  // Create a temp file and return as artifact.
  llvm::Optional<Artifact*> createTemporaryFileArtifact(Action *act);

  // Return an artifact corresponding to the proper output file (depends
  // on action plus command line flags).
  Artifact* createOutputFileArtifact(Action *act);

  // Toolchain, driver
  ToolChain &toolchain() { return toolchain_; }
  Driver &driver() { return driver_; }

  // Table of options
  llvm::opt::OptTable &opts();

  // Command line arguments
  llvm::opt::InputArgList &args();

  // First input file basename.
  std::string firstFileBase();

  // Compilation actions (built up by driver)
  ActionList &actions() { return actions_; }

  // Commands to carry out compilation
  CommandList &commands();

  // Create new input artifact based on arg.
  Artifact *newArgArtifact(llvm::opt::Arg *arg);

  // Push this action onto the owned actions list.
  template <typename T> void recordAction(T *act) {
    assert(act != nullptr);
    ownedActions_.push_back(std::unique_ptr<Action>(act));
  }

  // Schedule this action as part of the compilation.
  void addAction(Action *act) {
    actions_.push_back(act);
  }

  // Create a new command and add to the commands list.
  void addCommand(const Action &srcAction,
                  const Tool &creatingTool,
                  const char *executable,
                  llvm::opt::ArgStringList &args);

 private:
  Driver &driver_;
  ToolChain &toolchain_;
  ActionList actions_;
  CommandList commands_;
  std::string outFileName_;
  llvm::SmallVector<std::unique_ptr<Action>, 8> ownedActions_;
  llvm::SmallVector<std::unique_ptr<Artifact>, 8> ownedArtifacts_;
  llvm::SmallVector<std::unique_ptr<Command>, 8> ownedCommands_;
  llvm::SmallVector<std::string, 8> tempFileNames_;

  // Create new artifact based on temp file.
  Artifact *newFileArtifact(const char *tempfilename);
};

} // end namespace driver
} // end namespace gollvm

#endif // GOLLVM_DRIVER_COMPILATION_H
