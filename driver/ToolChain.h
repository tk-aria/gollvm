//===-- ToolChain.h -------------------------------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// Defines the ToolChain class (helper for driver functionality).
//
//===----------------------------------------------------------------------===//

#ifndef GOLLVM_DRIVER_TOOLCHAIN_H
#define GOLLVM_DRIVER_TOOLCHAIN_H

#include <string>
#include "llvm/ADT/Triple.h"
#include "llvm/Option/ArgList.h"

#include "Action.h"
#include "Tool.h"

namespace gollvm {
namespace driver {

class Tool;
class Driver;

typedef llvm::SmallVector<std::string, 8> pathlist;

// Abstract class for a collection of compiler tools. A given toolchain
// will create Tool objects for use in carrying out specific actions.

class ToolChain {
 public:
  // Return compiler, assembler, linker tools
  Tool *getCompiler();
  Tool *getAssembler();
  Tool *getLinker();

  // Ask for correct tool based on action type
  Tool *getTool(Action *act);

  // Locate a given file within the file search path.
  std::string getFilePath(const char *afile);

  // Locate a program (tool) within the program search path.
  std::string getProgramPath(const char *atool);

  // Return filepath and programpath lists.
  pathlist &programPaths() {
    return programPaths_;
  }
  pathlist &filePaths() {
    return filePaths_;
  }

  virtual std::string getDynamicLinker(const llvm::opt::ArgList &args) = 0;

  // Getters driver.
  Driver &driver() const { return driver_; }

  virtual ~ToolChain();

 protected:
  ToolChain(Driver &driver,
            const llvm::Triple &targetTriple);
  llvm::Triple &triple() { return triple_; }

  // Build new tool of the appropriate type
  virtual Tool *buildCompiler() = 0;
  virtual Tool *buildAssembler() = 0;
  virtual Tool *buildLinker() = 0;

 private:
  Driver &driver_;
  llvm::Triple triple_;
  std::unique_ptr<Tool> compiler_;
  std::unique_ptr<Tool> assembler_;
  std::unique_ptr<Tool> linker_;

  // List of places to look for tools (ld, as, etc)
  pathlist programPaths_;

  // List of places to look for object files (ex: crt0.o)
  pathlist filePaths_;

  ToolChain &thisToolChain() {
    ToolChain *tc = const_cast<ToolChain*>(this);
    return *tc;
  }
};

} // end namespace driver
} // end namespace gollvm

#endif // GOLLVM_DRIVER_TOOLCHAIN_H
