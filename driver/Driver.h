//===-- Driver.h ----------------------------------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// Defines the Driver class (helper for driver functionality).
//
//===----------------------------------------------------------------------===//

#ifndef GOLLVM_DRIVER_DRIVER_H
#define GOLLVM_DRIVER_DRIVER_H

#include "llvm/ADT/Optional.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/Triple.h"
#include "llvm/Option/ArgList.h"
#include "llvm/Option/OptTable.h"
#include "llvm/Support/CodeGen.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Target/TargetOptions.h"

#include "Action.h"
#include "Artifact.h"
#include "GollvmOptions.h"

#include <unordered_map>

namespace gollvm {
namespace driver {

class Compilation;
class InternalTool;
class ToolChain;

// Driver class. Drives the process of translating a given command
// line into a series of compilation actions, then into commands to
// carry out those actions.

class Driver {
 public:
  Driver(llvm::opt::InputArgList &args,
         llvm::opt::OptTable *optTable,
         const char *argv0);
  ~Driver();

  // Set up target and select toolchain. Returns nullptr on error.
  ToolChain *setup();

  // Create compilation object
  std::unique_ptr<Compilation> buildCompilation(ToolChain &tc);

  // Build actions for compilation. Returns false if error.
  // TEMPORARY: pass in asm out file from compiled Go.
  bool buildActions(Compilation &compilation,
                    const std::string &asmOutFile);

  // Process the action list. This means:
  // - execute any non-dependent actions that don't require the
  //   invocation of an external tool, and
  // - generate generate a list of commands for invoking external tools.
  // Return is true for success, false for error;
  bool processActions(Compilation &compilation);

  // Locate an object file needed for linking (ex: crt0.o)
  std::string getFilePath(llvm::StringRef name, ToolChain &toolchain);

  // Locate an external tool (ex: "as"). Returns a full path if a hit
  // is found, otherwise just returns its name argument.
  std::string getProgramPath(llvm::StringRef name, ToolChain &toolchain);

  // For constructing default output file with -c, -S, etc.
  std::string firstFileBase();

  // Getters
  const llvm::Triple &triple() const { return triple_; }
  llvm::opt::InputArgList &args() { return args_; }
  llvm::opt::OptTable &opts() { return *opts_; }
  const char *progname() const { return progname_; }

  // Name of driver (program invoked)
  std::string name();

  // Sysroot (or empty string if not present)
  std::string sysRoot() { return sysroot_; }

  // Helpers related to command line options.
  llvm::PICLevel::Level getPicLevel();
  bool isPIE();
  template<typename IT>
  llvm::Optional<IT> getLastArgAsInteger(gollvm::options::ID id,
                                         IT defaultValue);
  llvm::Optional<llvm::Reloc::Model> reconcileRelocModel();
  bool reconcileOptionPair(gollvm::options::ID yesOption,
                           gollvm::options::ID noOption,
                           bool defaultVal);
  llvm::Optional<llvm::FPOpFusion::FPOpFusionMode> getFPOpFusionMode();
  typedef llvm::SmallVector<llvm::opt::Arg *, 8> inarglist;
  ActionList createInputActions(const inarglist &infiles,
                                Compilation &compilation);

 private:
  llvm::Triple triple_;
  llvm::opt::InputArgList &args_;
  llvm::opt::OptTable *opts_;
  const char *progname_;
  std::string sysroot_;
  // maps target to toolchain for that target
  llvm::StringMap<std::unique_ptr<ToolChain>> toolchains_;
  // Maps non-input actions to output artifacts.
  std::unordered_map<Action *, Artifact*> artmap_;

  bool processAction(Action *act, Compilation &compilation, bool lastAct);
  ArtifactList collectInputArtifacts(Action *act, InternalTool *it);
  void emitVersion();
};

template<typename IT>
llvm::Optional<IT>
Driver::getLastArgAsInteger(gollvm::options::ID id,
                            IT defaultValue)
{
  IT result = defaultValue;
  llvm::opt::Arg *arg = args_.getLastArg(id);
  if (arg != nullptr) {
    if (llvm::StringRef(arg->getValue()).getAsInteger(10, result)) {
      llvm::errs() << progname_ << ": invalid argument '"
                   << arg->getValue() << "' to '"
                   << arg->getAsString(args_) << "' option\n";
      return llvm::None;
    }
  }
  return result;
}

} // end namespace driver
} // end namespace gollvm

#endif // GOLLVM_DRIVER_DRIVER_H
