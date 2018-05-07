//===-- llvm-goc.cpp - compiler driver for gollvm  ------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// Compiler driver for gollvm. Invokes frontend / backend to compile
// Go code into assembly and/or object files, and orchestrates process
// of assembling and linking if needed.
//
//===----------------------------------------------------------------------===//

#include "go-llvm-linemap.h"
#include "go-llvm-diagnostics.h"
#include "go-llvm.h"
#include "go-c.h"
#include "mpfr.h"
#include "GollvmOptions.h"
#include "GollvmConfig.h"

#include "Compilation.h"
#include "Driver.h"
#include "ToolChain.h"
#include "Tool.h"

#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/Triple.h"
#include "llvm/Analysis/TargetLibraryInfo.h"
#include "llvm/Bitcode/BitcodeWriterPass.h"
#include "llvm/IR/DiagnosticInfo.h"
#include "llvm/IR/DiagnosticPrinter.h"
#include "llvm/IR/IRPrintingPasses.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Verifier.h"
#include "llvm/MC/SubtargetFeature.h"
#include "llvm/Option/Arg.h"
#include "llvm/Option/ArgList.h"
#include "llvm/Option/OptTable.h"
#include "llvm/Option/Option.h"
#include "llvm/Passes/PassBuilder.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/Format.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/ManagedStatic.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/PrettyStackTrace.h"
#include "llvm/Support/Process.h"
#include "llvm/Support/Program.h"
#include "llvm/Support/Signals.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/ToolOutputFile.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Transforms/IPO.h"
#include "llvm/Transforms/IPO/PassManagerBuilder.h"

#include <algorithm>
#include <cstring>
#include <string>
#include <system_error>
#include <sys/types.h>
#include <unistd.h>

using namespace llvm;
using namespace gollvm::driver;

class CommandLineParser {
 public:
  CommandLineParser(opt::OptTable *opts)
      : opts_(opts)
  { }

  bool parseCommandLine(int argc, char **argv);

  opt::InputArgList &args() { return args_; }

 private:
  opt::OptTable *opts_;
  opt::InputArgList args_;
};

bool CommandLineParser::parseCommandLine(int argc, char **argv)
{
  const char *progname = argv[0];

  unsigned missingArgIndex, missingArgCount;
  ArrayRef<const char *> argvv = makeArrayRef(argv, argc);
  args_ = opts_->ParseArgs(argvv.slice(1), missingArgIndex, missingArgCount);

  // Honor --help first
  if (args_.hasArg(gollvm::options::OPT_help)) {
    opts_->PrintHelp(errs(), progname, "Gollvm (LLVM-based Go compiler)",
                     0, 0, false);
    exit(0);
  }

  // Honor -dumpversion
  if (args_.hasArg(gollvm::options::OPT_dumpversion)) {
    llvm::outs() << GOLLVM_COMPILERVERSION << "\n";
    exit(0);
  }

  // Complain about missing arguments.
  if (missingArgIndex != 0) {
    errs() << progname << ": error: argument to '"
           << args_.getArgString(missingArgIndex)
           << "' option missing, espected "
           << missingArgCount << " value(s)\n";
    return false;
  }

  // Check for unsupported options.
  for (const opt::Arg *arg : args_) {
    if (arg->getOption().hasFlag(gollvm::options::Unsupported)) {
      errs() << progname << ": error: unsupported command line option '"
             << arg->getAsString(args_) << "'\n";
      return false;
    }
  }

  // Check for unknown options.
  bool foundUnknown = false;
  for (const opt::Arg *arg : args_.filtered(gollvm::options::OPT_UNKNOWN)) {
    errs() << progname << ": error: unrecognized command line option '"
             << arg->getAsString(args_) << "'\n";
    foundUnknown = true;
  }
  if (foundUnknown)
    return false;

  // Honor -mllvm
  auto llvmargs = args_.getAllArgValues(gollvm::options::OPT_mllvm);
  if (! llvmargs.empty()) {
    unsigned nargs = llvmargs.size();
    auto args = llvm::make_unique<const char*[]>(nargs + 2);
    args[0] = "gollvm (LLVM option parsing)";
    for (unsigned i = 0; i != nargs; ++i)
      args[i + 1] = llvmargs[i].c_str();
    args[nargs + 1] = nullptr;
    llvm::cl::ParseCommandLineOptions(nargs + 1, args.get());
  }

  // Vett the -x option if present. At the moment only "-x go" is accepted
  // (assembler not allowed, but it could conceivably be added later).
  opt::Arg *xarg = args_.getLastArg(gollvm::options::OPT_x);
  if (xarg != nullptr &&
      ! llvm::StringRef(xarg->getValue()).equals("go")) {
    errs() << progname << ": invalid argument '"
           << xarg->getValue() << "' to '"
           << xarg->getAsString(args_) << "' option\n";
    return false;
  }

  return true;
}

int main(int argc, char **argv)
{
  // Print a stack trace if we signal out.
  sys::PrintStackTraceOnErrorSignal(argv[0]);
  PrettyStackTraceProgram X(argc, argv);
  llvm_shutdown_obj Y;  // Call llvm_shutdown() on exit.

  // Parse command line.
  std::unique_ptr<opt::OptTable> opts =
      gollvm::options::createGollvmDriverOptTable();
  CommandLineParser clp(opts.get());
  if (!clp.parseCommandLine(argc, argv))
    return 1;

  // Create driver.
  Driver driver(clp.args(), opts.get(), argv[0]);

  // Set up driver, select target and toolchain.
  ToolChain *toolchain = driver.setup();
  if (toolchain == nullptr)
    return 1;

  // Build compilation; construct actions for this compile.
  std::unique_ptr<Compilation> compilation =
      driver.buildCompilation(*toolchain);
  if (!driver.buildActions(*compilation))
    return 2;

  // Process the action list. This will carry out actions that don't
  // require use of an external tool, and will generate a list of
  // commands for invoking external tools.
  if (!driver.processActions(*compilation))
    return 3;

  // Execute the external-tool command list created above.
  if (! compilation->executeCommands())
    return 4;

  // We're done.
  return 0;
}
