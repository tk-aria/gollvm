//===-- GnuTools.cpp ------------------------------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// Implementations of gnutools Assembler and Linker classes.
//
//===----------------------------------------------------------------------===//

#include "GnuTools.h"

#include "Compilation.h"
#include "Driver.h"
#include "ToolChain.h"

#include "llvm/Option/ArgList.h"
#include "llvm/Support/Path.h"

#include <set>

using namespace gollvm::driver;

namespace gnutools {

// This helper routine is used for constructing linker and
// assembler command lines. It combines input arguments with
// any escape-oriented command line arguments via "-Wl,..." or
// equivalent. For example, consider the following command line:
//
//    llvm-goc -L /somepath foo.go -o qux \
//        -Wl,--whole-archive mumble.a -Wl,--no-whole-archive blah.a
//
// This will result in three linker inputs (foo.o, mumble.a, and blah.a).
// Here in order to get the semantics we have to properly interleave
// the inputs with the flags, e.g.
//
//    ld -o qux <...> foo.o --whole-archive mumble.a --no-whole-archive blah.a
//
// This helper routine walks through the command line arguments and picks
// out the corresponding "escaped" arguments and mixes them in with
// any args that appear in the input list.

static void combineInputsWithEscapes(gollvm::options::ID escape1,
                                     gollvm::options::ID escape2,
                                     const ArtifactList &inputArtifacts,
                                     llvm::opt::ArgList &args,
                                     llvm::opt::ArgStringList &cmdArgs)
{
  // Collect the args mentioned in the input artifacts.
  std::set<llvm::opt::Arg *> argset;
  for (auto &inart : inputArtifacts) {
    if (inart->type() == Artifact::A_Argument)
      argset.insert(inart->arg());
    else
      cmdArgs.push_back(inart->file());
  }

  // Walk the args to sort things out.
  for (auto arg : args) {

    // If this is an arg that is part of the input set, append it now.
    if (arg->getOption().getKind() == llvm::opt::Option::InputClass &&
        argset.find(arg) != argset.end()) {
      cmdArgs.push_back(arg->getValue());
      continue;
    }

    // If this matches one of our escape options, then add its value(s) now.
    if (arg->getOption().matches(escape1) ||
        arg->getOption().matches(escape2))
      for (auto &av : arg->getValues())
        cmdArgs.push_back(av);
  }
}

Assembler::Assembler(gollvm::driver::ToolChain &tc)
    : ExternalTool("gnu-assembler", tc)
{
}

bool Assembler::constructCommand(Compilation &compilation,
                                 const Action &jobAction,
                                 const ArtifactList &inputArtifacts,
                                 const Artifact &output)
{
  llvm::opt::ArgList &args = toolchain().driver().args();
  llvm::opt::ArgStringList cmdArgs;

  // Executable path.
  const char *executable =
      args.MakeArgString(toolchain().getProgramPath("as"));
  if (! executable) {
    llvm::errs() << "error: unable to locate path for 'as'\n";
    return false;
  }
  cmdArgs.push_back(executable);

  // Add correct 32/64 option.
  switch (toolchain().driver().triple().getArch()) {
    case llvm::Triple::x86:
      cmdArgs.push_back("--32");
      break;
    case llvm::Triple::x86_64:
      // NB: no GNUX32 support yet
      cmdArgs.push_back("--64");
      break;
    default:
      break;
  }

  // Output file.
  cmdArgs.push_back("-o");
  cmdArgs.push_back(output.file());

  // Incorporate inputs with -Wa,.. and -Xassembler args, in correct order.
  combineInputsWithEscapes(gollvm::options::OPT_Wa_COMMA,
                           gollvm::options::OPT_Xassembler,
                           inputArtifacts, args, cmdArgs);

  // Support for compressed debug.
  llvm::opt::Arg *gzarg = args.getLastArg(gollvm::options::OPT_gz,
                                          gollvm::options::OPT_gz_EQ);
  if (gzarg != nullptr) {
    if (gzarg->getOption().matches(gollvm::options::OPT_gz)) {
      cmdArgs.push_back("-compress-debug-sections");
    } else {
      std::string cds("-compress-debug-sections=");
      cds += gzarg->getValue();
      cmdArgs.push_back(args.MakeArgString(cds));
    }
  }
  cmdArgs.push_back(nullptr);

  // Add final command.
  compilation.addCommand(jobAction, *this,
                         executable, cmdArgs);

  return true;
}

Linker::Linker(gollvm::driver::ToolChain &tc)
    : ExternalTool("gnu-linker", tc)
{
}

bool Linker::constructCommand(Compilation &compilation,
                              const Action &jobAction,
                              const ArtifactList &inputArtifacts,
                              const Artifact &output)
{
  // implementation to appear in a subsequent patch
  return false;
}

} // end namespace gnutools
