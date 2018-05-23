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
#include "GollvmConfig.h"

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

static void
combineInputsWithEscapes(const std::set<unsigned> &escapes,
                         const std::set<unsigned> &flags,
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
    auto foundEscape = escapes.find(arg->getOption().getID());
    if (foundEscape != escapes.end())
      for (auto &av : arg->getValues())
        cmdArgs.push_back(av);

    // If this is part of the applicable flags set for the tool,
    // add the flag now.
    auto foundFlag = flags.find(arg->getOption().getID());
    if (foundFlag != flags.end())
      arg->render(args, cmdArgs);
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
  std::set<unsigned> asFlags;
  std::set<unsigned> asEscapes;
  asEscapes.insert(gollvm::options::OPT_Wa_COMMA);
  asEscapes.insert(gollvm::options::OPT_Xassembler);
  combineInputsWithEscapes(asEscapes, asFlags,
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

void Linker::addBeginFiles(llvm::opt::ArgStringList &cmdArgs)
{
  llvm::opt::ArgList &args = toolchain().driver().args();

  bool isPIE = toolchain().driver().isPIE();
  const char *crt1 = nullptr;
  if (!args.hasArg(gollvm::options::OPT_shared)) {
    // FIXME: no support yet for -pg
    if (isPIE)
      crt1 = "Scrt1.o";
    else
      crt1 = "crt1.o";
  }
  if (crt1)
    cmdArgs.push_back(args.MakeArgString(toolchain().getFilePath(crt1)));

  cmdArgs.push_back(args.MakeArgString(toolchain().getFilePath("crti.o")));

  const char *crtbegin = nullptr;
  if (args.hasArg(gollvm::options::OPT_static))
    crtbegin = "crtbeginT.o";
  else if (args.hasArg(gollvm::options::OPT_shared))
    crtbegin = "crtbeginS.o";
  else if (isPIE)
    crtbegin = "crtbeginS.o";
  else
    crtbegin = "crtbegin.o";
  cmdArgs.push_back(args.MakeArgString(toolchain().getFilePath(crtbegin)));
}

void Linker::addEndFiles(llvm::opt::ArgStringList &cmdArgs)
{
  llvm::opt::ArgList &args = toolchain().driver().args();

  const char *crtend = nullptr;
  if (args.hasArg(gollvm::options::OPT_shared) ||
      toolchain().driver().isPIE())
    crtend = "crtendS.o";
  else
    crtend = "crtend.o";
  cmdArgs.push_back(args.MakeArgString(toolchain().getFilePath(crtend)));
  cmdArgs.push_back(args.MakeArgString(toolchain().getFilePath("crtn.o")));
}

void Linker::addLDM(llvm::opt::ArgStringList &cmdArgs)
{
  cmdArgs.push_back("-m");
  switch (toolchain().driver().triple().getArch()) {
    case llvm::Triple::x86:
      cmdArgs.push_back("elf_i386");
      break;
    case llvm::Triple::x86_64:
      // NB: no GNUX32 support
      cmdArgs.push_back("elf_x86_64");
      break;
    default:
      // unhandled architecture
      cmdArgs.push_back("%unknown%");
      assert(false);
  }
}

void Linker::addSharedAndOrStaticFlags(llvm::opt::ArgStringList &cmdArgs)
{
  llvm::opt::ArgList &args = toolchain().driver().args();

  if (!args.hasArg(gollvm::options::OPT_static)) {
    cmdArgs.push_back("--eh-frame-hdr");
    if (!args.hasArg(gollvm::options::OPT_shared)) {
      // NB: no support for --dyld-prefix= option
      const std::string Loader = toolchain().getDynamicLinker(args);
      cmdArgs.push_back("-dynamic-linker");
      cmdArgs.push_back(args.MakeArgString(Loader));
    } else {
      cmdArgs.push_back("-shared");
    }
    if (toolchain().driver().isPIE())
      cmdArgs.push_back("-pie");
  } else {
    cmdArgs.push_back("-static");
  }
}

// Adds each thing in the toolchain filepath as an -L option.

void Linker::addFilePathArgs(llvm::opt::ArgStringList &cmdArgs)
{
  llvm::opt::ArgList &args = toolchain().driver().args();
  for (auto & fp : toolchain().filePaths())
    if (fp.length() > 0)
      cmdArgs.push_back(args.MakeArgString(llvm::StringRef("-L") + fp));
}

void Linker::addSysLibsStatic(llvm::opt::ArgList &args,
                              llvm::opt::ArgStringList &cmdArgs)
{
  // Go and pthread related libs.
  cmdArgs.push_back("-lgobegin");
  cmdArgs.push_back("-lgo");
  addFilePathArgs(cmdArgs);
  cmdArgs.push_back("-lm");
  cmdArgs.push_back("-u");
  cmdArgs.push_back("pthread_create");
  cmdArgs.push_back("--wrap=pthread_create");

  // Libgcc and libc.
  cmdArgs.push_back("--start-group");
  cmdArgs.push_back("-lgcc");
  cmdArgs.push_back("-lgcc_eh");
  cmdArgs.push_back("-lpthread");
  cmdArgs.push_back("-lc");
  cmdArgs.push_back("--end-group");
}

void Linker::addSysLibsShared(llvm::opt::ArgList &args,
                              llvm::opt::ArgStringList &cmdArgs)
{
  bool isStaticLibgo = args.hasArg(gollvm::options::OPT_static_libgo);
  bool havePthreadFlag = args.hasArg(gollvm::options::OPT_pthreads);
  cmdArgs.push_back("-lgobegin");
  if (isStaticLibgo)
    cmdArgs.push_back("-Bstatic");
  cmdArgs.push_back("-lgo");
  if (isStaticLibgo)
    cmdArgs.push_back("-Bdynamic");

  addFilePathArgs(cmdArgs);
  cmdArgs.push_back("-lm");
  cmdArgs.push_back("--wrap=pthread_create");

  // Libgcc and libc.
  bool isShared = args.hasArg(gollvm::options::OPT_shared);
  cmdArgs.push_back("-lgcc_s");
  if (!isShared)
    cmdArgs.push_back("-lgcc");
  if (isStaticLibgo || havePthreadFlag)
    cmdArgs.push_back("-lpthread");
  cmdArgs.push_back("-lc");
  cmdArgs.push_back("-lgcc_s");
  if (!isShared)
    cmdArgs.push_back("-lgcc");
}

bool Linker::constructCommand(Compilation &compilation,
                              const Action &jobAction,
                              const ArtifactList &inputArtifacts,
                              const Artifact &output)
{
  llvm::opt::ArgList &args = compilation.driver().args();
  llvm::opt::ArgStringList cmdArgs;

  // Honor -fuse-ld=XXX
  const char *executable = "ld.gold";
  llvm::opt::Arg *ldarg = args.getLastArg(gollvm::options::OPT_fuse_ld_EQ);
  if (ldarg != nullptr)
    executable = ldarg->getValue();

  cmdArgs.push_back(executable);

  // Perform program path lookup if needed.
  if (!llvm::sys::path::is_absolute(executable))
    executable = args.MakeArgString(toolchain().getProgramPath(executable));

  // Output file.
  cmdArgs.push_back("-o");
  cmdArgs.push_back(output.file());

  bool useStdLib = !args.hasArg(gollvm::options::OPT_nostdlib);

  if (useStdLib) {

    // Select proper options depending on presence of -static/-shared, etc.
    // Dynamic linker selection is also done here.
    addSharedAndOrStaticFlags(cmdArgs);

    // Add crtbegin*.
    addBeginFiles(cmdArgs);
  }

  // Incorporate inputs and -l/-L flags with -Wl,.. and -Xlinker args, in
  // correct order.
  std::set<unsigned> ldFlags;
  ldFlags.insert(gollvm::options::OPT_l);
  ldFlags.insert(gollvm::options::OPT_L);
  std::set<unsigned> ldEscapes;
  ldEscapes.insert(gollvm::options::OPT_Wl_COMMA);
  ldEscapes.insert(gollvm::options::OPT_Xlinker);
  combineInputsWithEscapes(ldEscapes, ldFlags,
                           inputArtifacts, args, cmdArgs);

  // Add -m flag.
  addLDM(cmdArgs);

  // FIXME: common this up with the assembler sequence above.
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

  // Pick up correct directory for Go libraries.
  std::string golib("-L");
  golib += GOLLVM_INSTALL_LIBDIR;
  cmdArgs.push_back(args.MakeArgString(golib.c_str()));

  if (useStdLib) {

    // Incorporate linker arguments needed for Go.
    bool isStatic = args.hasArg(gollvm::options::OPT_static);
    if (isStatic)
      addSysLibsStatic(args, cmdArgs);
    else
      addSysLibsShared(args, cmdArgs);

    // crtend files.
    addEndFiles(cmdArgs);
  }

  // end of args.
  cmdArgs.push_back(nullptr);

  // Add final command.
  compilation.addCommand(jobAction, *this, executable, cmdArgs);
  cmdArgs.push_back(nullptr);

  return true;
}

} // end namespace gnutools
