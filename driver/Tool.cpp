//===-- Tool.cpp ----------------------------------------------------------===//
//
// Copyright 2018 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
//
//===----------------------------------------------------------------------===//
//
// Gollvm driver helper class Tool methods.
//
//===----------------------------------------------------------------------===//

#include "Tool.h"

using namespace llvm;

#include "GollvmOptions.h"
#include "Driver.h"
#include "ToolChain.h"

#include "llvm/Option/Arg.h"
#include "llvm/Option/ArgList.h"
#include "llvm/Support/Regex.h"

namespace gollvm {
namespace driver {

Tool::Tool(const char *name, ToolChain &tc, ToolClass klass)
    : name_(name), toolchain_(tc), klass_(klass)
{
}

Tool::~Tool()
{
}

InternalTool::InternalTool(const char *name,
                           ToolChain &tc,
                           const std::string &executablePath)
    : Tool(name, tc, Tool::Internal),
      tc_(tc),
      executablePath_(executablePath)
{
}

InternalTool::~InternalTool()
{
}

static void quoteDump(const std::string &str, bool doquote)
{
  Regex qureg("^[-_/A-Za-z0-9_\\.]+$");
  errs() << " ";
  if (doquote)
    doquote = !qureg.match(str);
  errs() << (doquote ? "\"" : "") << str << (doquote ? "\"" : "");
}

static void dumpArg(opt::Arg &arg, bool doquote)
{
  if (arg.getOption().getKind() != opt::Option::InputClass)
    quoteDump(arg.getSpelling().str(), doquote);
  for (auto &val : arg.getValues())
    quoteDump(val, doquote);
}

bool InternalTool::emitMinusVOrHashHashHash(const Triple &triple,
                                            const Artifact &output,
                                            const Action &jobAction)
{
  // If -v is in effect, print something to show the effect of the
  // compilation. This is in some sense a fiction, because the top
  // level driver is not invoking an external tool to perform the
  // compile, but there is an expectation with compilers that if you
  // take the "-v" output and then execute each command shown by hand,
  // you'll get the same effect as the original command that produced
  // the "-v" output.
  opt::InputArgList &args = tc_.driver().args();
  bool hashHashHash = args.hasArg(gollvm::options::OPT__HASH_HASH_HASH);
  if (args.hasArg(gollvm::options::OPT_v) || hashHashHash) {
    errs() << "Target: " << triple.str() << "\n";
    errs() << " " << executablePath_;
    if (!args.hasArg(gollvm::options::OPT_S) &&
        jobAction.type() == Action::A_Compile &&
        !args.hasArg(gollvm::options::OPT_emit_llvm))
      errs() << " " << "-S";
    for (auto arg : args) {
      // Special case for -L. Here even if the user said "-L /x"
      // we render it as -L/x so as to be compatible with existing
      // code in the imported that expects the former and not the latter.
      if (arg->getOption().matches(gollvm::options::OPT_L))
        errs() << " -L" << arg->getValue();
      if (arg->getOption().getGroup().isValid() &&
          (arg->getOption().getGroup().getID() ==
           gollvm::options::OPT_Link_Group))
        continue;
      if (arg->getOption().matches(gollvm::options::OPT_v) ||
          arg->getOption().matches(gollvm::options::OPT_c) ||
          arg->getOption().matches(gollvm::options::OPT_o) ||
          arg->getOption().matches(gollvm::options::OPT__HASH_HASH_HASH) ||
          arg->getOption().matches(gollvm::options::OPT_save_temps))
        continue;
      dumpArg(*arg, hashHashHash);
    }

    errs() << " " << "-L" << tc_.driver().installedLibDir() << " " << "-o";
    quoteDump(output.file(), hashHashHash);
    errs() << "\n";
  }

  return hashHashHash;
}

ExternalTool::ExternalTool(const char *name, ToolChain &tc)
      : Tool(name, tc, Tool::External)
{
}

ExternalTool::~ExternalTool()
{
}

} // end namespace driver
} // end namespace gollvm
