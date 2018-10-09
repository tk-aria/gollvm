//===-- Driver.cpp --------------------------------------------------------===//
//
// Copyright 2018 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
//
//===----------------------------------------------------------------------===//
//
// Gollvm driver helper class Driver methods.
//
//===----------------------------------------------------------------------===//

#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/Program.h"

#include "Action.h"
#include "Compilation.h"
#include "Driver.h"
#include "LinuxToolChain.h"
#include "ToolChain.h"

using namespace llvm;

namespace gollvm {
namespace driver {

Driver::Driver(opt::InputArgList &args,
               opt::OptTable *optTable,
               const char *argv0)
    : args_(args),
      opts_(optTable),
      progname_(argv0)
{
  if (const opt::Arg *arg = args.getLastArg(gollvm::options::OPT_sysroot_EQ))
    sysroot_ = arg->getValue();
  if (const opt::Arg *arg = args.getLastArg(gollvm::options::OPT_gcc_toolchain_EQ))
    gccToolchainDir_ = arg->getValue();

  // Establish executable path and installation dir.
  executablePath_ = argv0;
  // Do a PATH lookup if argv0 is not a valid path.
  if (!llvm::sys::fs::exists(executablePath_)) {
    if (llvm::ErrorOr<std::string> path =
        llvm::sys::findProgramByName(executablePath_))
      executablePath_ = *path;
  }
  SmallString<128> abspath(executablePath_);
  llvm::sys::fs::make_absolute(abspath);
  installDir_ = llvm::sys::path::parent_path(abspath);
  prefixes_ = args.getAllArgValues(gollvm::options::OPT_B);
}

Driver::~Driver()
{
}

std::string Driver::installedLibDir()
{
  llvm::SmallString<256> ldir(installDir_);
  llvm::sys::path::append(ldir, "../lib64");
  return std::string(ldir.str());
}

// TODO: create a mechanism for capturing release tag/branch, and/or
// git/svn revision for LLVM, gollvm, and so on.

void Driver::emitVersion()
{
  // NB: the go build tool keys off the presence of the "experimental"
  // keyword (hashes compiler binary if detected).
  llvm::errs() << "gollvm version 1 (experimental)\n";
}

std::string Driver::getFilePath(llvm::StringRef name,
                                ToolChain &toolchain)
{
  // Include -Bprefixed name in search.
  SmallVector<std::string, 2> candidates;
  for (auto p : prefixes_)
    candidates.push_back((p + name).str());
  for (auto &cand : candidates) {
    if (llvm::sys::fs::exists(llvm::Twine(cand)))
      return cand;
  }

  // Examine install dir
  llvm::SmallString<256> installed(installedLibDir());
  llvm::sys::path::append(installed, name);
  if (llvm::sys::fs::exists(llvm::Twine(installed)))
    return installed.str();

  // Examine toolchain file paths.
  for (const auto &dir : toolchain.filePaths()) {
    llvm::SmallString<256> candidate(dir);
    llvm::sys::path::append(candidate, name);
    if (llvm::sys::fs::exists(llvm::Twine(candidate)))
      return candidate.str();
  }

  return name;
}

std::string Driver::getProgramPath(llvm::StringRef name,
                                   ToolChain &toolchain)
{
  // Include -Bprefixed and target-prefixed name in search.
  SmallVector<std::string, 3> candidates;
  for (auto p : prefixes_)
    candidates.push_back((p + name).str());
  candidates.push_back((triple_.str() + "-" + name).str());
  candidates.push_back(name);

  // Examine toolchain program paths.
  for (auto &dir : toolchain.programPaths()) {
    for (auto &cand : candidates) {
      llvm::SmallString<256> candidate(dir);
      llvm::sys::path::append(candidate, cand);
      if (llvm::sys::fs::can_execute(llvm::Twine(candidate)))
        return candidate.str();
    }
  }

  // Search path.
  for (auto &cand : candidates) {
    llvm::ErrorOr<std::string> pcand =
      llvm::sys::findProgramByName(cand);
    if (pcand)
      return *pcand;
  }

  return name;
}

// FIXME: some  platforms have PIE enabled by default; we don't
// yet support auto-detection of such platforms.

bool Driver::isPIE()
{
  // Treat these options as trumping -pie.
  // FIXME: also handle -r here when supported
  if (args_.hasArg(gollvm::options::OPT_shared) ||
      args_.hasArg(gollvm::options::OPT_static))
    return false;

  opt::Arg *arg = args_.getLastArg(gollvm::options::OPT_pie,
                                   gollvm::options::OPT_no_pie,
                                   gollvm::options::OPT_nopie);
  return (arg ? arg->getOption().matches(options::OPT_pie) : false);
}

// Return any settings from the -fPIC/-fpic/-fPIE/-fpie options, if
// present. The intent of the code below is to support "rightmost
// on the command line wins" (compatible with clang and other
// compilers), so if you specify "-fPIC -fpic" you get small PIC,
// whereas "-fPIC -fpic -fPIC" will give you large PIC.
// Similarly the presence of a "-fno-pic" to the right of "-fPIE"
// will disable use of the PIE code model.

PICLevel::Level Driver::getPicLevel()
{
  opt::Arg *arg = args_.getLastArg(gollvm::options::OPT_fpic,
                                   gollvm::options::OPT_fno_pic,
                                   gollvm::options::OPT_fPIC,
                                   gollvm::options::OPT_fno_PIC,
                                   gollvm::options::OPT_fpie,
                                   gollvm::options::OPT_fno_pie,
                                   gollvm::options::OPT_fPIE,
                                   gollvm::options::OPT_fno_PIE);
  if (arg == nullptr)
    return PICLevel::NotPIC;
  if (arg->getOption().matches(gollvm::options::OPT_fpic) ||
      arg->getOption().matches(gollvm::options::OPT_fpie))
    return PICLevel::SmallPIC;
  else if (arg->getOption().matches(gollvm::options::OPT_fPIC) ||
           arg->getOption().matches(gollvm::options::OPT_fPIE))
    return PICLevel::BigPIC;
  return PICLevel::NotPIC;
}

// Similar to the routine above, but for -fPIE/-fpie etc.

PIELevel::Level Driver::getPieLevel()
{
  opt::Arg *arg = args_.getLastArg(gollvm::options::OPT_fpie,
                                   gollvm::options::OPT_fno_pie,
                                   gollvm::options::OPT_fPIE,
                                   gollvm::options::OPT_fno_PIE);
  if (arg == nullptr)
    return PIELevel::Default;
  if (arg->getOption().matches(gollvm::options::OPT_fpie))
    return PIELevel::Small;
  else if (arg->getOption().matches(gollvm::options::OPT_fPIE))
    return PIELevel::Large;
  return PIELevel::Default;
}

// Returns TRUE if the rightmost enable -fpic/-fpie command line option is
// PIE as opposed to PIC.

bool Driver::picIsPIE()
{
  opt::Arg *lpa = args_.getLastArg(gollvm::options::OPT_fPIC,
                              gollvm::options::OPT_fno_PIC,
                              gollvm::options::OPT_fpic,
                              gollvm::options::OPT_fno_pic,
                              gollvm::options::OPT_fPIE,
                              gollvm::options::OPT_fno_PIE,
                              gollvm::options::OPT_fpie,
                              gollvm::options::OPT_fno_pie);
  if (!lpa)
    return false;
  opt::Option opt = lpa->getOption();
  return (opt.matches(gollvm::options::OPT_fPIE) ||
          opt.matches(gollvm::options::OPT_fpie));
}

// Given a pair of llvm::opt options (presumably corresponding to
// -fXXX and -fno-XXX boolean flags), select the correct value for the
// option depending on the relative position of the options on the
// command line (rightmost wins). For example, given -fblah -fno-blah
// -fblah, we want the same semantics as a single -fblah.

bool
Driver::reconcileOptionPair(gollvm::options::ID yesOption,
                            gollvm::options::ID noOption,
                            bool defaultVal)
{
  opt::Arg *arg = args_.getLastArg(yesOption, noOption);
  if (arg == nullptr)
    return defaultVal;
  if (arg->getOption().matches(yesOption))
    return true;
  assert(arg->getOption().matches(noOption));
  return false;
}

Optional<Reloc::Model>
Driver::reconcileRelocModel()
{
  auto picLevel = getPicLevel();
  if (picLevel != PICLevel::NotPIC) {
    Reloc::Model R = Reloc::PIC_;
    return R;
  }
  return None;
}

Optional<FPOpFusion::FPOpFusionMode>
Driver::getFPOpFusionMode()
{
  opt::Arg *arg = args_.getLastArg(gollvm::options::OPT_ffp_contract_EQ);
  FPOpFusion::FPOpFusionMode res = FPOpFusion::Standard;
  if (arg != nullptr) {
    std::string val(arg->getValue());
    if (val == "off")
      res = FPOpFusion::Strict;
    else if (val == "on")
      res = FPOpFusion::Standard;
    else if (val == "fast")
      res = FPOpFusion::Fast;
    else {
      errs() << progname_ << ": invalid argument '"
             << arg->getValue() << "' to '"
             << arg->getAsString(args_) << "' option\n";
      return None;
    }
  }
  return res;
}

std::unique_ptr<Compilation> Driver::buildCompilation(ToolChain &tc)
{
  return std::unique_ptr<Compilation>(new Compilation(*this, tc));
}

ToolChain *Driver::setup()
{
   bool inputseen = false;

  if (args_.hasArg(gollvm::options::OPT_v) ||
      args_.hasArg(gollvm::options::OPT__HASH_HASH_HASH))
    emitVersion();

  // Set triple.
  if (const opt::Arg *arg = args_.getLastArg(gollvm::options::OPT_target_EQ))
    triple_ = Triple(Triple::normalize(arg->getValue()));
  else
    triple_ = Triple(sys::getDefaultTargetTriple());

  // Honor -dumpmachine
  if (args_.hasArg(gollvm::options::OPT_dumpmachine)) {
    llvm::outs() << triple_.str() << "\n";
    exit(0);
  }

  // Look up toolchain.
  auto &tc = toolchains_[triple_.str()];
  if (!tc) {
    switch (triple_.getOS()) {
      case Triple::Linux:
        tc = make_unique<toolchains::Linux>(*this, triple_);
        break;
      default:
        errs() << progname_ << ": error: unsupported target "
               << triple_.str() << ", unable to create toolchain\n";
        return nullptr;
    }
  }

  // Honor -print-file-name=...
  opt::Arg *pfnarg = args_.getLastArg(gollvm::options::OPT_print_file_name_EQ);
  if (pfnarg) {
    llvm::outs() << getFilePath(pfnarg->getValue(), *tc) << "\n";
    exit(0);
  }

  // Honor -print-prog-name=...
  opt::Arg *ppnarg = args_.getLastArg(gollvm::options::OPT_print_prog_name_EQ);
  if (ppnarg) {
    llvm::outs() << getProgramPath(ppnarg->getValue(), *tc) << "\n";
    exit(0);
  }

  // Check for existence of input files.
  for (opt::Arg *arg : args_) {
    if (arg->getOption().getKind() == opt::Option::InputClass) {

      // Special case for "-" (always assumed to exist)
      if (strcmp(arg->getValue(), "-")) {
        std::string fn(arg->getValue());

        // Check for existence of input file.
        if (!sys::fs::exists(fn)) {
          errs() << progname_ << ": error: input file '"
                 << fn << "' does not exist\n";
          return nullptr;
        }
      }
      inputseen = true;
    }
  }

  // Issue an error if no inputs.
  if (! inputseen) {
    if (args_.hasArg(gollvm::options::OPT_v) ||
        args_.hasArg(gollvm::options::OPT__HASH_HASH_HASH))
      exit(0);
    errs() << progname_ << ": error: no inputs\n";
    return nullptr;
  }

  // FIXME: add code to weed out unknown architectures (ex:
  // SomethingWeird-unknown-linux-gnu).

  return tc.get();
}

void Driver::appendInputActions(const inarglist &ifargs,
                                ActionList &result,
                                Compilation &compilation)
{
  for (auto ifarg : ifargs) {
    bool schedAction = false;
    Action *act = nullptr;
    if (!strcmp(ifarg->getValue(), "-")) {
      opt::Arg *xarg = args_.getLastArg(gollvm::options::OPT_x);
      assert(xarg);
      const char *suf =
          (llvm::StringRef(xarg->getValue()).equals("c") ? "c" :
           (llvm::StringRef(xarg->getValue()).equals("go") ? "go" : "?"));
      act = new ReadStdinAction(suf);
      schedAction = true;
    } else {
      act = new InputAction(compilation.newArgArtifact(ifarg));
    }
    compilation.recordAction(act);
    if (schedAction)
      compilation.addAction(act);
    result.push_back(act);
  }
}

bool Driver::buildActions(Compilation &compilation)
{
  inarglist gofiles;
  inarglist asmfiles;
  inarglist linkerinputfiles;
  ActionList linkerInputActions;

  // Examine inputs to see what sort of actions we need.
  for (opt::Arg *arg : args_) {
    if (arg->getOption().getKind() == opt::Option::InputClass) {
      std::string fn(arg->getValue());

      // At the moment the canonical "-" input (stdin) is assumed
      // to be Go source.
      if (!strcmp(arg->getValue(), "-")) {
        gofiles.push_back(arg);
        continue;
      }

      size_t dotindex = fn.find_last_of(".");
      if (dotindex != std::string::npos) {
        std::string extension = fn.substr(dotindex);
        if (extension.compare(".s") == 0) {
          asmfiles.push_back(arg);
          continue;
        } else if (extension.compare(".go") == 0) {
          gofiles.push_back(arg);
          continue;
        } else if (extension.compare(".S") == 0) {
          errs() << progname_ << ": warning: " << arg->getValue()
                 << ": .S files (preprocessed assembler) not supported; "
                 << "treating as linker input.\n";
        }
      }

      // everything else assumed to be a linker input
      linkerinputfiles.push_back(arg);
      continue;
    }
  }

  bool OPT_c = args_.hasArg(gollvm::options::OPT_c);
  bool OPT_S = args_.hasArg(gollvm::options::OPT_S);

  // For -c/-S compiles, a mix of Go and assembly currently not allowed.
  if ((OPT_c || OPT_S) && !gofiles.empty() && !asmfiles.empty()) {
    errs() << progname_ << ": error: cannot specify mix of "
           << "Go and assembly inputs with -c/-S\n";
    return false;
  }

  // Handle Go compilation action if needed.
  if (!gofiles.empty()) {

    // Build a list of input actions for the go files.
    ActionList inacts;
    appendInputActions(gofiles, inacts, compilation);

    // Create action
    Action *gocompact =
        new Action(Action::A_Compile, inacts);
    compilation.recordAction(gocompact);
    compilation.addAction(gocompact);

    // Schedule assemble action now if no -S.
    if (!OPT_S && !args_.hasArg(gollvm::options::OPT_emit_llvm)) {
      // Create action
      Action *asmact =
          new Action(Action::A_Assemble, gocompact);
      compilation.recordAction(asmact);
      compilation.addAction(asmact);
      if (!OPT_c)
        linkerInputActions.push_back(asmact);
    }
  }

  // Create actions to assemble any asm files appearing on the cmd line.
  if (gofiles.empty() && !asmfiles.empty()) {

    // Issue an error if -c in combination with multiple files.
    if (OPT_c && asmfiles.size() > 1) {
      errs() << progname_ << ": error: cannot specify multiple inputs "
             << "in combination with -c\n";
      return false;
    }

    for (auto asmf : asmfiles) {

      // Input action.
      InputAction *ia = new InputAction(compilation.newArgArtifact(asmf));
      compilation.recordAction(ia);

      // Assemble action.
      Action *asmact =
          new Action(Action::A_Assemble, ia);
      compilation.recordAction(asmact);
      compilation.addAction(asmact);
      if (!OPT_c)
        linkerInputActions.push_back(asmact);
    }
  }

  // If -S or -c, we are done at this point.
  if (OPT_c || OPT_S)
    return true;

  // Create a linker action.
  appendInputActions(linkerinputfiles, linkerInputActions, compilation);
  Action *linkact =
          new Action(Action::A_Link, linkerInputActions);
  compilation.recordAction(linkact);
  compilation.addAction(linkact);

  return true;
}

ArtifactList Driver::collectInputArtifacts(Action *act, InternalTool *it)
{
  ArtifactList result;
  for (auto &input : act->inputs()) {
    InputAction *inact = input->castToInputAction();
    if (inact != nullptr) {
      result.push_back(inact->input());
      continue;
    }
    // It is an error if an internal-tool action is receiving a result
    // from an external tool (in the current model all internal-tool actions
    // have to take place before any external-tool actions).
    assert(it == nullptr || input->castToReadStdinAction() != nullptr);
    auto it = artmap_.find(input);
    assert(it != artmap_.end());
    result.push_back(it->second);
  }
  return result;
}

bool Driver::processAction(Action *act, Compilation &compilation, bool lastAct)
{
  // Select the result file for this action.
  Artifact *result = nullptr;
  if (!lastAct) {
    auto tfa = compilation.createTemporaryFileArtifact(act);
    if (!tfa)
      return false;
    result = *tfa;
    artmap_[act] = result;
  } else {
    result = compilation.createOutputFileArtifact(act);
  }

  // Select tool to process the action.
  Tool *tool = compilation.toolchain().getTool(act);
  assert(tool != nullptr);
  InternalTool *it = tool->castToInternalTool();

  // Collect input artifacts for this
  ArtifactList actionInputs = collectInputArtifacts(act, it);

  // If internal tool, perform now.
  if (it != nullptr) {
    if (! it->performAction(compilation, *act, actionInputs, *result))
      return false;
    return true;
  }

  // External tool: build command
  ExternalTool *et = tool->castToExternalTool();
  if (! et->constructCommand(compilation, *act, actionInputs, *result))
    return false;

  return true;
}

bool Driver::processActions(Compilation &compilation)
{
  SmallVector<Action*, 8> actv;
  for (Action *action : compilation.actions())
    actv.push_back(action);

  for (unsigned ai = 0; ai < actv.size(); ++ai) {
    Action *act = actv[ai];
    bool lastAction = (ai == actv.size()-1);
    if (!processAction(act, compilation, lastAction))
      return false;
  }

  return true;
}

} // end namespace driver
} // end namespace gollvm
