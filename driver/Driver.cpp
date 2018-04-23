//===-- Driver.cpp --------------------------------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
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
#include "Driver.h"
#include "Compilation.h"
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
}

Driver::~Driver()
{
}

std::string Driver::getFilePath(llvm::StringRef name,
                                ToolChain &toolchain)
{
  // to be implemented in a later patch
  assert(false);
  return "";
}

std::string Driver::getProgramPath(llvm::StringRef name,
                                   ToolChain &toolchain)
{
  // to be implemented in a later patch
  assert(false);
  return "";
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

// Return any settings from the -fPIC/-fpic options, if present. The
// intent of the code below is to support "rightmost on the command
// line wins" (compatible with clang and other compilers), so if you
// specify "-fPIC -fpic" you get small PIC, whereas "-fPIC -fpic
// -fPIC" this will give you large PIC.
PICLevel::Level Driver::getPicLevel()
{
  opt::Arg *arg = args_.getLastArg(gollvm::options::OPT_fpic,
                                   gollvm::options::OPT_fno_pic,
                                   gollvm::options::OPT_fPIC,
                                   gollvm::options::OPT_fno_PIC);
  if (arg == nullptr)
    return PICLevel::NotPIC;
  if (arg->getOption().matches(gollvm::options::OPT_fpic))
    return PICLevel::SmallPIC;
  else if (arg->getOption().matches(gollvm::options::OPT_fPIC))
    return PICLevel::BigPIC;
  return PICLevel::NotPIC;
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
  // to be implemented in a later patch
  return nullptr;
}

ActionList Driver::createInputActions(const inarglist &ifargs,
                                      Compilation &compilation)
{
  // to be implemented in a later patch
  return ActionList();
}

bool Driver::buildActions(Compilation &compilation)
{
  // to be implemented in a later patch
  assert(false);
  return false;
}

bool Driver::processActions(Compilation &compilation)
{
  // to be implemented in a later patch
  assert(false);
  return false;
}

} // end namespace driver
} // end namespace gollvm
