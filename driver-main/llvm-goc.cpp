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

static std::unique_ptr<ToolOutputFile>
GetOutputStream(std::string outFileName, bool binary)
{
  // Open the file.
  std::error_code EC;
  sys::fs::OpenFlags OpenFlags = sys::fs::F_None;
  if (!binary)
    OpenFlags |= sys::fs::F_Text;
  auto FDOut = llvm::make_unique<ToolOutputFile>(outFileName, EC,
                                                OpenFlags);
  if (EC) {
    errs() << "error opening " << outFileName << ": "
           << EC.message() << '\n';
    return nullptr;
  }

  return FDOut;
}

class BEDiagnosticHandler : public DiagnosticHandler {
  bool *error_;
 public:
  BEDiagnosticHandler(bool *errorPtr)
      : error_(errorPtr) {}
  bool handleDiagnostics(const DiagnosticInfo &DI) override {
    if (DI.getSeverity() == DS_Error)
      *error_ = true;
    if (auto *Remark = dyn_cast<DiagnosticInfoOptimizationBase>(&DI))
      if (!Remark->isEnabled())
        return true;
    DiagnosticPrinterRawOStream DP(errs());
    errs() << LLVMContext::getDiagnosticMessagePrefix(DI.getSeverity()) << ": ";
    DI.print(DP);
    errs() << "\n";
    return true;
  }
};

// Are we in "assemble" mode (all inputs are .s files) or "compile" mode
// (all inputs are *.go files)?
enum CompileMode {
  UnknownCompileMode,
  CompileAssemblyMode,
  CompileGoMode
};

// Helper class for managing the overall Go compilation process.

class CompilationOrchestrator {
 public:
  CompilationOrchestrator(const char *argvZero,
                          opt::InputArgList &args,
                          opt::OptTable &opts);
  ~CompilationOrchestrator();

  // Various stages in the setup/execution of the compilation.  The
  // convention here is to return false if there was a fatal error, true
  // otherwise.
  bool inspectCommandLine();
  bool preamble();
  bool initBridge();
  bool resolveInputOutput();
  bool invokeFrontEnd();
  bool invokeBridge();
  bool invokeBackEnd();
  bool invokeAssembler();

  // Exit code to return if there was an error in one of the steps above.
  int errorReturnCode() const { return errorReturnCode_; }

  // Temporary: return asm output file.
  const std::string &asmOutFile() const { return asmOutFileName_; }

 private:
  Triple triple_;
  llvm::LLVMContext context_;
  const char *progname_;
  CodeGenOpt::Level cgolvl_;
  unsigned olvl_;
  int errorReturnCode_;
  opt::OptTable &opts_;
  opt::InputArgList &args_;
  std::unique_ptr<Llvm_backend> bridge_;
  std::unique_ptr<TargetMachine> target_;
  std::unique_ptr<Llvm_linemap> linemap_;
  std::unique_ptr<llvm::Module> module_;
  std::vector<std::string> inputFileNames_;
  std::string outFileName_;
  std::string asmOutFileName_;
  std::unique_ptr<ToolOutputFile> asmout_;
  std::unique_ptr<ToolOutputFile> out_;
  std::unique_ptr<TargetLibraryInfoImpl> tlii_;
  CompileMode compileMode_;
  bool hasError_;
  bool tmpCreated_;

  CompileMode compileMode() const { return compileMode_; }
  bool setCompileMode(CompileMode mode) {
    bool mixedModeError = false;
    if (compileMode_ == UnknownCompileMode)
      compileMode_ = mode;
    else if (compileMode_ != mode)
      mixedModeError = true;
    return mixedModeError;
  }

  bool phaseSuccessful() { errorReturnCode_++; return true; }

  void createPasses(legacy::PassManager &MPM,
                    legacy::FunctionPassManager &FPM);
  TargetMachine::CodeGenFileType getOutputFileType();
  template<typename IT>
  llvm::Optional<IT> getLastArgAsInteger(gollvm::options::ID id,
                                         IT defaultValue);
  PICLevel::Level getPicLevel();
  llvm::Optional<llvm::Reloc::Model> reconcileRelocModel();
  bool reconcileOptionPair(gollvm::options::ID yesOption,
                           gollvm::options::ID noOption,
                           bool defaultVal);
  llvm::Optional<llvm::FPOpFusion::FPOpFusionMode> getFPOpFusionMode();
  std::string firstFileBase();
};

CompilationOrchestrator::CompilationOrchestrator(const char *argvZero,
                                                 opt::InputArgList &args,
                                                 opt::OptTable &opts)
    : progname_(argvZero), cgolvl_(CodeGenOpt::Default),
      olvl_(2), errorReturnCode_(1), opts_(opts), args_(args),
      compileMode_(UnknownCompileMode),
      hasError_(false), tmpCreated_(false)
{
  InitializeAllTargets();
  InitializeAllTargetMCs();
  InitializeAllAsmPrinters();
  InitializeAllAsmParsers();
}

CompilationOrchestrator::~CompilationOrchestrator()
{
  if (tmpCreated_)
    sys::fs::remove(asmOutFileName_);
}

TargetMachine::CodeGenFileType CompilationOrchestrator::getOutputFileType()
{
  TargetMachine::CodeGenFileType ft;
  if (args_.hasArg(gollvm::options::OPT_S) ||
      args_.hasArg(gollvm::options::OPT_emit_llvm))
    ft = TargetMachine::CGFT_AssemblyFile;
  else
    ft = TargetMachine::CGFT_ObjectFile;
  return ft;
}

template<typename IT>
llvm::Optional<IT>
CompilationOrchestrator::getLastArgAsInteger(gollvm::options::ID id,
                                             IT defaultValue)
{
  IT result = defaultValue;
  opt::Arg *arg = args_.getLastArg(id);
  if (arg != nullptr) {
    if (llvm::StringRef(arg->getValue()).getAsInteger(10, result)) {
      errs() << progname_ << ": invalid argument '"
             << arg->getValue() << "' to '"
             << arg->getAsString(args_) << "' option\n";
      return None;
    }
  }
  return result;
}

// Return any settings from the -fPIC/-fpic options, if present. The
// intent of the code below is to support "rightmost on the command
// line wins" (compatible with clang and other compilers), so if you
// specify "-fPIC -fpic" you get small PIC, whereas "-fPIC -fpic
// -fPIC" this will give you large PIC.
PICLevel::Level CompilationOrchestrator::getPicLevel()
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
CompilationOrchestrator::reconcileOptionPair(gollvm::options::ID yesOption,
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

llvm::Optional<llvm::Reloc::Model>
CompilationOrchestrator::reconcileRelocModel()
{
  auto picLevel = getPicLevel();
  if (picLevel != llvm::PICLevel::NotPIC) {
    Reloc::Model R = Reloc::PIC_;
    return R;
  }
  return None;
}

llvm::Optional<llvm::FPOpFusion::FPOpFusionMode>
CompilationOrchestrator::getFPOpFusionMode()
{
  opt::Arg *arg = args_.getLastArg(gollvm::options::OPT_ffp_contract_EQ);
  llvm::FPOpFusion::FPOpFusionMode res = FPOpFusion::Standard;
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

std::string CompilationOrchestrator::firstFileBase()
{
  std::string firstFile = inputFileNames_[0];
  size_t dotindex = firstFile.find_last_of(".");
  assert(dotindex != std::string::npos);
  return firstFile.substr(0, dotindex);
}

bool CompilationOrchestrator::inspectCommandLine()
{
  // Collect input file names
  for (opt::Arg *arg : args_) {
    if (arg->getOption().getKind() == opt::Option::InputClass) {
      const char *val = arg->getValue();
      inputFileNames_.push_back(val);
    }
  }

  // Set triple.
  if (const opt::Arg *arg = args_.getLastArg(gollvm::options::OPT_target_EQ))
    triple_ = Triple(Triple::normalize(arg->getValue()));
  else
    triple_ = Triple(sys::getDefaultTargetTriple());

  // Support -march
  std::string archStr;
  opt::Arg *archarg = args_.getLastArg(gollvm::options::OPT_march_EQ);
  if (archarg != nullptr) {
    std::string val(archarg->getValue());
    if (val == "native")
      archStr = sys::getHostCPUName();
    else
      archStr = archarg->getValue();
    triple_.setArchName(archStr);
  }

  return phaseSuccessful();
}

bool CompilationOrchestrator::preamble()
{
  // Get the target specific parser.
  std::string Error;
  const Target *TheTarget =
      TargetRegistry::lookupTarget("", triple_, Error);
  if (!TheTarget) {
    errs() << progname_ << ": " << Error;
    return false;
  }

  // optimization level
  opt::Arg *oarg = args_.getLastArg(gollvm::options::OPT_O_Group);
  if (oarg != nullptr) {
    StringRef lev(oarg->getValue());
    if (lev.size() != 1) {
      errs() << progname_ << ": invalid argument to -O flag: "
             << lev << "\n";
      return false;
    }
    switch (lev[0]) {
      case '0':
        olvl_ = 0;
        cgolvl_ = CodeGenOpt::None;
        break;
      case '1':
        olvl_ = 1;
        cgolvl_ = CodeGenOpt::Less;
        break;
      case 's': // TODO: -Os same as -O for now.
      case '2':
        olvl_ = 2;
        cgolvl_ = CodeGenOpt::Default;
        break;
      case '3':
        olvl_ = 3;
        cgolvl_ = CodeGenOpt::Aggressive;
        break;
      default:
        errs() << progname_ << ": invalid optimization level.\n";
        return false;
    }
  }

  go_no_warn = args_.hasArg(gollvm::options::OPT_w);

  TargetOptions Options;

  // FIXME: turn off integrated assembler for now.
  Options.DisableIntegratedAS = true;

  // FIXME: this hard-wires on the equivalent of -ffunction-sections
  // and -fdata-sections, since there doesn't seem to be a high-level
  // hook for selecting a separate section for a specific variable or
  // function (other than forcing it into a comdat, which is not
  // always what we want).
  Options.FunctionSections = true;
  Options.DataSections = true;
  Options.UniqueSectionNames = true;

  // FIXME: this needs to be dependent on target triple
  Options.EABIVersion = llvm::EABI::Default;

  // init array use
  Options.UseInitArray =
      reconcileOptionPair(gollvm::options::OPT_fuse_init_array,
                          gollvm::options::OPT_fno_use_init_array,
                          true);

  // FP trapping mode
  Options.NoTrappingFPMath =
      reconcileOptionPair(gollvm::options::OPT_ftrapping_math,
                          gollvm::options::OPT_fno_trapping_math,
                          true);


  // The -fno-math-errno option is essentially a no-op when compiling
  // Go code, but -fmath-errno does not make sense, since 'errno' is
  // not exposed in any meaningful way as part of the math package.
  // Allow users to set -fno-math-errno for compatibility reasons, but
  // issue an error if -fmath-errno is set.
  bool mathErrno = reconcileOptionPair(gollvm::options::OPT_fmath_errno,
                                       gollvm::options::OPT_fno_math_errno,
                                       false);
  if (mathErrno) {
    errs() << "error: -fmath-errno unsupported for Go code\n";
    return false;
  }

  // FP contract settings.
  auto dofuse = getFPOpFusionMode();
  if (!dofuse)
    return false;
  Options.AllowFPOpFusion = *dofuse;

  // Support -mcpu
  std::string cpuStr;
  opt::Arg *cpuarg = args_.getLastArg(gollvm::options::OPT_mcpu_EQ);
  if (cpuarg != nullptr) {
    std::string val(cpuarg->getValue());
    if (val == "native")
      cpuStr = sys::getHostCPUName();
    else
      cpuStr = cpuarg->getValue();
  }

  // Features
  SubtargetFeatures features;
  features.getDefaultSubtargetFeatures(triple_);
  std::string featStr = features.getString();

  // Create target machine
  Optional<llvm::CodeModel::Model> CM = None;
  target_.reset(
      TheTarget->createTargetMachine(triple_.getTriple(), cpuStr, featStr,
                                     Options, reconcileRelocModel(),
                                     CM, cgolvl_));
  assert(target_.get() && "Could not allocate target machine!");

  return phaseSuccessful();
}

// This helper performs the various initial steps needed to set up the
// compilation, including prepping the LLVM context, creating an LLVM
// module, creating the bridge itself (Llvm_backend object) and
// setting up the Go frontend via a call to go_create_gogo(). At the
// end of this routine things should be ready to kick off the front end.

bool CompilationOrchestrator::initBridge()
{
  // Set up the LLVM context
  context_.setDiagnosticHandler(
      llvm::make_unique<BEDiagnosticHandler>(&this->hasError_));

  // Construct linemap and module
  linemap_.reset(new Llvm_linemap());
  module_.reset(new llvm::Module("gomodule", context_));

  // Add the target data from the target machine, if it exists
  module_->setTargetTriple(triple_.getTriple());
  module_->setDataLayout(target_->createDataLayout());
  module_->setPICLevel(getPicLevel());

  // Now construct Llvm_backend helper.
  bridge_.reset(new Llvm_backend(context_, module_.get(), linemap_.get()));

  // Honor inline, tracelevel cmd line options
  llvm::Optional<unsigned> tl =
      getLastArgAsInteger(gollvm::options::OPT_tracelevel_EQ, 0u);
  if (!tl)
    return false;
  bridge_->setTraceLevel(*tl);
  bridge_->setNoInline(args_.hasArg(gollvm::options::OPT_fno_inline));

  // Support -fgo-dump-ast
  if (args_.hasArg(gollvm::options::OPT_fgo_dump_ast))
    go_enable_dump("ast");

  // Populate 'args' struct with various bits of information needed by
  // the front end, then pass it to the front end via go_create_gogo().
  struct go_create_gogo_args args;
  unsigned bpi = target_->getPointerSize(0) * 8;
  args.int_type_size = bpi;
  args.pointer_size = bpi;
  opt::Arg *pkpa = args_.getLastArg(gollvm::options::OPT_fgo_pkgpath_EQ);
  args.pkgpath = (pkpa == nullptr ? NULL : pkpa->getValue());
  opt::Arg *pkpp = args_.getLastArg(gollvm::options::OPT_fgo_prefix_EQ);
  args.prefix = (pkpp == nullptr ? NULL : pkpp->getValue());
  opt::Arg *relimp =
      args_.getLastArg(gollvm::options::OPT_fgo_relative_import_path_EQ);
  args.relative_import_path =
      (relimp == nullptr ? NULL : relimp->getValue());
  opt::Arg *chdr =
      args_.getLastArg(gollvm::options::OPT_fgo_c_header_EQ);
  args.c_header = (chdr == nullptr ? NULL : chdr->getValue());
  args.check_divide_by_zero =
      reconcileOptionPair(gollvm::options::OPT_fgo_check_divide_zero,
                          gollvm::options::OPT_fno_go_check_divide_zero,
                          true);
  args.check_divide_overflow =
        reconcileOptionPair(gollvm::options::OPT_fgo_check_divide_overflow,
                            gollvm::options::OPT_fno_go_check_divide_overflow,
                            true);
  args.compiling_runtime =
      args_.hasArg(gollvm::options::OPT_fgo_compiling_runtime);
  llvm::Optional<int> del =
      getLastArgAsInteger(gollvm::options::OPT_fgo_debug_escape_EQ, 0);
  if (!del)
    return false;
  args.debug_escape_level = *del;
  opt::Arg *hasharg =
      args_.getLastArg(gollvm::options::OPT_fgo_debug_escape_hash_EQ);
  args.debug_escape_hash = (hasharg != nullptr ? hasharg->getValue() : NULL);
  args.nil_check_size_threshold = -1;
  args.linemap = linemap_.get();
  args.backend = bridge_.get();
  go_create_gogo (&args);

  /* The default precision for floating point numbers.  This is used
     for floating point constants with abstract type.  This may
     eventually be controllable by a command line option.  */
  mpfr_set_default_prec (256);

  // Escape analysis
  bool enableEscapeAnalysis =
      reconcileOptionPair(gollvm::options::OPT_fgo_optimize_allocs,
                          gollvm::options::OPT_fno_go_optimize_allocs,
                          true);
  go_enable_optimize("allocs", enableEscapeAnalysis ? 1 : 0);

  // Include dirs
  std::vector<std::string> incargs =
      args_.getAllArgValues(gollvm::options::OPT_I);
  for (auto dir : incargs) {
    if (sys::fs::is_directory(dir))
      go_add_search_path(dir.c_str());
  }

  // Library dirs
  // TODO: add version, architecture dirs
  std::vector<std::string> libargs =
      args_.getAllArgValues(gollvm::options::OPT_L);
  for (auto dir : libargs)
    if (sys::fs::is_directory(dir))
      go_add_search_path(dir.c_str());

  return phaseSuccessful();
}

bool CompilationOrchestrator::resolveInputOutput()
{
  // What 'mode' are we operating in? If all inputs are *.s files,
  // then we're in assemble mode; if all inputs are *.go files, we're
  // in compile mode. No support for a mix of *.s and *.go files.
  bool mixedModeError = false;
  if (inputFileNames_.size() == 0) {
    errs() << "error: no input files supplied.\n";
    return false;
  }
  for (auto &fn : inputFileNames_) {
    // Check for existence of input file.
    if (!sys::fs::exists(fn)) {
      errs() << "error: input file '"
             << fn << "' does not exist\n";
      return false;
    }
    size_t dotindex = fn.find_last_of(".");
    if (dotindex == std::string::npos) {
      errs() << "error: malformed input file '"
             << fn << "' (no extension)\n";
      return false;
    }
    std::string extension = fn.substr(dotindex);
    if (extension.compare(".s") == 0) {
      mixedModeError = setCompileMode(CompileAssemblyMode);
      if (mixedModeError)
        break;
    } else if (extension.compare(".go") == 0) {
      mixedModeError = setCompileMode(CompileGoMode);
      if (mixedModeError)
        break;
    } else {
      errs() << "error: unknown input file '"
             << fn << "' (extension must be '.s' or '.go')\n";
      return false;
    }
  }
  if (mixedModeError) {
    errs() << "error: mixing of assembler (*.s) and Go (*.go) "
           << "source files not supported.\n";
    return false;
  }
  if (args_.hasArg(gollvm::options::OPT_S) &&
      compileMode_ == CompileAssemblyMode) {
    errs() << "error: -S option not valid if input files "
           << "are assembly source.\n";
    return false;
  }

  // Decide where we're going to send the output for this compilation.
  // If the "-o" option was not specified, use the basename of the
  // first input argument.
  TargetMachine::CodeGenFileType ft = getOutputFileType();
  opt::Arg *outFileArg = args_.getLastArg(gollvm::options::OPT_o);
  if (outFileArg) {
    outFileName_ = outFileArg->getValue();
  } else {
    outFileName_ = firstFileBase();
    if (ft == TargetMachine::CGFT_AssemblyFile) {
      if (args_.hasArg(gollvm::options::OPT_emit_llvm)) {
        if (args_.hasArg(gollvm::options::OPT_S)) {
          outFileName_ += ".ll";
        } else {
          outFileName_ += ".bc";
        }
      } else {
        outFileName_ += ".s";
      }
    } else {
      outFileName_ += ".o";
    }
  }

  // If we're not compiling to assembly, then we need an intermediate
  // output file into which we'll emit assembly code.
  if (ft != TargetMachine::CGFT_AssemblyFile) {
    if (args_.hasArg(gollvm::options::OPT_save_temps)) {
      asmOutFileName_ = firstFileBase();
      asmOutFileName_ += ".s";
    } else {
      llvm::SmallString<128> tempFileName;
      std::error_code tfcEC =
          llvm::sys::fs::createTemporaryFile("asm", "s", tempFileName);
      if (tfcEC) {
        errs() << tfcEC.message() << "\n";
        return false;
      }
      tmpCreated_ = true;
      sys::RemoveFileOnSignal(tempFileName);
      asmOutFileName_ = std::string(tempFileName.c_str());
    }
  } else {
    asmOutFileName_ = outFileName_;
  }

  // Open the assembler output file
  asmout_ = GetOutputStream(asmOutFileName_, /*text*/ false);
  if (!asmout_)
    return false;

  // Open object file as well if needed
  if (ft != TargetMachine::CGFT_AssemblyFile) {
    out_ = GetOutputStream(outFileName_, /*binary*/ true);
    if (!out_)
      return false;
  }

  return phaseSuccessful();
}

bool CompilationOrchestrator::invokeFrontEnd()
{
  // If we're in "assemble" mode, no need to invoke the compiler
  if (compileMode_ == CompileAssemblyMode)
    return true;

  // Collect the input files and kick off the front end
  // Kick off the front end
  unsigned nfiles = inputFileNames_.size();
  std::unique_ptr<const char *> filenames(new const char *[nfiles]);
  const char **fns = filenames.get();
  unsigned idx = 0;
  for (auto &fn : inputFileNames_)
    fns[idx++] = fn.c_str();
  go_parse_input_files(fns, nfiles, false, true);
  if (!args_.hasArg(gollvm::options::OPT_nobackend))
    go_write_globals();
  if (args_.hasArg(gollvm::options::OPT_dump_ir))
    bridge_->dumpModule();
  if (!args_.hasArg(gollvm::options::OPT_noverify) && !go_be_saw_errors())
    bridge_->verifyModule();
  llvm::Optional<unsigned> tl =
      getLastArgAsInteger(gollvm::options::OPT_tracelevel_EQ, 0u);
  if (*tl)
    std::cerr << "linemap stats:" << linemap_->statistics() << "\n";

  // Delete the bridge at this point. In the case that there were
  // errors, this will help clean up any unreachable llvm Instructions
  // (which would otherwise trigger asserts); in the non-error case it
  // will help to free up bridge-related memory prior to kicking off
  // the pass manager.
  bridge_.reset(nullptr);

  // Early exit at this point if we've seen errors
  if (go_be_saw_errors())
    return false;

  return phaseSuccessful();
}

void CompilationOrchestrator::createPasses(legacy::PassManager &MPM,
                                           legacy::FunctionPassManager &FPM)
{
  if (args_.hasArg(gollvm::options::OPT_disable_llvm_passes))
    return;

  // FIXME: support LTO, ThinLTO, PGO

  PassManagerBuilder pmb;

  // Configure the inliner
  if (args_.hasArg(gollvm::options::OPT_fno_inline) || olvl_ == 0) {
    // Nothing here at the moment. There is go:noinline, but no equivalent
    // of go:alwaysinline.
  } else {
    bool disableInlineHotCallSite = false; // for autofdo, not yet supported
    pmb.Inliner =
        createFunctionInliningPass(olvl_, 2, disableInlineHotCallSite);
  }

  pmb.OptLevel = olvl_;
  pmb.SizeLevel = 0; // TODO: decide on right value here
  pmb.PrepareForThinLTO = false;
  pmb.PrepareForLTO = false;

  FPM.add(new TargetLibraryInfoWrapperPass(*tlii_));
  if (! args_.hasArg(gollvm::options::OPT_noverify))
    FPM.add(createVerifierPass());

  pmb.populateFunctionPassManager(FPM);
  pmb.populateModulePassManager(MPM);
}

bool CompilationOrchestrator::invokeBackEnd()
{
  // If we're in "assemble" mode, no need to invoke the compiler
  if (compileMode_ == CompileAssemblyMode)
    return true;

  tlii_.reset(new TargetLibraryInfoImpl(triple_));

  legacy::PassManager modulePasses;
  legacy::FunctionPassManager functionPasses(module_.get());

  // Set up module and function passes
  if (!args_.hasArg(gollvm::options::OPT_disable_llvm_passes)) {
    modulePasses.add(
        createTargetTransformInfoWrapperPass(target_->getTargetIRAnalysis()));
    functionPasses.add(
        createTargetTransformInfoWrapperPass(target_->getTargetIRAnalysis()));
    createPasses(modulePasses, functionPasses);
  }

  // Add passes to emit bitcode or LLVM IR as appropriate. Here we mimic
  // clang behavior, which is to emit bitcode when "-emit-llvm" is specified
  // but an LLVM IR dump of "-S -emit-llvm" is used.
  raw_pwrite_stream *OS = &asmout_->os();
  if (args_.hasArg(gollvm::options::OPT_emit_llvm)) {
    bool bitcode = !args_.hasArg(gollvm::options::OPT_S);
    bool preserveUseLists =
        reconcileOptionPair(gollvm::options::OPT_emit_llvm_uselists,
                            gollvm::options::OPT_no_emit_llvm_uselists,
                            false);
    modulePasses.add(bitcode ?
                     createBitcodeWriterPass(*OS, preserveUseLists) :
                     createPrintModulePass(*OS, "", preserveUseLists));
  }

  // Set up codegen passes
  legacy::PassManager codeGenPasses;
  if (!args_.hasArg(gollvm::options::OPT_disable_llvm_passes)) {
    codeGenPasses.add(
        createTargetTransformInfoWrapperPass(target_->getTargetIRAnalysis()));

    // Codegen setup
    codeGenPasses.add(new TargetLibraryInfoWrapperPass(*tlii_));
    bool noverify = args_.hasArg(gollvm::options::OPT_noverify);
    TargetMachine::CodeGenFileType ft = TargetMachine::CGFT_AssemblyFile;
    if (target_->addPassesToEmitFile(codeGenPasses, *OS, ft,
                                     /*DisableVerify=*/ noverify)) {
      errs() << "error: unable to interface with target\n";
      return false;
    }
  }

  // Here we go... first function passes
  functionPasses.doInitialization();
  for (Function &F : *module_.get())
    if (!F.isDeclaration())
      functionPasses.run(F);
  functionPasses.doFinalization();

  // ... then module passes
  modulePasses.run(*module_.get());

  // ... and finally code generation
  if (args_.hasArg(gollvm::options::OPT_disable_llvm_passes))
    codeGenPasses.run(*module_.get());

  if (hasError_)
    return false;

  // If -v is in effect, print something to show the effect of the
  // compilation. This is in some sense a fiction, because the top
  // level driver is not invoking a tool to perform the compile, but
  // there is an expectation with compilers that if you take the
  // "-v" output and then execute each command shown by hand, you'll
  // get the same effect as the original command that produced the
  // "-v" output.
  if (args_.hasArg(gollvm::options::OPT_v)) {
    errs() << progname_ << " -S";
    for (auto arg : args_) {
      if (arg->getOption().matches(gollvm::options::OPT_v) ||
          arg->getOption().matches(gollvm::options::OPT_c) ||
          arg->getOption().matches(gollvm::options::OPT_o) ||
          arg->getOption().matches(gollvm::options::OPT_save_temps))
        continue;
      errs() << " " << arg->getAsString(args_);
    }
    errs() << " -o " << asmOutFileName_ << "\n";
  }

  // Keep the resulting output file if -S or -save-temps are in effect.
  if (getOutputFileType() == TargetMachine::CGFT_AssemblyFile ||
      args_.hasArg(gollvm::options::OPT_save_temps))
    asmout_->keep();

  return phaseSuccessful();
}

bool CompilationOrchestrator::invokeAssembler()
{
  if (getOutputFileType() == TargetMachine::CGFT_AssemblyFile)
    return true;

  ArrayRef<StringRef> searchpaths;
  auto aspath = sys::findProgramByName("as", searchpaths);
  if (! aspath ) {
    errs() << "error: unable to locate path for 'as'" << "\n";
    return false;
  }

  // Note: ArgStringList is effectively a vector of "const char *".
  opt::ArgStringList asmcmd;
  asmcmd.push_back("as");
  if (compileMode_ == CompileAssemblyMode) {
    for (auto &fn : inputFileNames_)
      asmcmd.push_back(fn.c_str());
  } else {
    asmcmd.push_back(asmOutFileName_.c_str());
  }
  asmcmd.push_back("-o");
  asmcmd.push_back(outFileName_.c_str());
  args_.AddAllArgValues(asmcmd,
                        gollvm::options::OPT_Wa_COMMA,
                        gollvm::options::OPT_Xassembler);
  opt::Arg *gzarg = args_.getLastArg(gollvm::options::OPT_gz,
                                     gollvm::options::OPT_gz_EQ);
  std::string cds;
  if (gzarg != nullptr) {
    if (gzarg->getOption().matches(gollvm::options::OPT_gz)) {
      asmcmd.push_back("-compress-debug-sections");
    } else {
      cds = "-compress-debug-sections=";
      cds += gzarg->getValue();
      asmcmd.push_back(cds.c_str());
    }
  }
  asmcmd.push_back(nullptr);

  if (args_.hasArg(gollvm::options::OPT_v)) {
    bool first = true;
    for (auto arg : asmcmd) {
      errs() << (first ? "" : " ") << arg;
      first = false;
    }
    errs() << "\n";
  }

  std::string errMsg;
  bool rval = true;
  int rc = sys::ExecuteAndWait(*aspath, asmcmd.data(),
                               /*env=*/nullptr, /*Redirects*/{},
                               /*secondsToWait=*/0,
                               /*memoryLimit=*/0, &errMsg);
  if (rc != 0) {
    errs() << errMsg << "\n";
    rval = false;
  } else {
    out_->keep();
  }

  return (rval ? phaseSuccessful() : false);
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

  CompilationOrchestrator orchestrator(argv[0], clp.args(), *opts.get());

  // Collect input files from the command line, resolve target.
  if (!orchestrator.inspectCommandLine())
    return orchestrator.errorReturnCode();

  // Initialize target
  if (!orchestrator.preamble())
    return orchestrator.errorReturnCode();

  // Set up the bridge
  if (!orchestrator.initBridge())
    return orchestrator.errorReturnCode();

  // Determine output file
  if (!orchestrator.resolveInputOutput())
    return orchestrator.errorReturnCode();

  // Invoke front end
  if (! orchestrator.invokeFrontEnd())
    return orchestrator.errorReturnCode();

  // Invoke back end
  if (! orchestrator.invokeBackEnd())
    return orchestrator.errorReturnCode();

  // Create driver.
  Driver driver(clp.args(), opts.get(), argv[0]);

  // Set up driver, select target and toolchain.
  ToolChain *toolchain = driver.setup();
  if (toolchain == nullptr)
    return 1;

  // Build compilation; construct actions for this compile.
  std::unique_ptr<Compilation> compilation =
      driver.buildCompilation(*toolchain);
  if (!driver.buildActions(*compilation, orchestrator.asmOutFile()))
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
