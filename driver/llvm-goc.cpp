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

#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/Triple.h"
#include "llvm/Analysis/TargetLibraryInfo.h"
#include "llvm/CodeGen/CommandFlags.def"
#include "llvm/IR/DiagnosticInfo.h"
#include "llvm/IR/DiagnosticPrinter.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Verifier.h"
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

#include "GollvmOptions.h"

#include <algorithm>
#include <cstring>
#include <string>
#include <system_error>
#include <sys/types.h>
#include <unistd.h>

using namespace llvm;

static cl::opt<std::string>
TargetTriple("mtriple", cl::desc("Override target triple for module"));

static cl::list<std::string>
InputFilenames(cl::Positional,
               cl::desc("<input go source files>"),
               cl::OneOrMore);

static cl::list<std::string>
IncludeDirs("I", cl::desc("Include directory to be searched for packages."),
            cl::Prefix,
            cl::ZeroOrMore);

static cl::list<std::string>
LibDirs("L", cl::desc("Library directory to be added to search path"),
        cl::Prefix,
        cl::ZeroOrMore);

static cl::list<std::string>
PreprocDefs("D", cl::desc("Preprocessor definitions (currently ignored)."),
            cl::Prefix,
            cl::ZeroOrMore);

// Determine optimization level.
static cl::opt<char>
OptLevel("O",
         cl::desc("Optimization level. [-O0, -O1, -O2, or -O3] "
                  "(default = '-O2')"),
         cl::Prefix,
         cl::ZeroOrMore,
         cl::init(' '));

static cl::opt<std::string>
OutputFileName("o",
               cl::desc("Set name of output file."),
               cl::init(""));

// These are provided for compatibility purposes -- they are currently ignored.
static cl::opt<bool>
M64Option("m64",  cl::desc("Dummy -m64 arg."), cl::init(false), cl::ZeroOrMore);
static cl::opt<bool>
MinusGOption("g",  cl::desc("Dummy -g arg."), cl::init(false), cl::ZeroOrMore);
static cl::opt<bool>
MinusCOption("c",  cl::desc("Dummy -c arg."), cl::init(false), cl::ZeroOrMore);
static cl::opt<bool>
MinusVOption("v",  cl::desc("Dummy -v arg."), cl::init(false), cl::ZeroOrMore);
static cl::opt<bool>
XasmCppOption("xassembler-with-cpp", cl::desc("Dummy -xassembler-with-cpp arg."), cl::init(false), cl::ZeroOrMore);


// Generate assembly and not object file. This is a no-op for now.
static cl::opt<bool>
CapSOption("S",  cl::desc("Emit assembly as opposed to object code."),
           cl::init(false), cl::ZeroOrMore);

static cl::opt<bool>
NoBackend("nobackend",
          cl::desc("Stub out back end invocation."),
          cl::init(false));

static cl::opt<bool>
NoVerify("noverify",
          cl::desc("Stub out module verifier invocation."),
          cl::init(false));

static cl::opt<bool>
CheckDivideZero("fgo-check-divide-zero",
                cl::desc("Add explicit checks for divide-by-zero."),
                cl::init(true));

static cl::opt<bool>
CheckDivideOverflow("fgo-check-divide-overflow",
                    cl::desc("Add explicit checks for division overflow in INT_MIN / -1."),
                    cl::init(true));

static cl::opt<bool>
CompilingRuntime("fgo-compiling-runtime",
                 cl::desc("Compiling the runtime package."),
                 cl::init(false));

static cl::opt<bool>
DumpAst("fgo-dump-ast",
        cl::desc("Dump Go frontend internal AST structure."),
        cl::init(false));

static cl::opt<bool>
DumpIR("dump-ir",
        cl::desc("Dump LLVM IR for module at end of run."),
        cl::init(false));

static cl::opt<bool>
NoInline("fno-inline",
         cl::desc("Disable inlining."),
         cl::init(false));

static cl::opt<bool>
OptimizeAllocs("fgo-optimize-allocs",
               cl::desc("Enable escape analysis in the go frontend."),
               cl::ZeroOrMore,
               cl::init(false));

static cl::opt<bool>
NoOptimizeAllocs("fno-go-optimize-allocs",
               cl::desc("Disable escape analysis in the go frontend."),
               cl::ZeroOrMore,
               cl::init(false));

static cl::opt<std::string>
PackagePath("fgo-pkgpath",
            cl::desc("Set Go package path."),
            cl::init(""));

static cl::opt<std::string>
PackagePrefix("fgo-prefix",
              cl::desc("Set package-specific prefix for exported Go names."),
              cl::init(""));

static cl::opt<std::string>
RelativeImportPath("fgo-relative-import-path",
                   cl::desc("Treat a relative import as relative to path."),
                   cl::init(""));

static cl::opt<std::string>
CHeader("fgo-c-header",
        cl::desc("The C header file to write."),
        cl::init(""));

static cl::opt<int>
EscapeDebugLevel("fgo-debug-escape",
                 cl::desc("Emit debugging information related to the "
                          "escape analysis pass when run with "
                          "-fgo-optimize-allocs."),
                 cl::init(0));

static cl::opt<std::string>
EscapeDebugHash("fgo-debug-escape-hash",
        cl::desc("A hash value to debug escape analysis. Argument is "
                 "a binary string. This runs escape analysis only on "
                 "functions whose names hash to values that match the "
                 "given suffix. Can be used to binary search across "
                 "functions to uncover escape analysis bugs."),
                cl::init(""));

static cl::opt<unsigned>
TraceLevel("tracelevel",
           cl::desc("Set debug trace level (def: 0, no trace output)."),
           cl::init(0));

// Provide a way to turn off the full passmanager (to make it easier
// to triage failures). This should be a temporary flag (not for the
// long term).
static cl::opt<bool>
FullPasses("full-passes",
           cl::desc("Enable all available optimization passes."),
           cl::init(true));

static cl::alias
FFuseFPOps("ffp-contract",
           cl::desc("Alias for -fp-contract"),
           cl::aliasopt(FuseFPOps));

static cl::opt<bool>
NoTrappingMath("fno-trapping-math",
               cl::desc("Generate code with the assumption FP operations will "
                        "not generate user-visible traps."),
               cl::ZeroOrMore,
               cl::init(false));

static cl::opt<bool>
TrappingMath("ftrapping-math",
             cl::desc("Generate code with the assumption FP operations can "
                      "generate user-visible traps."),
             cl::ZeroOrMore,
             cl::init(false));

static cl::opt<bool>
NoMathErrno("fno-math-errno",
            cl::desc("Do not require that math functions set "
                     "'errno' to indicate errors. This option has no "
                     "effect (provided for compatibility purposes)."),
            cl::ZeroOrMore,
            cl::init(false));

static cl::opt<bool>
MathErrno("fmath-errno",
          cl::desc("Insure that calls to math functions that incur errors "
                   "results in setting of 'errno' (unimplemented, will "
                   "generate an error if not overridden with a subsequent "
                   "instance of -fno-math-errno)."),
          cl::ZeroOrMore,
          cl::init(false));

// Given a pair of cl::opt objects corresponding to -fXXX and -fno-XXX
// boolean flags, select the correct value for the option depending on
// the relative position of the options on the command line (rightmost
// wins). For example, given -fblah -fno-blah -fblah, we want the same
// semantics as a single -fblah.

static bool reconcileOptionPair(cl::opt<bool> &yesOption,
                                cl::opt<bool> &noOption,
                                bool defaultVal)
{
  bool value = defaultVal;
  if (yesOption.getNumOccurrences() > 0) {
    if (noOption.getNumOccurrences() > 0) {
      value = yesOption.getPosition() > noOption.getPosition();
    } else {
      value = true;
    }
  } else {
    if (noOption.getNumOccurrences() > 0) {
      value = false;
    }
  }
  return value;
}

static cl::opt<bool>
PICSmall("fpic",
        cl::desc("Generate position-independent code (small model)."),
        cl::ZeroOrMore,
        cl::init(false));

static cl::opt<bool>
PICBig("fPIC",
        cl::desc("Generate position-independent code (large model)."),
        cl::ZeroOrMore,
        cl::init(false));

// Return any settings from the -fPIC/-fpic options, if present. The
// intent of the code below is to support "rightmost on the command
// line wins" (compatible with clang and other compilers), so if you
// specify "-fPIC -fpic" you get small PIC, whereas "-fPIC -fpic
// -fPIC" this will give you large PIC.
static PICLevel::Level getPicLevel()
{
  auto picLevel = PICLevel::NotPIC;
  if (PICBig && PICSmall.getPosition() < PICBig.getPosition())
    picLevel = PICLevel::BigPIC;
  else if (PICSmall && PICSmall.getPosition() > PICBig.getPosition())
    picLevel = PICLevel::SmallPIC;
  return picLevel;
}

static PICLevel::Level reconcilePicLevel()
{
  // FIXME: as a result of including CodeGen/CommandFlags.def, we have
  // both the -relocation-model option as well as -fpic/-fPIC. For
  // now, honor the former first, then the latter (this behavior will
  // eventually go away once we no longer use CommandFlags.def).
  if (RelocModel.getNumOccurrences() > 0) {
    if (RelocModel == llvm::Reloc::Static)
      return llvm::PICLevel::NotPIC;
    assert(RelocModel == llvm::Reloc::PIC_);
    return llvm::PICLevel::BigPIC;
  }
  return getPicLevel();
}

static llvm::Optional<llvm::Reloc::Model> reconcileRelocModel()
{
  // FIXME: again, as a result of including CodeGen/CommandFlags.def,
  // both the -relocation-model option as well as -fpic/-fPIC.  Honor
  // the first the former, then the latter.
  if (RelocModel.getNumOccurrences()) {
    llvm::Reloc::Model R = RelocModel;
    return R;
  }
  auto picLevel = getPicLevel();
  if (picLevel != llvm::PICLevel::NotPIC) {
    Reloc::Model R = Reloc::PIC_;
    return R;
  }
  return None;
}

static TargetMachine::CodeGenFileType getOutputFileType()
{
  // FIXME: as a result of including CodeGen/CommandFlags.def, we have
  // both the -filetype option as well as "-S". For now, honor the
  // former first, then the latter (eventually we'll want to move away
  // from CommandFlags.def).
  TargetMachine::CodeGenFileType ft;
  if (FileType.getNumOccurrences() > 0)
    ft = FileType;
  else if (CapSOption.getNumOccurrences() > 0 && CapSOption)
    ft = TargetMachine::CGFT_AssemblyFile;
  else
    ft = TargetMachine::CGFT_ObjectFile;
  return ft;
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
  CompilationOrchestrator(const char *argvZero);
  ~CompilationOrchestrator();

  // Various stages in the setup/execution of the compilation.  The
  // convention here is to return false if there was a fatal error, true
  // otherwise.
  bool parseCommandLine(int argc, char **argv);
  bool preamble();
  bool initBridge();
  bool resolveInputOutput();
  bool invokeFrontEnd();
  bool invokeBridge();
  bool invokeBackEnd();
  bool invokeAssembler();

  // Exit code to return if there was an error in one of the steps above.
  int errorReturnCode() const { return errorReturnCode_; }

 private:
  Triple triple_;
  llvm::LLVMContext context_;
  const char *progname_;
  CodeGenOpt::Level cgolvl_;
  unsigned olvl_;
  int errorReturnCode_;
  std::unique_ptr<opt::OptTable> opts_;
  opt::InputArgList args_;
  std::unique_ptr<Llvm_backend> bridge_;
  std::unique_ptr<TargetMachine> target_;
  std::unique_ptr<Llvm_linemap> linemap_;
  std::unique_ptr<llvm::Module> module_;
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
};

CompilationOrchestrator::CompilationOrchestrator(const char *argvZero)

    : progname_(argvZero), cgolvl_(CodeGenOpt::Default),
      olvl_(2), errorReturnCode_(1),
      opts_(gollvm::options::createGollvmDriverOptTable()),
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

bool CompilationOrchestrator::parseCommandLine(int argc, char **argv)
{
  SmallVector<const char *, 256> argvsmv;
  SpecificBumpPtrAllocator<char> ArgAllocator;
  std::error_code EC = sys::Process::GetArgumentVector(
      argvsmv, makeArrayRef(argv, argc), ArgAllocator);
  if (EC) {
    errs() << "error: couldn't get arguments: " << EC.message() << '\n';
    return false;
  }
  ArrayRef<const char *> argvv(argvsmv);

  unsigned missingArgIndex, missingArgCount;
  this->args_ =
      opts_->ParseArgs(argvv.slice(1), missingArgIndex, missingArgCount);

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

  // Temporary until we can migrate over to new parsing scheme.
  cl::ParseCommandLineOptions(argc, argv, "gollvm driver\n");

  return phaseSuccessful();
}

bool CompilationOrchestrator::preamble()
{
  triple_ = Triple(Triple::normalize(TargetTriple));
  if (triple_.getTriple().empty())
    triple_.setTriple(sys::getDefaultTargetTriple());

  // Get the target specific parser.
  std::string Error;
  const Target *TheTarget =
      TargetRegistry::lookupTarget(MArch, triple_, Error);
  if (!TheTarget) {
    errs() << progname_ << ": " << Error;
    return false;
  }

  // FIXME: cpu, features not yet supported
  std::string CPUStr = getCPUStr(), FeaturesStr = getFeaturesStr();

  // optimization level
  switch (OptLevel) {
    default:
      errs() << progname_ << ": invalid optimization level.\n";
      return false;
    case '0':
      olvl_ = 0;
      cgolvl_ = CodeGenOpt::None;
      break;
    case '1':
      olvl_ = 1;
      cgolvl_ = CodeGenOpt::Less;
      break;
    case 's': // TODO: same as -O2 for now.
    case ' ':
    case '2':
      break;
    case '3':
      olvl_ = 3;
      cgolvl_ = CodeGenOpt::Aggressive;
      break;
  }

  go_no_warn = NoWarn;

  TargetOptions Options = InitTargetOptionsFromCodeGenFlags();

  // FIXME: turn off integrated assembler for now.
  Options.DisableIntegratedAS = true;

  // FIXME: this hard-wires on the equivalent of -ffunction-sections
  // and -fdata-sections, since there doesn't seem to be a high-level
  // hook for selecting a separate section for a specific variable or
  // function (other than forcing it into a comdat, which is not
  // always what we want).
  Options.FunctionSections = true;
  Options.DataSections = true;

  // FP trapping mode
  Options.NoTrappingFPMath = !reconcileOptionPair(TrappingMath,
                                                  NoTrappingMath,
                                                  true);

  // The -fno-math-errno option is essentially a no-op when compiling
  // Go code, but -fmath-errno does not make sense, since 'errno' is
  // not exposed in any meaningful way as part of the math package.
  // Allow users to set -fno-math-errno for compatibility reasons, but
  // issue an error if -fmath-errno is set.
  bool mathErrno = reconcileOptionPair(MathErrno, NoMathErrno, false);
  if (mathErrno) {
    errs() << "error: -fmath-errno unsupported for Go code\n";
    return false;
  }

  // FP contract settings.
  Options.AllowFPOpFusion = FuseFPOps;

  // FIXME: get rid of -fp-contract in favor of -ffp-contract
  // FIXME: allow multiple -ffp-contract=XXX settings on the command line.

  // Create target machine
  Optional<llvm::CodeModel::Model> CM = None;
  target_.reset(
      TheTarget->createTargetMachine(triple_.getTriple(), CPUStr, FeaturesStr,
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

  // Vett relocation model.
  if (RelocModel.getNumOccurrences() > 0 &&
      (RelocModel != llvm::Reloc::Static &&
       RelocModel != llvm::Reloc::PIC_)) {
    errs() << "error: unsupported -relocation-model selection\n";
    return false;
  }

  // Construct linemap and module
  linemap_.reset(new Llvm_linemap());
  module_.reset(new llvm::Module("gomodule", context_));

  // Add the target data from the target machine, if it exists
  module_->setTargetTriple(triple_.getTriple());
  module_->setDataLayout(target_->createDataLayout());
  module_->setPICLevel(reconcilePicLevel());

  // Now construct Llvm_backend helper.
  bridge_.reset(new Llvm_backend(context_, module_.get(), linemap_.get()));

  // Honor inline, tracelevel cmd line options
  bridge_->setTraceLevel(TraceLevel);
  bridge_->setNoInline(NoInline);

  // Support -fgo-dump-ast
  if (DumpAst)
    go_enable_dump("ast");

  // Populate 'args' struct with various bits of information needed by
  // the front end, then pass it to the front end via go_create_gogo().
  struct go_create_gogo_args args;
  unsigned bpi = target_->getPointerSize(0) * 8;
  args.int_type_size = bpi;
  args.pointer_size = bpi;
  args.pkgpath = PackagePath.empty() ? NULL : PackagePath.c_str();
  args.prefix = PackagePrefix.empty() ? NULL : PackagePrefix.c_str();
  args.relative_import_path =
      RelativeImportPath.empty() ? NULL : RelativeImportPath.c_str();
  args.c_header = CHeader.empty() ? NULL : CHeader.c_str();
  args.check_divide_by_zero = CheckDivideZero;
  args.check_divide_overflow = CheckDivideOverflow;
  args.compiling_runtime = CompilingRuntime;
  args.debug_escape_level = EscapeDebugLevel;
  args.debug_escape_hash = nullptr;
  if (!EscapeDebugHash.empty())
    args.debug_escape_hash = EscapeDebugHash.c_str();
  args.nil_check_size_threshold = -1;
  args.linemap = linemap_.get();
  args.backend = bridge_.get();
  go_create_gogo (&args);

  /* The default precision for floating point numbers.  This is used
     for floating point constants with abstract type.  This may
     eventually be controllable by a command line option.  */
  mpfr_set_default_prec (256);

  // Escape analysis
  bool enableEscapeAnalysis = reconcileOptionPair(OptimizeAllocs,
                                                  NoOptimizeAllocs,
                                                  true);
  go_enable_optimize("allocs", enableEscapeAnalysis ? 1 : 0);

  // Include dirs
  if (! IncludeDirs.empty()) {
    for (auto dir : IncludeDirs) {
      if (sys::fs::is_directory(dir))
        go_add_search_path(dir.c_str());
    }
  }

  // Library dirs
  // TODO: add version, architecture dirs
  if (! LibDirs.empty()) {
    for (auto dir : LibDirs) {
      struct stat st;
      if (stat (dir.c_str(), &st) == 0 && S_ISDIR (st.st_mode))
        go_add_search_path(dir.c_str());
    }
  }

  return phaseSuccessful();
}

bool CompilationOrchestrator::resolveInputOutput()
{
  // What 'mode' are we operating in? If all inputs are *.s files,
  // then we're in assemble mode; if all inputs are *.go files, we're
  // in compile mode. No support for a mix of *.s and *.go files.
  bool mixedModeError = false;
  if (InputFilenames.size() == 0) {
    errs() << "error: no input files supplied.\n";
    return false;
  }
  for (auto &fn : InputFilenames) {
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
  if (CapSOption && compileMode_ == CompileAssemblyMode) {
    errs() << "error: -S option not valid if input files "
           << "are assembly source.\n";
    return false;
  }

  // Decide where we're going to send the output for this compilation.
  // If the "-o" option was not specified, use the basename of the
  // first input argument.
  outFileName_ = OutputFileName;
  TargetMachine::CodeGenFileType ft = getOutputFileType();
  if (OutputFileName.empty()) {
    std::string firstFile = InputFilenames[0];
    size_t dotindex = firstFile.find_last_of(".");
    assert(dotindex != std::string::npos);
    outFileName_ = firstFile.substr(0, dotindex);
    outFileName_ += (ft == TargetMachine::CGFT_AssemblyFile ? ".s" : ".o");
  }

  // If we're not compiling to assembly, then we need an intermediate
  // output file into which we'll emit assembly code.
  if (ft != TargetMachine::CGFT_AssemblyFile) {
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
  unsigned nfiles = InputFilenames.size();
  std::unique_ptr<const char *> filenames(new const char *[nfiles]);
  const char **fns = filenames.get();
  unsigned idx = 0;
  for (auto &fn : InputFilenames)
    fns[idx++] = fn.c_str();
  go_parse_input_files(fns, nfiles, false, true);
  if (! NoBackend)
    go_write_globals();
  if (DumpIR)
    bridge_->dumpModule();
  if (! NoVerify && !go_be_saw_errors())
    bridge_->verifyModule();
  if (TraceLevel)
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
  // FIXME: support LTO, ThinLTO, PGO

  PassManagerBuilder pmb;

  // Configure the inliner
  if (NoInline || olvl_ == 0) {
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
  if (! NoVerify)
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

  // Set up module and function passes
  legacy::PassManager modulePasses;
  modulePasses.add(
      createTargetTransformInfoWrapperPass(target_->getTargetIRAnalysis()));
  legacy::FunctionPassManager functionPasses(module_.get());
  functionPasses.add(
      createTargetTransformInfoWrapperPass(target_->getTargetIRAnalysis()));
  createPasses(modulePasses, functionPasses);

  // Set up codegen passes
  legacy::PassManager codeGenPasses;
  codeGenPasses.add(
      createTargetTransformInfoWrapperPass(target_->getTargetIRAnalysis()));

  // Codegen setup
  raw_pwrite_stream *OS = &asmout_->os();
  codeGenPasses.add(new TargetLibraryInfoWrapperPass(*tlii_));
  if (target_->addPassesToEmitFile(codeGenPasses, *OS, FileType,
                                   /*DisableVerify=*/ NoVerify)) {
    errs() << "error: unable to interface with target\n";
    return false;
  }

  // Before executing passes, print the final values of the LLVM options.
  cl::PrintOptionValues();

  // Temporary -- to be removed in a follow-on CL
  if (FullPasses) {

    // Here we go... first function passes
    functionPasses.doInitialization();
    for (Function &F : *module_.get())
      if (!F.isDeclaration())
        functionPasses.run(F);
    functionPasses.doFinalization();

    // ... then module passes
    modulePasses.run(*module_.get());
  }

  // ... and finally code generation
  codeGenPasses.run(*module_.get());

  if (hasError_)
    return false;

  // Keep the resulting output file if -S is in effect.
  if (getOutputFileType() == TargetMachine::CGFT_AssemblyFile)
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

  std::vector<const char *> args;
  args.push_back("as");
  if (compileMode_ == CompileAssemblyMode) {
    for (auto &fn : InputFilenames)
      args.push_back(fn.c_str());
  } else {
    args.push_back(asmOutFileName_.c_str());
  }
  args.push_back("-o");
  args.push_back(outFileName_.c_str());
  args.push_back(nullptr);

  std::string errMsg;
  bool rval = true;
  int rc = sys::ExecuteAndWait(*aspath, args.data(),
                               /*env=*/nullptr, /*Redirects*/{},
                               /*secondsToWait=*/0,
                               /*memoryLimit=*/0, &errMsg);
  if (rc != 0) {
    errs() << errMsg << "\n";
    rval = false;
  } else {
    out_->keep();
  }

  return (rval ? true : phaseSuccessful());
}

int main(int argc, char **argv)
{
  CompilationOrchestrator orchestrator(argv[0]);

  // Print a stack trace if we signal out.
  sys::PrintStackTraceOnErrorSignal(argv[0]);
  PrettyStackTraceProgram X(argc, argv);
  llvm_shutdown_obj Y;  // Call llvm_shutdown() on exit.

  // Parse command line.
  if (!orchestrator.parseCommandLine(argc, argv))
    return orchestrator.errorReturnCode();

  // Initialize target and sort out selected command line options.
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

  // Invoke assembler if needed.
  if (! orchestrator.invokeAssembler())
    return orchestrator.errorReturnCode();

  // We're done.
  return 0;
}
