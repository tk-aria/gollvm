//===-- llvm-goparse.cpp - Debug test driver for go parser for llvm  ------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// Invokes the gofrontend parser on specified input files.
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
#include "llvm/Passes/PassBuilder.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/Format.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/ManagedStatic.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/PrettyStackTrace.h"
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
#include <sys/stat.h>
#include <unistd.h>

using namespace llvm;

static cl::opt<std::string>
TargetTriple("mtriple", cl::desc("Override target triple for module"));

static cl::list<std::string>
InputFilenames(cl::Positional,
               cl::desc("<input go source files>"),
               cl::OneOrMore);

static cl::opt<std::string>
IncludeDirs("I", cl::desc("<include dirs>"));

static cl::opt<std::string>
LibDirs("L", cl::desc("<system library dirs>"));

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

static std::unique_ptr<ToolOutputFile>
GetOutputStream() {
  // Decide if we need "binary" output.
  bool Binary = false;
  switch (FileType) {
  case TargetMachine::CGFT_AssemblyFile:
    break;
  case TargetMachine::CGFT_ObjectFile:
  case TargetMachine::CGFT_Null:
    Binary = true;
    break;
  }

  // Open the file.
  std::error_code EC;
  sys::fs::OpenFlags OpenFlags = sys::fs::F_None;
  if (!Binary)
    OpenFlags |= sys::fs::F_Text;
  auto FDOut = llvm::make_unique<ToolOutputFile>(OutputFileName, EC,
                                                   OpenFlags);
  if (EC) {
    errs() << "error opening " << OutputFileName << ": "
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

// Helper class for managing the overall Go compilation process.

class CompilationOrchestrator {
 public:
  CompilationOrchestrator(const char *argvZero);
  ~CompilationOrchestrator() { }

  // Various stages in the setup/execution of the compilation.  The
  // convention here is to return false if there was a fatal error, true
  // otherwise.
  bool preamble();
  bool initBridge();
  bool invokeFrontEnd();
  bool invokeBridge();
  bool invokeBackEnd();

 private:
  Triple triple_;
  llvm::LLVMContext context_;
  const char *progname_;
  CodeGenOpt::Level cgolvl_;
  unsigned olvl_;
  std::unique_ptr<Llvm_backend> bridge_;
  std::unique_ptr<TargetMachine> target_;
  std::unique_ptr<Llvm_linemap> linemap_;
  std::unique_ptr<llvm::Module> module_;
  std::unique_ptr<ToolOutputFile> out_;
  std::unique_ptr<TargetLibraryInfoImpl> tlii_;
  bool hasError_;

  void createPasses(legacy::PassManager &MPM,
                    legacy::FunctionPassManager &FPM);
};

CompilationOrchestrator::CompilationOrchestrator(const char *argvZero)

    : progname_(argvZero), cgolvl_(CodeGenOpt::Default),
      olvl_(2), hasError_(false)
{
  InitializeAllTargets();
  InitializeAllTargetMCs();
  InitializeAllAsmPrinters();
  InitializeAllAsmParsers();
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
                                     Options, getRelocModel(), CM, cgolvl_));
  assert(target_.get() && "Could not allocate target machine!");

  return true;
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
  unsigned bpi = target_->getPointerSize() * 8;
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
  if (OptimizeAllocs)
    go_enable_optimize("allocs");

  // Include dirs
  if (! IncludeDirs.empty()) {
    std::stringstream ss(IncludeDirs);
    std::string dir;
    while(std::getline(ss, dir, ':')) {
      struct stat st;
      if (stat (dir.c_str(), &st) == 0 && S_ISDIR (st.st_mode))
        go_add_search_path(dir.c_str());
    }
  }

  // Library dirs
  // TODO: add version, architecture dirs
  if (! LibDirs.empty()) {
    std::stringstream ss(LibDirs);
    std::string dir;
    while(std::getline(ss, dir, ':')) {
      struct stat st;
      if (stat (dir.c_str(), &st) == 0 && S_ISDIR (st.st_mode))
        go_add_search_path(dir.c_str());
    }
  }

  // Figure out where we are going to send the output.
  if (OutputFileName.empty()) {
    errs() << "no output file specified (please use -o option)\n";
    return false;
  }
  out_ = GetOutputStream();
  if (!out_)
    return false;

  return true;
}

bool CompilationOrchestrator::invokeFrontEnd()
{
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

  return true;
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
  raw_pwrite_stream *OS = &out_->os();
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

  // Declare success.
  out_->keep();

  return true;
}

int main(int argc, char **argv)
{
  CompilationOrchestrator orchestrator(argv[0]);

  cl::ParseCommandLineOptions(argc, argv, "gollvm driver\n");

  // Print a stack trace if we signal out.
  sys::PrintStackTraceOnErrorSignal(argv[0]);
  PrettyStackTraceProgram X(argc, argv);
  llvm_shutdown_obj Y;  // Call llvm_shutdown() on exit.

  // Initialize target and sort out selected command line options.
  if (!orchestrator.preamble())
    return 1;

  // Set up the bridge
  if (!orchestrator.initBridge())
    return 2;

  // Invoke front end
  if (! orchestrator.invokeFrontEnd())
    return 3;

  // Invoke back end
  if (! orchestrator.invokeBackEnd())
    return 4;

  // We're done.
  return 0;
}
