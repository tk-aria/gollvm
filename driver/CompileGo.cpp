//===-- CompileGo.cpp ------------------------------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// Gollvm driver helper class "CompileGo" methods.
//
//===----------------------------------------------------------------------===//

#include "CompileGo.h"

#include "go-llvm-linemap.h"
#include "go-llvm-diagnostics.h"
#include "go-llvm.h"
#include "go-c.h"
#include "mpfr.h"
#include "GollvmOptions.h"

#include "Action.h"
#include "Artifact.h"
#include "Driver.h"
#include "ToolChain.h"

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

using namespace llvm;

namespace gollvm {
namespace driver {

class CompileGoImpl {
 public:
  CompileGoImpl(ToolChain &tc, const std::string &executablePath);

  // Perform compilation.
  bool performAction(Compilation &compilation,
                     const Action &jobAction,
                     const ArtifactList &inputArtifacts,
                     const Artifact &output);

 private:
  Triple triple_;
  const ToolChain &toolchain_;
  Driver &driver_;
  LLVMContext context_;
  const char *progname_;
  std::string executablePath_;
  opt::InputArgList &args_;
  CodeGenOpt::Level cgolvl_;
  unsigned olvl_;
  bool hasError_;
  std::unique_ptr<Llvm_backend> bridge_;
  std::unique_ptr<TargetMachine> target_;
  std::unique_ptr<Llvm_linemap> linemap_;
  std::unique_ptr<Module> module_;
  std::vector<std::string> inputFileNames_;
  std::string asmOutFileName_;
  std::unique_ptr<ToolOutputFile> asmout_;
  std::unique_ptr<TargetLibraryInfoImpl> tlii_;

  void createPasses(legacy::PassManager &MPM,
                    legacy::FunctionPassManager &FPM);

  // This routine emits output for -### and/or -v, then returns TRUE
  // of the compilation should be stubbed out (-###) or FALSE otherwise.
  bool preamble(const Artifact &output);

  // The routines below return TRUE for success, FALSE for failure/error/
  bool setup();
  bool initBridge();
  bool invokeFrontEnd();
  bool invokeBridge();
  bool invokeBackEnd();
  bool resolveInputOutput(const Action &jobAction,
                          const ArtifactList &inputArtifacts,
                          const Artifact &output);
};

CompileGoImpl::CompileGoImpl(ToolChain &tc, const std::string &executablePath)
    : triple_(tc.driver().triple()),
      toolchain_(tc),
      driver_(tc.driver()),
      progname_(tc.driver().progname()),
      executablePath_(executablePath),
      args_(tc.driver().args()),
      cgolvl_(CodeGenOpt::Default),
      olvl_(2),
      hasError_(false)
{
  InitializeAllTargets();
  InitializeAllTargetMCs();
  InitializeAllAsmPrinters();
  InitializeAllAsmParsers();
}

bool CompileGoImpl::performAction(Compilation &compilation,
                                  const Action &jobAction,
                                  const ArtifactList &inputArtifacts,
                                  const Artifact &output)
{
  if (preamble(output))
    return true;

  // Resolve input/output files.
  if (!resolveInputOutput(jobAction, inputArtifacts, output))
    return false;

  // Setup
  if (!setup())
    return false;

  // Set up the bridge
  if (!initBridge())
    return false;

  // Invoke front end
  if (!invokeFrontEnd())
    return false;

  // Invoke back end
  if (!invokeBackEnd())
    return false;

  return true;
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

bool CompileGoImpl::preamble(const Artifact &output)
{
  // If -v is in effect, print something to show the effect of the
  // compilation. This is in some sense a fiction, because the top
  // level driver is not invoking an external tool to perform the
  // compile, but there is an expectation with compilers that if you
  // take the "-v" output and then execute each command shown by hand,
  // you'll get the same effect as the original command that produced
  // the "-v" output.
  bool hashHashHash = args_.hasArg(gollvm::options::OPT__HASH_HASH_HASH);
  const char *qu = (hashHashHash ? "\"" : "");
  if (args_.hasArg(gollvm::options::OPT_v) || hashHashHash) {
    errs() << " " << executablePath_ << " " << qu << "-S" << qu;
    for (auto arg : args_) {
      if (arg->getOption().matches(gollvm::options::OPT_v) ||
          arg->getOption().matches(gollvm::options::OPT_c) ||
          arg->getOption().matches(gollvm::options::OPT_o) ||
          arg->getOption().matches(gollvm::options::OPT__HASH_HASH_HASH) ||
          arg->getOption().matches(gollvm::options::OPT_save_temps))
        continue;
      errs() << " " << qu << arg->getAsString(args_) << qu;
    }
    errs() << " " << qu << "-o" << qu << " " << qu << output.file() << qu
           << "\n";
  }

  return hashHashHash;
}

bool CompileGoImpl::resolveInputOutput(const Action &jobAction,
                                       const ArtifactList &inputArtifacts,
                                       const Artifact &output)
{
  // Collect input files
  for (auto inp : inputArtifacts)
    inputFileNames_.push_back(inp->file());
  assert(! inputFileNames_.empty());
  asmOutFileName_ = output.file();

  // Open output file.
  std::error_code EC;
  sys::fs::OpenFlags OpenFlags = sys::fs::F_Text;
  auto FDOut = llvm::make_unique<ToolOutputFile>(asmOutFileName_, EC,
                                                 OpenFlags);
  if (EC) {
    errs() << progname_ << ": error opening " << asmOutFileName_ << ": "
           << EC.message() << '\n';
    return false;
  }

  asmout_.reset(FDOut.release());
  asmout_->keep();
  return true;
}

bool CompileGoImpl::setup()
{
  // Set triple.
  triple_ = driver_.triple();

  // Get the target specific parser.
  std::string Error;
  const Target *TheTarget =
      TargetRegistry::lookupTarget("", triple_, Error);
  if (!TheTarget) {
    errs() << progname_ << Error;
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
      driver_.reconcileOptionPair(gollvm::options::OPT_fuse_init_array,
                                  gollvm::options::OPT_fno_use_init_array,
                                  true);

  // FP trapping mode
  Options.NoTrappingFPMath =
      driver_.reconcileOptionPair(gollvm::options::OPT_ftrapping_math,
                                  gollvm::options::OPT_fno_trapping_math,
                                  false);


  // The -fno-math-errno option is essentially a no-op when compiling
  // Go code, but -fmath-errno does not make sense, since 'errno' is
  // not exposed in any meaningful way as part of the math package.
  // Allow users to set -fno-math-errno for compatibility reasons, but
  // issue an error if -fmath-errno is set.
  bool mathErrno =
      driver_.reconcileOptionPair(gollvm::options::OPT_fmath_errno,
                                  gollvm::options::OPT_fno_math_errno,
                                  false);
  if (mathErrno) {
    errs() << "error: -fmath-errno unsupported for Go code\n";
    return false;
  }

  // FP contract settings.
  auto dofuse = driver_.getFPOpFusionMode();
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

  // Features.
  // FIXME: incorporate command line flags.
  SubtargetFeatures features;
  features.getDefaultSubtargetFeatures(triple_);
  std::string featStr = features.getString();

  // Create target machine
  Optional<llvm::CodeModel::Model> CM = None;
  target_.reset(
      TheTarget->createTargetMachine(triple_.getTriple(), cpuStr, featStr,
                                     Options, driver_.reconcileRelocModel(),
                                     CM, cgolvl_));
  assert(target_.get() && "Could not allocate target machine!");

  return true;
}

// This helper performs the various initial steps needed to set up the
// compilation, including prepping the LLVM context, creating an LLVM
// module, creating the bridge itself (Llvm_backend object) and
// setting up the Go frontend via a call to go_create_gogo(). At the
// end of this routine things should be ready to kick off the front end.

bool CompileGoImpl::initBridge()
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
  module_->setPICLevel(driver_.getPicLevel());

  // Now construct Llvm_backend helper.
  bridge_.reset(new Llvm_backend(context_, module_.get(), linemap_.get()));

  // Honor inline, tracelevel cmd line options
  llvm::Optional<unsigned> tl =
      driver_.getLastArgAsInteger(gollvm::options::OPT_tracelevel_EQ, 0u);
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
      driver_.reconcileOptionPair(gollvm::options::OPT_fgo_check_divide_zero,
                                  gollvm::options::OPT_fno_go_check_divide_zero,
                                  true);
  args.check_divide_overflow =
      driver_.reconcileOptionPair(gollvm::options::OPT_fgo_check_divide_overflow,
                                  gollvm::options::OPT_fno_go_check_divide_overflow,
                                  true);
  args.compiling_runtime =
      args_.hasArg(gollvm::options::OPT_fgo_compiling_runtime);
  llvm::Optional<int> del =
      driver_.getLastArgAsInteger(gollvm::options::OPT_fgo_debug_escape_EQ, 0);
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
      driver_.reconcileOptionPair(gollvm::options::OPT_fgo_optimize_allocs,
                                  gollvm::options::OPT_fno_go_optimize_allocs,
                                  true);
  go_enable_optimize("allocs", enableEscapeAnalysis ? 1 : 0);

  // Include dirs
  std::vector<std::string> incargs =
      args_.getAllArgValues(gollvm::options::OPT_I);
  for (auto &dir : incargs) {
    if (sys::fs::is_directory(dir))
      go_add_search_path(dir.c_str());
  }

  // Library dirs
  // TODO: add version, architecture dirs
  std::vector<std::string> libargs =
      args_.getAllArgValues(gollvm::options::OPT_L);
  for (auto &dir : libargs) {
    struct stat st;
    if (stat (dir.c_str(), &st) == 0 && S_ISDIR (st.st_mode))
      go_add_search_path(dir.c_str());
  }

  return true;
}

bool CompileGoImpl::invokeFrontEnd()
{
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
      driver_.getLastArgAsInteger(gollvm::options::OPT_tracelevel_EQ, 0u);
  if (*tl)
    std::cerr << "linemap stats:" << linemap_->statistics() << "\n";

  // Delete the bridge at this point. In the case that there were
  // errors, this will help clean up any unreachable LLVM Instructions
  // (which would otherwise trigger asserts); in the non-error case it
  // will help to free up bridge-related memory prior to kicking off
  // the pass manager.
  bridge_.reset(nullptr);

  // Early exit at this point if we've seen errors
  if (go_be_saw_errors())
    return false;

  return true;
}

void CompileGoImpl::createPasses(legacy::PassManager &MPM,
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

bool CompileGoImpl::invokeBackEnd()
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

  // Add passes to emit bitcode or LLVM IR as appropriate. Here we mimic
  // clang behavior, which is to emit bitcode when "-emit-llvm" is specified
  // but an LLVM IR dump of "-S -emit-llvm" is used.
  raw_pwrite_stream *OS = &asmout_->os();
  if (args_.hasArg(gollvm::options::OPT_emit_llvm)) {
    bool bitcode = !args_.hasArg(gollvm::options::OPT_S);
    bool preserveUseLists =
        driver_.reconcileOptionPair(gollvm::options::OPT_emit_llvm_uselists,
                                    gollvm::options::OPT_no_emit_llvm_uselists,
                                    false);
    modulePasses.add(bitcode ?
                     createBitcodeWriterPass(*OS, preserveUseLists) :
                     createPrintModulePass(*OS, "", preserveUseLists));
  }

  // Set up codegen passes
  legacy::PassManager codeGenPasses;
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

  // Here we go... first function passes
  functionPasses.doInitialization();
  for (Function &F : *module_.get())
    if (!F.isDeclaration())
      functionPasses.run(F);
  functionPasses.doFinalization();

  // ... then module passes
  modulePasses.run(*module_.get());

  // ... and finally code generation
  codeGenPasses.run(*module_.get());

  if (hasError_)
    return false;

  return true;
}

//......................................................................

CompileGo::CompileGo(ToolChain &tc, const std::string &executablePath)
    : InternalTool("gocompiler", tc),
      impl_(new CompileGoImpl(tc, executablePath))
{
}

CompileGo::~CompileGo()
{
}

bool CompileGo::performAction(Compilation &compilation,
                              const Action &jobAction,
                              const ArtifactList &inputArtifacts,
                              const Artifact &output)
{
  return impl_->performAction(compilation, jobAction, inputArtifacts, output);
}

} // end namespace driver
} // end namespace gollvm
