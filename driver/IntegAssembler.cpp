//===-- IntegAssembler.cpp ------------------------------------------------===//
//
// Copyright 2020 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
//
//===----------------------------------------------------------------------===//
//
// Gollvm driver helper class "IntegAssembler" methods.
//
//===----------------------------------------------------------------------===//

#include "IntegAssembler.h"

#include "go-llvm-linemap.h"
#include "go-llvm-diagnostics.h"
#include "go-llvm.h"
#include "go-c.h"
#include "mpfr.h"
#include "GollvmOptions.h"
#include "GollvmConfig.h"
#include "GollvmPasses.h"

#include "Action.h"
#include "Artifact.h"
#include "Driver.h"
#include "ToolChain.h"

//#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/Triple.h"
//#include "llvm/Analysis/TargetLibraryInfo.h"
//#include "llvm/Bitcode/BitcodeWriterPass.h"
#include "llvm/Config/llvm-config.h"
//#include "llvm/CodeGen/MachineModuleInfo.h"
//#include "llvm/CodeGen/TargetPassConfig.h"
//#include "llvm/IR/DiagnosticInfo.h"
//#include "llvm/IR/DiagnosticPrinter.h"
//#include "llvm/IR/IRPrintingPasses.h"
#include "llvm/IR/LLVMContext.h"
//#include "llvm/IR/LegacyPassManager.h"
//#include "llvm/IR/LLVMRemarkStreamer.h"
//#include "llvm/Remarks/YAMLRemarkSerializer.h"
//#include "llvm/IR/Verifier.h"
//#include "llvm/MC/SubtargetFeature.h"
#include "llvm/MC/MCAsmBackend.h"
#include "llvm/MC/MCAsmInfo.h"
#include "llvm/MC/MCCodeEmitter.h"
#include "llvm/MC/MCContext.h"
#include "llvm/MC/MCInstrInfo.h"
#include "llvm/MC/MCObjectFileInfo.h"
#include "llvm/MC/MCObjectWriter.h"
#include "llvm/MC/MCParser/MCAsmParser.h"
#include "llvm/MC/MCParser/MCTargetAsmParser.h"
#include "llvm/MC/MCRegisterInfo.h"
#include "llvm/MC/MCSectionMachO.h"
#include "llvm/MC/MCStreamer.h"
#include "llvm/MC/MCSubtargetInfo.h"
#include "llvm/MC/MCTargetOptions.h"
#include "llvm/Option/Arg.h"
#include "llvm/Option/ArgList.h"
#include "llvm/Option/OptTable.h"
#include "llvm/Option/Option.h"
//#include "llvm/Passes/PassBuilder.h"
//#include "llvm/CodeGen/Passes.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"
//#include "llvm/Support/Format.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Host.h"
//#include "llvm/Support/ManagedStatic.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/Path.h"
//#include "llvm/Support/PrettyStackTrace.h"
#include "llvm/Support/Process.h"
#include "llvm/Support/Program.h"
#include "llvm/Support/Regex.h"
#include "llvm/Support/Signals.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Support/TargetSelect.h"
//#include "llvm/Support/ToolOutputFile.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Target/TargetMachine.h"
//#include "llvm/Transforms/IPO.h"
//#include "llvm/Transforms/IPO/PassManagerBuilder.h"
//#include "llvm/Transforms/Utils.h"

#include <sstream>

using namespace llvm;

namespace gollvm {
namespace driver {

class IntegAssemblerImpl {
 public:
  IntegAssemblerImpl(IntegAssembler &ia, ToolChain &tc, const std::string &executablePath);

  // Perform compilation.
  bool performAction(Compilation &compilation,
                     const Action &jobAction,
                     const ArtifactList &inputArtifacts,
                     const Artifact &output);

 private:
  IntegAssembler &ia_;
  Triple triple_;
  const ToolChain &toolchain_;
  Driver &driver_;
  LLVMContext context_;
  const char *progname_;
  std::string executablePath_;
  opt::InputArgList &args_;
  std::string inputFileName_;
  std::string objOutFileName_;
  std::unique_ptr<raw_fd_ostream> objout_;

  bool resolveInputOutput(const Action &jobAction,
                          const ArtifactList &inputArtifacts,
                          const Artifact &output);
  bool invokeAssembler();
};

IntegAssemblerImpl::IntegAssemblerImpl(IntegAssembler &ia, ToolChain &tc, const std::string &executablePath)
    : ia_(ia),
      triple_(tc.driver().triple()),
      toolchain_(tc),
      driver_(tc.driver()),
      progname_(tc.driver().progname()),
      executablePath_(executablePath),
      args_(tc.driver().args())
{
  InitializeAllTargets();
  InitializeAllTargetMCs();
  InitializeAllAsmPrinters();
  InitializeAllAsmParsers();
}

bool IntegAssemblerImpl::resolveInputOutput(const Action &jobAction,
                                            const ArtifactList &inputArtifacts,
                                            const Artifact &output)
{
  // Collect input file. Should be only one.
  if (inputArtifacts.size() != 1) {
    errs() << progname_ << ": expected exactly one input file, "
           << inputArtifacts.size() << " provided.\n";
    return false;
  }
  inputFileName_ = inputArtifacts[0]->file();
  objOutFileName_ = output.file();

  // Remove output on signal.
  if (objOutFileName_ != "-")
    sys::RemoveFileOnSignal(objOutFileName_);

  // Open output file.
  std::error_code EC;
  sys::fs::OpenFlags OpenFlags = sys::fs::OF_None;
  objout_ = std::make_unique<raw_fd_ostream>(
      objOutFileName_, EC, OpenFlags);
  if (EC) {
    errs() << progname_ << ": error opening " << objOutFileName_ << ": "
           << EC.message() << '\n';
    return false;
  }
  return true;
}

bool IntegAssemblerImpl::invokeAssembler()
{
  // Get the target specific parser.
  std::string Error;
  const Target *TheTarget = TargetRegistry::lookupTarget("", triple_, Error);
  if (!TheTarget) {
    errs() << progname_ << ": unknown/unsupported target "
           << triple_.str() << "\n";
    return false;
  }

  ErrorOr<std::unique_ptr<MemoryBuffer>> Buffer =
      MemoryBuffer::getFileOrSTDIN(inputFileName_);
  if (std::error_code EC = Buffer.getError()) {
    Error = EC.message();
    errs() << progname_ << ": opening/reading " << inputFileName_ << ": "
           << EC.message() << "\n";
    return false;
  }

  auto Trip = triple_.str();
  SourceMgr SrcMgr;

  // Tell SrcMgr about this buffer, which is what the parser will pick up.
  SrcMgr.AddNewSourceBuffer(std::move(*Buffer), SMLoc());

  // Record the location of the include directories so that the lexer can find
  // it later.
  SrcMgr.setIncludeDirs(driver_.args().getAllArgValues(gollvm::options::OPT_I));

  std::unique_ptr<MCRegisterInfo> MRI(TheTarget->createMCRegInfo(Trip));
  assert(MRI && "Unable to create target register info!");

  MCTargetOptions MCOptions;
  std::unique_ptr<MCAsmInfo> MAI(
      TheTarget->createMCAsmInfo(*MRI, Trip, MCOptions));
  assert(MAI && "Unable to create target asm info!");

  // FIXME: at this point what we need to do is collect up any assembler
  // arguments specified with -Wa,XXX and turn them into the correct
  // back end setup options. For now, just assert if we see -Wa.
  auto waComArg = args_.getLastArg(gollvm::options::OPT_Wa_COMMA);
  auto xAsmArg = args_.getLastArg(gollvm::options::OPT_Xassembler);
  if (waComArg != nullptr || xAsmArg != nullptr) {
    errs() << progname_ << ": internal error: option '"
           <<  (waComArg != nullptr ? waComArg->getAsString(args_) :
                xAsmArg->getAsString(args_))
           << "' not yet implemented in integrated assembler\n";
    assert(false);
    return false;
  }

  // FIXME: no support yet for -march (bring over from CompileGo.cpp)
  opt::Arg *cpuarg = args_.getLastArg(gollvm::options::OPT_march_EQ);
  if (cpuarg != nullptr) {
    errs() << progname_ << ": internal error: option '"
           <<  cpuarg->getAsString(args_)
           << "' not yet implemented in integrated assembler\n";
    assert(false);
    return false;
  }

  // Support for compressed debug.
  llvm::DebugCompressionType CompressDebugSections =
      llvm::DebugCompressionType::None;
  llvm::opt::Arg *gzarg = args_.getLastArg(gollvm::options::OPT_gz,
                                           gollvm::options::OPT_gz_EQ);
  if (gzarg != nullptr) {
    if (gzarg->getOption().matches(gollvm::options::OPT_gz)) {
      CompressDebugSections = llvm::DebugCompressionType::GNU;
    } else {
      CompressDebugSections =
          llvm::StringSwitch<llvm::DebugCompressionType>(gzarg->getValue())
          .Case("none", llvm::DebugCompressionType::None)
          .Case("zlib", llvm::DebugCompressionType::Z)
          .Case("zlib-gnu", llvm::DebugCompressionType::GNU)
          .Default(llvm::DebugCompressionType::None);
    }
  }

  // Ensure MCAsmInfo initialization occurs before any use, otherwise sections
  // may be created with a combination of default and explicit settings.
  MAI->setCompressDebugSections(CompressDebugSections);

  // FIXME: This is not pretty. MCContext has a ptr to MCObjectFileInfo and
  // MCObjectFileInfo needs a MCContext reference in order to initialize itself.
  std::unique_ptr<MCObjectFileInfo> MOFI(new MCObjectFileInfo());

  MCContext Ctx(MAI.get(), MRI.get(), MOFI.get(), &SrcMgr, &MCOptions);

  bool PIC = (driver_.getPicLevel() != PICLevel::NotPIC);
  MOFI->InitMCObjectFileInfo(triple_, PIC, Ctx);
  Ctx.setGenDwarfForAssembly(true);
  //Ctx.setDwarfVersion(driver_.dwarfVersion());

  // Use current dir (llvm-goc does not yet support -fdebug-compilation-dir)
  SmallString<128> CWD;
  if (!sys::fs::current_path(CWD))
    Ctx.setCompilationDir(CWD);

  // Honor -fdebug-prefix=... option.
  for (const auto &arg : driver_.args().getAllArgValues(gollvm::options::OPT_fdebug_prefix_map_EQ)) {
    std::pair<StringRef, StringRef> p = StringRef(arg).split('=');
    Ctx.addDebugPrefixMapEntry(std::string(p.first), std::string(p.second));
  }


  StringRef BaseName = llvm::sys::path::filename(inputFileName_);
  Ctx.setMainFileName(BaseName);
  // FIXME: incorporate version here?
  Ctx.setDwarfDebugProducer("llvm-goc");

  // Build up the feature string from the target feature list.
  std::string FS;
  std::string CPU;
  std::unique_ptr<MCStreamer> Str;
  std::unique_ptr<MCInstrInfo> MCII(TheTarget->createMCInstrInfo());
  std::unique_ptr<MCSubtargetInfo> STI(
      TheTarget->createMCSubtargetInfo(Trip, CPU, FS));

  raw_pwrite_stream *Out = objout_.get();
  std::unique_ptr<buffer_ostream> BOS;

  if (!objout_->supportsSeeking()) {
    BOS = std::make_unique<buffer_ostream>(*objout_);
    Out = BOS.get();
  }

  std::unique_ptr<MCCodeEmitter> CE(
      TheTarget->createMCCodeEmitter(*MCII, *MRI, Ctx));
  std::unique_ptr<MCAsmBackend> MAB(
      TheTarget->createMCAsmBackend(*STI, *MRI, MCOptions));
  std::unique_ptr<MCObjectWriter> OW = MAB->createObjectWriter(*Out);

  Triple T(driver_.triple());
  unsigned RelaxAll = 0;
  unsigned IncrementalLinkerCompatible = 0;
  Str.reset(TheTarget->createMCObjectStreamer(
      T, Ctx, std::move(MAB), std::move(OW), std::move(CE), *STI,
      RelaxAll, IncrementalLinkerCompatible,
        /*DWARFMustBeAtTheEnd*/ true));

  bool NoExecStack = false; // FIXME?
  Str.get()->InitSections(NoExecStack);

  // Assembly to object compilation should leverage assembly info.
  Str->setUseAssemblerInfoForParsing(true);

  std::unique_ptr<MCAsmParser> Parser(
      createMCAsmParser(SrcMgr, Ctx, *Str.get(), *MAI));

  std::unique_ptr<MCTargetAsmParser> TAP(
      TheTarget->createMCAsmParser(*STI, *Parser, *MCII, MCOptions));
  if (!TAP) {
    errs() << progname_ << ": error: unknown triple"
           << triple_.str() << "\n";
    return false;
  }

  // FIXME: add support for -Wa,-defsym here?

  Parser->setTargetParser(*TAP.get());
  bool NoInitialTextSection = false;
  auto Failed = Parser->Run(NoInitialTextSection);

  // Close Streamer first.
  // It might have a reference to the output stream.
  Str.reset();
  // Close the output stream early.
  BOS.reset();
  objout_.reset();

  // Delete output file if there were errors.
  if (Failed) {
    if (objOutFileName_ != "-")
      sys::fs::remove(objOutFileName_);
  }

  return !Failed;
}

bool IntegAssemblerImpl::performAction(Compilation &compilation,
                                  const Action &jobAction,
                                  const ArtifactList &inputArtifacts,
                                  const Artifact &output)
{
  if (ia_.emitMinusVOrHashHashHash(triple_, output, jobAction))
    return true;

  // Resolve input/output files.
  if (!resolveInputOutput(jobAction, inputArtifacts, output))
    return false;

  // Invoke back end
  if (!invokeAssembler())
    return false;

  return true;
}



//........................................................................

IntegAssembler::IntegAssembler(ToolChain &tc, const std::string &executablePath)
    : InternalTool("integassembler", tc, executablePath),
      impl_(new IntegAssemblerImpl(*this, tc, executablePath))
{
}

IntegAssembler::~IntegAssembler()
{
}

bool IntegAssembler::performAction(Compilation &compilation,
                              const Action &jobAction,
                              const ArtifactList &inputArtifacts,
                              const Artifact &output)
{
  return impl_->performAction(compilation, jobAction, inputArtifacts, output);
}


} // end namespace driver
} // end namespace gollvm
