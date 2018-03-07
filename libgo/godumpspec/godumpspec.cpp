//===-- godumpspec.cpp - C->Go helper utility for llvm --------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This program is a helper for the libgo build. Given an object file
// and macros file derived from a given C source file, emit Go translations
// for the types/constants/macros in the C file.
//
// Expected usage mode looks something like this:
//
//   % cc -E -dM -c -o somefile-macros.txt somefile.c
//   % cc -g -c -o somefile.o somefile.c
//   % llvm-godumpspec -object somefile.o \
//         -macrotmp somefile-macros.txt \
//         -output somefile-types-and-macros.go
//
// The tool reads DWARF from 'somefile.o' and combines the type/var/constant
// info from the DWARF with macro definitions from 'somefile-macros.txt'
// to produce Go equivalents for the type/var/constant info in the original
// C source file.
//
//===----------------------------------------------------------------------===//

#include "llvm/DebugInfo/DIContext.h"
#include "llvm/DebugInfo/DWARF/DWARFContext.h"
#include "llvm/Object/Binary.h"
#include "llvm/Object/ObjectFile.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/Format.h"
#include "llvm/Support/ManagedStatic.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/PrettyStackTrace.h"
#include "llvm/Support/Signals.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/ToolOutputFile.h"
#include "llvm/Support/raw_ostream.h"

#include <unordered_set>
#include <unordered_map>
#include <iostream>
#include <fstream>
#include <sstream>
#include <string>

using namespace llvm;
using namespace object;

namespace {
using namespace cl;

static cl::opt<std::string>
InputObjectFile("object", cl::desc("Object file for *.c file"));

static cl::opt<std::string>
InputMacrosFile("macrotmp", cl::desc("Macros file for *.c file"));

static cl::opt<std::string>
OutputFilename("output", cl::desc("Output file to write."));

static cl::opt<unsigned>
PointerSize("pointersize", cl::desc("Size of a pointer in bytes for "
                                    "the target architecture of interest. "
                                    "Defaults to host pointer size."),
            cl::init(sizeof(void*)));

static cl::opt<bool>
Trace("trace", cl::desc("Enable debug trace output."));

} // namespace

static void error(StringRef Prefix, std::error_code EC) {
  if (!EC)
    return;
  errs() << Prefix << ": " << EC.message() << "\n";
  exit(1);
}

struct ObjectState {
  std::unique_ptr<MemoryBuffer> mbuf_;
  std::unique_ptr<Binary> binary_;
  std::unique_ptr<DWARFContext> dwctxt_;
};

static int visitObjectFile(const std::string &infile,
                           ObjectState &ostate,
                           raw_ostream &os)
{
  ErrorOr<std::unique_ptr<MemoryBuffer>> buffOrErr =
      MemoryBuffer::getFile(infile);
  error(infile, buffOrErr.getError());
  std::unique_ptr<MemoryBuffer> buffer = std::move(buffOrErr.get());
  ostate.mbuf_.reset(buffer.release());

  Expected<std::unique_ptr<Binary>> binOrErr =
      object::createBinary(*ostate.mbuf_);
  error(infile, errorToErrorCode(binOrErr.takeError()));
  std::unique_ptr<Binary> binary = std::move(binOrErr.get());
  ostate.binary_.reset(binary.release());

  // NB: no MachO support at the moment
  auto *obj = dyn_cast<ObjectFile>(ostate.binary_.get());
  if (obj == nullptr) {
    errs() << "error: problems opening object file " << infile << "\n";
    return 1;
  }
  ostate.dwctxt_.reset(DWARFContext::create(*obj).release());

  // Expect to see exactly one DWARF CU.
  if (ostate.dwctxt_->getNumCompileUnits() < 1) {
    errs() << "error: no DWARF compilation units found in " << infile << "\n";
    return 1;
  } else if (ostate.dwctxt_->getNumCompileUnits() > 1) {
    errs() << "error: unexpected multiple DWARF compilation "
           << "units found in " << infile << "\n";
    return 1;
  }

  DWARFCompileUnit *cu = ostate.dwctxt_->getCompileUnitAtIndex(0);
  assert(cu);

  // Implementation stubbed out (to appear in following patch).


  return 0;
}

static int visitMacrosFile(const std::string &infile,
                           raw_ostream &os)
{
  // Implementation stubbed out (to appear in following patch).
  return 0;
}

int main(int argc, char **argv) {

  // Print a stack trace if we signal out.
  sys::PrintStackTraceOnErrorSignal(argv[0]);
  PrettyStackTraceProgram X(argc, argv);
  llvm_shutdown_obj Y;  // Call llvm_shutdown() on exit.

  llvm::InitializeAllTargetInfos();
  llvm::InitializeAllTargetMCs();

  cl::ParseCommandLineOptions(
      argc, argv,
      "Emit Go translation for type/const/macro information derived "
      "from compilation of a C file.\n");

  if (InputObjectFile.empty()) {
    errs() << "error: supply input object file using -object option.\n";
    return 1;
  }

  std::unique_ptr<ToolOutputFile> OutputFile;
  if (!OutputFilename.empty()) {
    std::error_code EC;
    OutputFile = llvm::make_unique<ToolOutputFile>(OutputFilename, EC,
                                                     sys::fs::F_None);
    error("Unable to open output file" + OutputFilename, EC);
    // Don't remove output file if we exit with an error.
    OutputFile->keep();
  }

  raw_ostream &OS = OutputFile ? OutputFile->os() : outs();
  ObjectState ostate;

  int rc = 0;
  if (! InputObjectFile.empty()) {
    rc |= visitObjectFile(InputObjectFile, ostate, OS);
  }
  if (! InputMacrosFile.empty()) {
    rc |= visitMacrosFile(InputMacrosFile, OS);
  }

  return rc;
}
