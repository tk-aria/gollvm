//===-- LinuxToolChain.cpp ------------------------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// Gollvm driver helper class LinuxToolChain methods.
//
//===----------------------------------------------------------------------===//

#include "LinuxToolChain.h"

#include "llvm/Support/FileSystem.h"
#include "llvm/Support/raw_ostream.h"

#include "Driver.h"
#include "GnuTools.h"
#include "Tool.h"
#include "ToolChain.h"

using namespace gollvm::driver;

namespace toolchains {

static void addIfPathExists(pathlist &paths, const llvm::Twine &path)
{
  if (llvm::sys::fs::exists(path))
    paths.push_back(path.str());
}

Linux::Linux(gollvm::driver::Driver &driver,
             const llvm::Triple &targetTriple)
    : ToolChain(driver, targetTriple),
      inspectFS_(gnutools::gccdetect::InspectRealFS()),
      gccDetector_(targetTriple,
                   driver.sysRoot(),
                   inspectFS_)
{
  gccDetector_.init();

  // Program paths
  pathlist &ppaths = programPaths();
  addIfPathExists(ppaths, llvm::Twine(gccDetector_.getParentLibPath() +
                                      "/../" + targetTriple.str() +
                                      "/bin").str());

  // File paths
  pathlist &fpaths = filePaths();
  addIfPathExists(fpaths, gccDetector_.getLibPath());

  // additional path setup to appear in a forthcoming patch
}

Linux::~Linux()
{
}

Tool *Linux::buildCompiler()
{
  // implementation to appear in a subsequent patch
  return nullptr;
}

Tool *Linux::buildAssembler()
{
  return new gnutools::Assembler(*this);
}

Tool *Linux::buildLinker()
{
  return new gnutools::Linker(*this);
}

} // end namespace toolchains
