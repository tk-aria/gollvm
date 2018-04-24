//===-- LinuxToolChain.h --------------------------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// Defines the LinuxToolChain class (helper for driver functionality).
//
//===----------------------------------------------------------------------===//

#ifndef GOLLVM_DRIVER_LINUXTOOLCHAIN_H
#define GOLLVM_DRIVER_LINUXTOOLCHAIN_H

#include "ToolChain.h"
#include "GccUtils.h"

namespace toolchains {

// Toolchain for linux.

class Linux : public gollvm::driver::ToolChain {
 public:
  Linux(gollvm::driver::Driver &driver,
        const llvm::Triple &targetTriple);
  ~Linux();

  gollvm::driver::Tool *buildCompiler() override;
  gollvm::driver::Tool *buildAssembler() override;
  gollvm::driver::Tool *buildLinker() override;

 private:
  gnutools::gccdetect::InspectRealFS inspectFS_;
  gnutools::gccdetect::GCCInstallationDetector gccDetector_;
};

} // end namespace toolchains

#endif // GOLLVM_DRIVER_LINUXTOOLCHAIN_H
