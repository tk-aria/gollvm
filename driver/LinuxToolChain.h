//===-- LinuxToolChain.h --------------------------------------------------===//
//
// Copyright 2018 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
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
  std::string getDynamicLinker(const llvm::opt::ArgList &args) override;

 private:
  gnutools::gccdetect::InspectRealFS inspectFS_;
  gnutools::gccdetect::GCCInstallationDetector gccDetector_;
};

} // end namespace toolchains

#endif // GOLLVM_DRIVER_LINUXTOOLCHAIN_H
