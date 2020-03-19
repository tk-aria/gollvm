//===-- Distro.cpp --------------------------------------------------------===//
//
// Copyright 2020 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
//
//===----------------------------------------------------------------------===//
//
// Defines the DetectDistro utility function (helper for driver).
//
//===----------------------------------------------------------------------===//

#include "Distro.h"

#include "llvm/Support/Host.h"

namespace distro {

// DetectDistro tries to detect the linux distro on which we're running.
//
// NB: we're sweeping a lot of cross-compilation issues under the rug here; the
// working assumption is that users will have to rely on a sysroot in such
// cases.

DistroVariety DetectDistro(gnutools::gccdetect::InspectFS &ifs,
                           const llvm::Triple &target) {
  // Not linux? no distro.
  if (!target.isOSLinux())
    return DistroUnknown;
  llvm::Triple HostTriple(llvm::sys::getProcessTriple());
  if (!HostTriple.isOSLinux())
    return DistroUnknown;

  if (ifs.exists("/etc/debian_version"))
    return DistroDebian;

  if (ifs.exists("/etc/lsb-release"))
    return DistroUbuntu;

  if (ifs.exists("/etc/redhat-release"))
    return DistroRedhat;

  if (ifs.exists("/etc/SuSE-release"))
    return DistroOpenSUSE;

  if (ifs.exists("/etc/alpine-release"))
    return DistroAlpine;

  if (ifs.exists("/etc/arch-release"))
    return DistroArchLinux;

  if (ifs.exists("/etc/gentoo-release"))
    return DistroGentoo;

  return DistroUnknown;
}

}
