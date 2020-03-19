//===-- Distro.h --------------------------------------------------===//
//
// Copyright 2020 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
//
//===----------------------------------------------------------------------===//
//
// Defines the DetectDistro helper function and return codes.
//
//===----------------------------------------------------------------------===//

#ifndef GOLLVM_DRIVER_DISTRO_H
#define GOLLVM_DRIVER_DISTRO_H

#include "GccUtils.h"
#include "llvm/ADT/Triple.h"

namespace distro {

enum DistroVariety {
  DistroAlpine,
  DistroArchLinux,
  DistroDebian,
  DistroRedhat,
  DistroGentoo,
  DistroOpenSUSE,
  DistroUbuntu,
  DistroUnknown
};

extern DistroVariety DetectDistro(gnutools::gccdetect::InspectFS &ifs,
                                  const llvm::Triple &target);

} // end namespace distro

#endif // GOLLVM_DRIVER_DISTRO_H

