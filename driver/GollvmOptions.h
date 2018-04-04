//===--- GollvmOptions.h - Gollvm Option info & table -----------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_GOLLVM_DRIVER_GOLLVMOPTIONS_H
#define LLVM_GOLLVM_DRIVER_GOLLVMOPTIONS_H

#include <memory>

namespace llvm {
namespace opt {
class OptTable;
}
}

namespace gollvm {
namespace options {

/// Flags specifically for gollvm options.  Must not overlap with
/// llvm::opt::DriverFlag.
enum flags {
  DriverOption = (1 << 4),
  LinkerInput = (1 << 5),
  Unsupported = (1 << 6),
  Ignored = (1 << 7)
};

enum ID {
    OPT_INVALID = 0, // This is not an option ID.
#define OPTION(PREFIX, NAME, ID, KIND, GROUP, ALIAS, ALIASARGS, FLAGS, PARAM,  \
               HELPTEXT, METAVAR, VALUES)                                      \
  OPT_##ID,
#include "GollvmOptions.inc"
    LastOption
#undef OPTION
};

extern std::unique_ptr<llvm::opt::OptTable> createGollvmDriverOptTable();

} // end namespace options
} // end namespace gollvm

#endif
