//===-- GccUtils.h --------------------------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// Defines the GCCInstallationDetector class (helper for driver functionality).
//
//===----------------------------------------------------------------------===//

#ifndef GOLLVM_DRIVER_GCCUTILS_H
#define GOLLVM_DRIVER_GCCUTILS_H

#include "ToolChain.h"
#include "llvm/Option/ArgList.h"

namespace gnutools {

namespace gccdetect {

typedef llvm::SmallVector<std::string, 4> stringvec;
class GCCVersion;
class InspectFS;

// Gollvm relies on an installed copy of GCC to provide objects and
// libraries needed for linking. This class implements a GCC
// "installation detector" (modeled after the similarly named clang
// class), which looks for GCC bits in the expected places. This
// version is much simpler than the clang version.
//
// Expected usage model: construct a GCCInstallationDetector with
// a given set of args (target triple, etc), then invoke the init()
// routine. Once initialized it doesn't change much (expectation
// is that it will just be queried via various getters).
//
// Notes:
//
// 1) Clang's gcc detector contains a lot of very complex and thinly
//    documented code for detecting and vetting "multilib" gcc
//    installations (not supported here, although this code looks
//    for biarch variants, such as x86 32-bit variant on x86_64).
//
// 2) Clang implements a filesystem cache to speed things up in
//    the case where there are repeated "stat" calls made on files in
//    a given directory (also not supported here, although in theory
//    it could be added).
//
// 3) There is (apparently) a great deal of variation regarding
//    how/where GCC is installed across various Linux distributions--
//    in Clang the detector includes a linux distro detector and good
//    deal of complicate "fuzzy matching" logic, which is not
//    replicated here (could be added later).

class GCCInstallationDetector {
 public:
  GCCInstallationDetector(const llvm::Triple &targetTriple,
                          const std::string &gccToolchainDir,
                          const std::string &sysroot,
                          InspectFS &inspector);

  void init();

  // Return the version of the "best" or "most suitable" installation
  // of GCC on the host (or in the sysroot, depending). If no GCC
  // installation is found, version.valid() will return false.
  GCCVersion &version() { return *version_; }

  // Return the found triple (taking into account triple aliases).
  llvm::Triple foundTriple() { return foundTriple_; }

  // Returns the runtime install path of the detected GCC installation, e.g.
  // /usr/lib/gcc/x86_64-linux-gnu/7.
  llvm::StringRef getInstallPath() { return installPath_; }

  // Return the library path for the specified target (e.g.
  // /usr/lib/gcc/x86_64-linux-gnu/7/32).
  llvm::StringRef getLibPath() { return libPath_; }

  // Return the parent of the target-specific runtime path (can
  // be used to form the path of corresponding "bin" dir for
  // tool selection).
  llvm::StringRef getParentLibPath() { return parentLibPath_; }

  // for unit testing
  std::string toString();

 private:
  llvm::Triple triple_; // target for which we looked
  llvm::Triple foundTriple_; // target we found (taking into account aliases)
  std::string gccToolchainDir_;
  std::string sysroot_;
  InspectFS &inspector_;
  std::unique_ptr<GCCVersion> version_;
  std::string libPath_;
  std::string installPath_;
  std::string parentLibPath_;

  // Opaque search state object.
  struct state;

  bool selectLibDirs(state &s);
  void scanPrefix(state &s, const std::string &pref);
  void scanCandidate(state &s, const std::string &cand,
                     const std::string &alias,
                     const std::string &suffix);
  bool validCandidate(const std::string &cand,
                      const std::string &suffix);
};

// GCC version helper class. Given something that looks like a GCC
// version number (e.g. 4.9, 7, 8.3.mypatch), parses the string into
// major and minor numbers plus any "remainder". A gcc version is
// considered valid if it has a parseable major version number of 4 or
// later.

class GCCVersion {
 public:
  GCCVersion();
  static GCCVersion parse(llvm::StringRef vtext);
  std::string toString() const;
  std::string text() { return text_; }
  bool operator<(const GCCVersion &rhs) const;
  bool valid() { return maj_ > 0; }

 private:
  std::string text_; // unparsed
  int maj_, min_;
  std::string remainder_;
};

// Abstract helper class for file system inspection. This class
// provides the "scanDir", "is_directory", and "exists" abstract
// methods, which are expects to behave the corresponding
// llvm::sys::fs:: functions behave. The purpose for having this class
// is so that we can create a mock FS inspector for unit tests.

class InspectFS {
 public:
  InspectFS() { }
  virtual ~InspectFS() { }
  virtual stringvec scanDir(const std::string &dir) = 0;
  virtual bool exists(const std::string &path) = 0;
  virtual bool is_directory(const std::string &path) = 0;
};

// A concrete file system inspector. No caching at the moment, could
// be added later if need be.

class InspectRealFS : public InspectFS {
 public:
  InspectRealFS() { }
   ~InspectRealFS() { }
  stringvec scanDir(const std::string &dir) override;
  bool is_directory(const std::string &path) override;
  bool exists(const std::string &path) override;
};

} // end namespace gccdetect

} // end namespace gnutools

#endif // GOLLVM_DRIVER_GCCUTILS_H
