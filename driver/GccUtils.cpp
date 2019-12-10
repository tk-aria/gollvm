//===-- GccUtils.cpp ------------------------------------------------------===//
//
// Copyright 2018 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
//
//===----------------------------------------------------------------------===//
//
// Gcc installation detector and related code.
//
//===----------------------------------------------------------------------===//

#include <sstream>

#include "GccUtils.h"

#include "llvm/Support/FileSystem.h"

namespace gnutools {

namespace gccdetect {

stringvec InspectRealFS::scanDir(const std::string &dir)
{
  stringvec result;
  std::error_code ec;
  llvm::sys::fs::directory_iterator dend, dit(dir, ec);
  if (ec)
    return stringvec();
  while (dit != dend) {
    result.push_back(dit->path());
    dit.increment(ec);
    if (ec)
      return stringvec();
  }
  return result;
}

bool InspectRealFS::is_directory(const std::string &path)
{
  return llvm::sys::fs::is_directory(path);
}

bool InspectRealFS::exists(const std::string &path)
{
  return llvm::sys::fs::exists(path);
}

//........................................................................

GCCVersion::GCCVersion()
    : maj_(-1), min_(-1)
{
}

// Break up a string into (up to) three chunks/tokens based on a specific
// delimitor.

static llvm::SmallVector<std::string, 4> tokenize3(const std::string &s,
                                                  char delim)
{
  llvm::SmallVector<std::string, 4> result;
  size_t cur;
  size_t nxt = -1;
  for (unsigned chunks = 0; chunks < 2; ++ chunks) {
    cur = nxt + 1;
    nxt = s.find_first_of(delim, cur);
    size_t rem = (nxt == std::string::npos ? nxt : nxt - cur);
    result.push_back(s.substr(cur, rem));
    if (nxt == std::string::npos)
      break;
  }
  if (nxt != std::string::npos)
    result.push_back(s.substr(nxt+1, std::string::npos));
  return result;
}

GCCVersion GCCVersion::parse(llvm::StringRef vtext)
{
  const GCCVersion bad;
  GCCVersion result;
  std::string vts(vtext.str());
  auto tokens = tokenize3(vts, '.');

  // Major version.
  if (tokens.empty() ||
      llvm::StringRef(tokens[0]).getAsInteger(10, result.maj_) ||
      result.maj_ < 4)
    return bad;
  result.text_ = vtext;
  if (tokens.size() == 1)
    return result;

  // Minor version
  if (llvm::StringRef(tokens[1]).getAsInteger(10, result.min_) ||
      result.min_ < 0)
    return bad;
  if (tokens.size() == 2)
    return result;

  // Store away remainder (patch or equivalent).
  result.remainder_ = tokens[2];
  return result;
}

bool GCCVersion::operator<(const GCCVersion &rhs) const
{
  if (maj_ != rhs.maj_)
    return maj_ < rhs.maj_;
  if (min_ != rhs.min_)
    return min_ < rhs.min_;
  return remainder_.compare(rhs.remainder_) < 0;
}

std::string GCCVersion::toString() const
{
  std::stringstream ss;
  ss << "maj=" << maj_ << " min=" << min_ << " rem=" << remainder_;
  return ss.str();
}

//........................................................................

GCCInstallationDetector::
GCCInstallationDetector(const llvm::Triple &targetTriple,
                        const std::string &gccToolchainDir,
                        const std::string &sysroot,
                        InspectFS &inspector)
    : triple_(targetTriple),
      gccToolchainDir_(gccToolchainDir),
      sysroot_(sysroot),
      inspector_(inspector),
      version_(new GCCVersion())
{
}

struct GCCInstallationDetector::state {
  // requested triple
  llvm::Triple triple;
  // list of triple aliases to use when searching
  std::vector<std::string> tripleAliases;
  // libdirs to look for within prefix dir
  stringvec libdirs;
  // biarch suffixes (or empty string)
  std::vector<std::string> suffixes;
  // best version so far
  GCCVersion version;
  // path to best verison
  std::string candidate;
  // found suffix
  std::string suffix;
  // found triple
  llvm::Triple candTriple;
};

bool GCCInstallationDetector::selectLibDirs(state &s)
{
  // TODO: add more cases as additional architectures brought on.

  switch (triple_.getArch()) {
    case llvm::Triple::x86:
    case llvm::Triple::x86_64:
      s.tripleAliases = {
        triple_.str(),
        "x86_64-linux-gnu",       "x86_64-unknown-linux-gnu",
        "x86_64-pc-linux-gnu",    "x86_64-redhat-linux6E",
        "x86_64-redhat-linux",    "x86_64-suse-linux"
      };
      s.libdirs.push_back("lib");
      s.libdirs.push_back("lib64");
      if (triple_.getArch() == llvm::Triple::x86) {
        s.triple.setTriple(triple_.get64BitArchVariant().str());
        s.suffixes = {"/32", "/lib32"};
      } else {
        s.triple.setTriple(triple_.str());
        s.suffixes = {""};
      }
      break;
    case llvm::Triple::aarch64:
      // more triples to be identified and added
      s.tripleAliases = {
        triple_.str(),
        "aarch64-linux-gnu", "aarch64-unknown-linux-gnu",
        "aarch64-pc-linux-gnu", "aarch64-redhat-linux",
        "aarch64-suse-linux"
      };
      // multilib is not supported on major aarch64/arm64 linux distributions
      // subject to change when more scenarios to be taken into account
      s.libdirs.push_back("lib");
      s.triple.setTriple(triple_.str());
      s.suffixes = {""};
      break;
    default:
      llvm::errs() << "error: unsupported triple "
                   << triple_.str() << " in " << __FUNCTION__ << "\n";
      assert(false);
      return false;
  }
  return true;
}

// From comments in Clang it appears that there are some Linux systems
// that do not use crtbegin.o; if need be this could be switched to libgcc.a.

bool GCCInstallationDetector::validCandidate(const std::string &cand,
                                             const std::string &suffix)
{
  // is there an actual installation?
  std::stringstream ss;
  ss << cand << suffix << "/crtbegin.o";
  std::string crtb = ss.str();
  if (!inspector_.exists(crtb))
    return false;
  return true;
}

void GCCInstallationDetector::scanCandidate(state &s,
                                            const std::string &cand,
                                            const std::string &alias,
                                            const std::string &suffix)
{
  stringvec dirents = inspector_.scanDir(cand);
  for (auto &ent : dirents) {
    std::size_t pos = ent.find_last_of('/');
    assert(pos != std::string::npos);
    std::string base(ent.substr(pos+1));
    // gcc version valid?
    GCCVersion v = GCCVersion::parse(base);
    if (!v.valid())
      continue;
    if (!validCandidate(ent, suffix))
      continue;
    // more recent than previously found version?
    if (s.version < v) {
      s.version = v;
      s.candidate = ent;
      s.suffix = suffix;
      s.candTriple.setTriple(alias);
    }
  }
}

void GCCInstallationDetector::scanPrefix(state &s, const std::string &pref)
{
  const char *infixes[] = { "/gcc/", "/gcc-cross/" };
  for (auto &lib : s.libdirs) {
    // Form candidate dir
    for (auto &infix : infixes) {
      for (auto &tripleAlias : s.tripleAliases) {
        std::stringstream ss;
        ss << pref << "/" << lib << infix << tripleAlias;
        for (auto &suffix : s.suffixes)
          scanCandidate(s, ss.str(), tripleAlias, suffix);
      }
    }
  }
}

void GCCInstallationDetector::init()
{
  state s;

  // Setup
  if (!selectLibDirs(s))
    return;

  if (!gccToolchainDir_.empty())
    scanPrefix(s, gccToolchainDir_);

  if (!s.version.valid() && !sysroot_.empty()) {
    // If sysroot is non-empty, then perform a search there first.
    std::string srp(sysroot_);
    srp += "/usr";
    scanPrefix(s, srp);
  }

  // If we found something viable in the sysroot, then don't look any
  // farther (this differs from clang AFAICT). The intent here is that
  // if the sysroot provides GCC X but the system has GCC X+1, we
  // still want to use the version in the sysroot.
  if (!s.version.valid()) {
    // Examine the host system.
    scanPrefix(s, "/usr");
  }

  // Incorporate results.
  if (s.version.valid()) {
    *version_ = s.version;
    foundTriple_.setTriple(s.candTriple.str());
    installPath_ = s.candidate;
    libPath_ = s.candidate;
    libPath_ += s.suffix;
    parentLibPath_ = installPath_ + "/../.." +
        (s.suffix.empty() ? "" : "/..");
  }
}

std::string GCCInstallationDetector::toString()
{
  std::stringstream ss;
  ss << "version: " << version_->text() << "\n";
  ss << "foundTriple: " << foundTriple_.str() << "\n";
  ss << "libPath: " << libPath_ << "\n";
  ss << "parentLibPath: " << parentLibPath_ << "\n";
  ss << "installPath: " << installPath_ << "\n";
  return ss.str();
}

} // end namespace gccdetect

} // end namespace gnutools
