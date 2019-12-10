//===---- DriverUtilsTests.cpp --------------------------------------------===//
//
// Copyright 2018 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
//
//===----------------------------------------------------------------------===//

#include <iostream>
#include <map>
#include <set>

#include "Driver.h"
#include "GccUtils.h"

#include "gtest/gtest.h"

#include "DiffUtils.h"

using namespace goBackendUnitTests;
using namespace gnutools::gccdetect;

namespace {

TEST(DriverUtilsTests, ParseGccVersion) {

  struct X {
    const char *verstr;
    const char *expected;
    GCCVersion gv;
  } cands[] = {
    { "foo",     "maj=-1 min=-1 rem=" },
    { "-1",      "maj=-1 min=-1 rem=" },
    { "0.2",     "maj=-1 min=-1 rem=" },
    { "4",       "maj=4 min=-1 rem=" },
    { "4.9.3",   "maj=4 min=9 rem=3" },
    { "4.9.x",   "maj=4 min=9 rem=x" },
    { "7.3.9.q", "maj=7 min=3 rem=9.q" }
  };

  unsigned k = 0;
  for (auto &tc : cands) {
    tc.gv = GCCVersion::parse(tc.verstr);
    EXPECT_EQ(tc.expected, tc.gv.toString());
    std::cerr << k++ << " " << tc.gv.toString() << "\n";
  }

  EXPECT_FALSE(cands[0].gv < cands[0].gv);
  EXPECT_FALSE(cands[0].gv < cands[1].gv);
  for (unsigned k = 2; k < 5; ++k) {
    EXPECT_TRUE(cands[k].gv < cands[k+1].gv);
    if (!(cands[k].gv < cands[k+1].gv)) {
      std::cerr << "compare failed: " << k << " < " << k + 1 << "\n";
    }
  }
}

// Fake filesystem inspector, for unit testing GCC installation detector.
// The underlying data structure for the fake FS is a map keyed by
// string (directory path, e.g. "/dir1/dir2") whose value is a pair
// <F,D> where F is a set of files contained in the dir and D is a set
// of directories contained in the dir.

class InspectFakeFS : public InspectFS {
 public:
  explicit InspectFakeFS(const std::vector<std::string> &entries);
  explicit InspectFakeFS(const std::string &text);
  ~InspectFakeFS() { }
  stringvec scanDir(const std::string &dir);
  bool exists(const std::string &path);
  bool is_directory(const std::string &path);
  std::string toString();
 private:
  void addParent(const std::string &dir);
  void populate(const std::vector<std::string> &entries);
  typedef struct {
    std::set<std::string> files;
    std::set<std::string> dirs;
  } filesdirs;
  std::map<std::string, filesdirs> dirmap_;
};

InspectFakeFS::InspectFakeFS(const std::string &text)
{
  populate(tokenize(text));
}

InspectFakeFS::InspectFakeFS(const std::vector<std::string> &entries)
{
  populate(entries);
}

void InspectFakeFS::addParent(const std::string &dir)
{
  // chop dir into pdir, base
  std::size_t pos = dir.find_last_of('/');
  assert(pos != std::string::npos);
  std::string pdir(dir.substr(0,pos));
  bool goup = true;
  if (pdir.empty()) {
    goup = false;
    pdir = "/";
  }

  // record that dir has child directory subdir
  auto &de = dirmap_[pdir];
  std::string base(dir.substr(pos+1));
  if (! base.empty())
    de.dirs.insert(base);

  // hack: remove from files if needed
  auto it = de.files.find(base);
  if (it != de.files.end())
    de.files.erase(it);

  // walk back up the tree
  if (goup)
    addParent(pdir);
}

void InspectFakeFS::populate(const std::vector<std::string> &entries)
{
  for (auto &ent : entries) {

    // Record file in dir
    std::size_t pos = ent.find_last_of('/');
    assert(pos != std::string::npos);
    std::string dir(ent.substr(0,pos));
    std::string file(ent.substr(pos+1));
    if (dir.empty())
      dir = "/";
    if (! file.empty()) {
      auto &de = dirmap_[dir];
      de.files.insert(file);
    }

    // Visit parent of dir if needed.
    addParent(dir);
  }
}

bool InspectFakeFS::exists(const std::string &path)
{
  std::size_t pos = path.find_last_of('/');
  assert(pos != std::string::npos);
  std::string dir(path.substr(0,pos));
  std::string base(path.substr(pos+1));
  if (dir.empty())
    dir = "/";
  auto it = dirmap_.find(dir);
  if (it == dirmap_.end())
    return false;
  if (base.empty())
    return true;
  if (it->second.files.find(base) != it->second.files.end())
    return true;
  if (it->second.dirs.find(base) != it->second.dirs.end())
    return true;
  return false;
}

stringvec InspectFakeFS::scanDir(const std::string &dir)
{
  stringvec result;
  auto it = dirmap_.find(dir);
  if (it == dirmap_.end())
    return result;
  auto &ds = it->second.dirs;
  for (auto &d : ds) {
    std::string path(dir);
    path += "/";
    path += d;
    result.push_back(path);
  }
  auto &fs = it->second.files;
  for (auto &f : fs) {
    std::string path(dir);
    path += "/";
    path += f;
    result.push_back(path);
  }
  return result;
}

bool InspectFakeFS::is_directory(const std::string &path)
{
  std::size_t pos = path.find_last_of('/');
  assert(pos != std::string::npos);
  std::string dir(path.substr(0,pos));
  if (dir.empty())
    dir = "/";
  auto it = dirmap_.find(dir);
  if (it == dirmap_.end())
    return false;
  std::string base(path.substr(pos+1));
  assert(! base.empty());
  if (it->second.dirs.find(base) != it->second.dirs.end())
    return true;
  return false;
}

std::string InspectFakeFS::toString()
{
  std::stringstream ss;
  for (auto &de : dirmap_) {
    ss << "dir " << de.first << ": ";
    for (auto &sd : de.second.dirs) {
      ss << " *" << sd;
    }
    for (auto &f : de.second.files) {
      ss << " " << f;
    }
    ss << "\n";
  }
  return ss.str();
}

template<class T>
bool expectToString(T &cand, const std::string &expected)
{
  std::string reason;
  std::string actual(cand.toString());
  bool equal = difftokens(expected, actual, reason);
  if (! equal)
    complainOnNequal(reason, expected, actual, false);
  return equal;
}

TEST(DriverUtilsTests, TestFakeFS) {

  {
    std::vector<std::string> t = { "/" };
    InspectFakeFS fake(t);

    const char *exp = R"RAW_RESULT(
       dir /:
    )RAW_RESULT";

    bool isOK = expectToString(fake, exp);
    EXPECT_TRUE(isOK);
  }

  {
    std::vector<std::string> t = { "/foo", "/bar", "/bar/baz" };
    InspectFakeFS fake(t);

    const char *exp = R"RAW_RESULT(
      dir /:  *bar foo
      dir /bar:  baz
    )RAW_RESULT";

    bool isOK = expectToString(fake, exp);
    EXPECT_TRUE(isOK);
  }
}

// Test harness for testing GCCInstallationDetector.

class DetectorHarness {
 public:
  DetectorHarness(const char *fileSystemContents,
                  const char *trip,
                  const char *sysroot)
      : triple_(trip),
        fs_(fileSystemContents),
        sysroot_(sysroot),
        detector_(triple_, "", sysroot_, fs_)
  {
    detector_.init();
  }

  GCCInstallationDetector &detector() { return detector_; }
  InspectFakeFS &fakefs() { return fs_; }

 private:
  llvm::Triple triple_;
  InspectFakeFS fs_;
  std::string sysroot_;
  GCCInstallationDetector detector_;
};

TEST(DriverUtilsTests, GCCInstallationDetectorBasicAmd64) {

  // Here we have two installations, version 6 and version 7.
  const char *install = R"RAW_RESULT(
      /mumble
      /usr/lib/gcc/x86_64-linux-gnu/6/32/crtbegin.o
      /usr/lib/gcc/x86_64-linux-gnu/blah
      /usr/lib/gcc/x86_64-linux-gnu/6/crtbegin.o
      /usr/lib/gcc/x86_64-linux-gnu/7/crtbegin.o
    )RAW_RESULT";

  // Case 1: no sysroot, looking for 64-bit compiler.
  DetectorHarness harness1(install, "x86_64-linux-gnu", "");
  const char *exp64 = R"RAW_RESULT(
      version: 7
      foundTriple: x86_64-linux-gnu
      libPath: /usr/lib/gcc/x86_64-linux-gnu/7
      parentLibPath: /usr/lib/gcc/x86_64-linux-gnu/7/../..
      installPath: /usr/lib/gcc/x86_64-linux-gnu/7
    )RAW_RESULT";
  bool isOK1 = expectToString(harness1.detector(), exp64);
  EXPECT_TRUE(isOK1);

  // Case 2: no sysroot, we're on a 64-bit machine looking for 32-bit
  // compiler. Here we pick up version 6 since version 7 in this case
  // has no 32-bit libraries.
  DetectorHarness harness2(install, "i386-linux-gnu", "");
  const char *exp32 = R"RAW_RESULT(
      version: 6
      foundTriple: x86_64-linux-gnu
      libPath: /usr/lib/gcc/x86_64-linux-gnu/6/32
      parentLibPath: /usr/lib/gcc/x86_64-linux-gnu/6/../../..
      installPath: /usr/lib/gcc/x86_64-linux-gnu/6
    )RAW_RESULT";
  bool isOK2 = expectToString(harness2.detector(), exp32);
  EXPECT_TRUE(isOK2);
}

TEST(DriverUtilsTests, GCCInstallationDetectorBasicARM64) {

  // Here we have two installations, version 6 and version 7.
  const char *install = R"RAW_RESULT(
      /mumble
      /usr/lib/gcc/aarch64-linux-gnu/blah
      /usr/lib/gcc/aarch64-linux-gnu/6/crtbegin.o
      /usr/lib/gcc/aarch64-linux-gnu/7/crtbegin.o
    )RAW_RESULT";

  // Case 1: no sysroot, looking for 64-bit compiler.
  // Gcc doesn't support multilib on Arm64, so don't need
  // to test that case.
  DetectorHarness harness1(install, "aarch64-linux-gnu", "");
  const char *exp64 = R"RAW_RESULT(
      version: 7
      foundTriple: aarch64-linux-gnu
      libPath: /usr/lib/gcc/aarch64-linux-gnu/7
      parentLibPath: /usr/lib/gcc/aarch64-linux-gnu/7/../..
      installPath: /usr/lib/gcc/aarch64-linux-gnu/7
    )RAW_RESULT";
  bool isOK1 = expectToString(harness1.detector(), exp64);
  EXPECT_TRUE(isOK1);
}

TEST(DriverUtilsTests, GCCInstallationDetectorSysRootAmd64) {

  const char *install = R"RAW_RESULT(
      /mumble
      /usr/lib/gcc/x86_64-linux-gnu/7/crtbegin.o
      /mysysroot/usr/lib/gcc/x86_64-linux-gnu/6.2.3/crtbegin.o
    )RAW_RESULT";

  // We have GCC 7 installed on the host, but GCC 6 in sysroot,
  // which in this case is what we want.
  DetectorHarness harness1(install, "x86_64-linux-gnu", "/mysysroot");
  const char *exp64 = R"RAW_RESULT(
      version: 6.2.3
      foundTriple: x86_64-linux-gnu
      libPath: /mysysroot/usr/lib/gcc/x86_64-linux-gnu/6.2.3
      parentLibPath: /mysysroot/usr/lib/gcc/x86_64-linux-gnu/6.2.3/../..
      installPath: /mysysroot/usr/lib/gcc/x86_64-linux-gnu/6.2.3
    )RAW_RESULT";
  bool isOK1 = expectToString(harness1.detector(), exp64);
  EXPECT_TRUE(isOK1);
}

TEST(DriverUtilsTests, GCCInstallationDetectorSysRootARM64) {

  const char *install = R"RAW_RESULT(
      /mumble
      /usr/lib/gcc/aarch64-linux-gnu/7/crtbegin.o
      /mysysroot/usr/lib/gcc/aarch64-linux-gnu/6.2.3/crtbegin.o
    )RAW_RESULT";

  // We have GCC 7 installed on the host, but GCC 6 in sysroot,
  // which in this case is what we want.
  DetectorHarness harness1(install, "aarch64-linux-gnu", "/mysysroot");
  const char *exp64 = R"RAW_RESULT(
      version: 6.2.3
      foundTriple: aarch64-linux-gnu
      libPath: /mysysroot/usr/lib/gcc/aarch64-linux-gnu/6.2.3
      parentLibPath: /mysysroot/usr/lib/gcc/aarch64-linux-gnu/6.2.3/../..
      installPath: /mysysroot/usr/lib/gcc/aarch64-linux-gnu/6.2.3
    )RAW_RESULT";
  bool isOK1 = expectToString(harness1.detector(), exp64);
  EXPECT_TRUE(isOK1);
}

TEST(DriverUtilsTests, GCCInstallationDetectorTripleAliasesAmd64) {

  // Regrettably, there is a fair amount of variation in terms
  // of target triples and how GCC is installed. This test checks
  // to make sure we can accommodate such differences.

  const char *install = R"RAW_RESULT(
      /mumble
      /usr/lib/gcc/x86_64-linux-gnu/5/crtbegin.o
      /usr/lib/gcc/x86_64-linux-gnu/7/crtbegin.o
    )RAW_RESULT";

  // Case 1: install is x86_64-linux-gnu, but we are looking for
  // x86_64-unknown-linux-gnu
  DetectorHarness harness1(install, "x86_64-unknown-linux-gnu", "");
  const char *exp1 = R"RAW_RESULT(
      version: 7
      foundTriple: x86_64-linux-gnu
      libPath: /usr/lib/gcc/x86_64-linux-gnu/7
      parentLibPath: /usr/lib/gcc/x86_64-linux-gnu/7/../..
      installPath: /usr/lib/gcc/x86_64-linux-gnu/7
    )RAW_RESULT";
  bool isOK1 = expectToString(harness1.detector(), exp1);
  EXPECT_TRUE(isOK1);

  // Case 2: install is x86_64-linux-gnu, but we are looking for
  // x86_64-redhat-linux-gnu
  DetectorHarness harness2(install, "x86_64-redhat-linux-gnu", "");
  const char *exp2 = R"RAW_RESULT(
      version: 7
      foundTriple: x86_64-linux-gnu
      libPath: /usr/lib/gcc/x86_64-linux-gnu/7
      parentLibPath: /usr/lib/gcc/x86_64-linux-gnu/7/../..
      installPath: /usr/lib/gcc/x86_64-linux-gnu/7
    )RAW_RESULT";
  bool isOK2 = expectToString(harness2.detector(), exp2);
  EXPECT_TRUE(isOK2);
}

TEST(DriverUtilsTests, GCCInstallationDetectorTripleAliasesARM64) {

  // Regrettably, there is a fair amount of variation in terms
  // of target triples and how GCC is installed. This test checks
  // to make sure we can accommodate such differences.

  const char *install = R"RAW_RESULT(
      /mumble
      /usr/lib/gcc/aarch64-linux-gnu/5/crtbegin.o
      /usr/lib/gcc/aarch64-linux-gnu/7/crtbegin.o
    )RAW_RESULT";

  // Case 1: install is aarch64-linux-gnu, but we are looking for
  // aarch64-unknown-linux-gnu
  DetectorHarness harness1(install, "aarch64-unknown-linux-gnu", "");
  const char *exp1 = R"RAW_RESULT(
      version: 7
      foundTriple: aarch64-linux-gnu
      libPath: /usr/lib/gcc/aarch64-linux-gnu/7
      parentLibPath: /usr/lib/gcc/aarch64-linux-gnu/7/../..
      installPath: /usr/lib/gcc/aarch64-linux-gnu/7
    )RAW_RESULT";
  bool isOK1 = expectToString(harness1.detector(), exp1);
  EXPECT_TRUE(isOK1);

  // Case 2: install is aarch64-linux-gnu, but we are looking for
  // aarch64-redhat-linux-gnu
  DetectorHarness harness2(install, "aarch64-redhat-linux-gnu", "");
  const char *exp2 = R"RAW_RESULT(
      version: 7
      foundTriple: aarch64-linux-gnu
      libPath: /usr/lib/gcc/aarch64-linux-gnu/7
      parentLibPath: /usr/lib/gcc/aarch64-linux-gnu/7/../..
      installPath: /usr/lib/gcc/aarch64-linux-gnu/7
    )RAW_RESULT";
  bool isOK2 = expectToString(harness2.detector(), exp2);
  EXPECT_TRUE(isOK2);
}

TEST(DriverUtilsTests, GCCInstallationDetectorBiarchAliasesAmd64) {
  const char *install = R"RAW_RESULT(
      /mumble
      /usr/lib/gcc/x86_64-redhat-linux/6/lib32/crtbegin.o
      /usr/lib/gcc/x86_64-redhat-linux/6/crtbegin.o
      /usr/lib/gcc/x86_64-redhat-linux/7/crtbegin.o
    )RAW_RESULT";

  // On 64-bit machine looking for 32-bit libraries, but here the
  // distro has decided to install 32-bit libraries in /lib32, not /32.
  DetectorHarness harness(install, "i386-linux-gnu", "");
  const char *exp32 = R"RAW_RESULT(
      version: 6
      foundTriple: x86_64-redhat-linux
      libPath: /usr/lib/gcc/x86_64-redhat-linux/6/lib32
      parentLibPath: /usr/lib/gcc/x86_64-redhat-linux/6/../../..
      installPath: /usr/lib/gcc/x86_64-redhat-linux/6
    )RAW_RESULT";
  bool isOK = expectToString(harness.detector(), exp32);
  EXPECT_TRUE(isOK);
}

} // namespace
