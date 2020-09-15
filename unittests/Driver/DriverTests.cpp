//===---- DriverTests.cpp -------------------------------------------------===//
//
// Copyright 2020 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
//
//===----------------------------------------------------------------------===//

#include <iostream>
#include <map>
#include <set>
#include <stdarg.h>

#include "Driver.h"
#include "Compilation.h"

#include "gtest/gtest.h"

#include "DiffUtils.h"

using namespace llvm;
using namespace gollvm::driver;
using namespace goBackendUnitTests;

namespace {

inline std::vector<const char *> A(const char *arg, ...) {
  va_list ap;
  va_start(ap, arg);
  std::vector<const char *> result;
  while(arg != nullptr) {
    result.push_back(arg);
    arg = va_arg(ap, const char *);
  }
  return result;
}

class DrvTestHarness {
 public:
  explicit DrvTestHarness(const std::vector<const char *> args)
      : args_(args) { }

  // Returns non-zero on error, zero for success.
  unsigned Perform();

  // Test that a dump of driver actions matches the expeced result.
  bool expectActions(const ExpectedDump &ed);

 private:
  const std::vector<const char *> args_;
  std::string actionsDump_;
};

unsigned DrvTestHarness::Perform()
{
  std::unique_ptr<opt::OptTable> opts =
      gollvm::options::createGollvmDriverOptTable();
  unsigned missingArgIndex, missingArgCount;
  ArrayRef<const char *> argvv = makeArrayRef(args_);
  opt::InputArgList args =
      opts->ParseArgs(argvv, missingArgIndex, missingArgCount);

  // Complain about missing arguments.
  if (missingArgIndex != 0) {
    errs() << "error: argument to '"
           << args.getArgString(missingArgIndex)
           << "' option missing, expected "
           << missingArgCount << " value(s)\n";
    return 1;
  }

  // Look for unknown options.
  for (const opt::Arg *arg : args.filtered(gollvm::options::OPT_UNKNOWN)) {
    errs() << "error: unrecognized command line option '"
             << arg->getAsString(args) << "'\n";
    return 2;
  }

  // Create driver.
  Driver driver(args, opts.get(), "llvm-goc", true);
  driver.setUnitTesting();

  // Set up driver, select target and toolchain.
  ToolChain *toolchain = driver.setup();
  if (toolchain == nullptr)
    return 1;

  // Build compilation; construct actions for this compile.
  std::unique_ptr<Compilation> compilation =
      driver.buildCompilation(*toolchain);
  if (!driver.buildActions(*compilation))
    return 2;
  if (!driver.processActions(*compilation))
    return 3;
  actionsDump_ = driver.dumpActions(*compilation);
  return 0;
}

bool DrvTestHarness::expectActions(const ExpectedDump &ed)
{
  const std::string &expected = ed.content;
  std::string reason;
  auto actual = actionsDump_;
  bool equal = difftokens(expected, actual, reason);
  if (! equal)
    complainOnNequal(reason, ed, actual);
  return equal;
}

TEST(DriverTests, SimpleCompile) {
  DrvTestHarness h(A("-c", "foo.go", nullptr));

  DECLARE_EXPECTED_OUTPUT(exp, R"RAW_RESULT(
    Action compile+assemble
      inputs:
        inputfile Artifact arg(foo.go)
      output:
        Artifact file(foo.o)
  )RAW_RESULT");

  unsigned res = h.Perform();
  ASSERT_TRUE(res == 0 && "Setup failed");

  bool isOK = h.expectActions(exp);
  EXPECT_TRUE(isOK && "Actions dump does not have expected contents");
}

TEST(DriverTests, NoIntegAsmCompile) {
  DrvTestHarness h(A("-c", "-fno-integrated-as", "foo.go", nullptr));

  DECLARE_EXPECTED_OUTPUT(exp, R"RAW_RESULT(
    Action compile
      inputs:
        inputfile Artifact arg(foo.go)
      output:
        Artifact file(/tmp/out.compile.0)
    Action assemble
      inputs:
        compile
      output:
        Artifact file(foo.o)
  )RAW_RESULT");

  unsigned res = h.Perform();
  ASSERT_TRUE(res == 0 && "Setup failed");

  bool isOK = h.expectActions(exp);
  EXPECT_TRUE(isOK && "Actions dump does not have expected contents");
}

TEST(DriverTests, CompileToAsm) {
  DrvTestHarness h(A("-S", "foo.go", nullptr));

  DECLARE_EXPECTED_OUTPUT(exp, R"RAW_RESULT(
    Action compile
      inputs:
        inputfile Artifact arg(foo.go)
      output:
        Artifact file(foo.s)
  )RAW_RESULT");

  unsigned res = h.Perform();
  ASSERT_TRUE(res == 0 && "Setup failed");

  bool isOK = h.expectActions(exp);
  EXPECT_TRUE(isOK && "Actions dump does not have expected contents");
}

TEST(DriverTests, CompileToLllvmBitcode) {
  DrvTestHarness h(A("-emit-llvm", "-c", "foo.go", nullptr));

  DECLARE_EXPECTED_OUTPUT(exp, R"RAW_RESULT(
    Action compile
      inputs:
        inputfile Artifact arg(foo.go)
      output:
        Artifact file(foo.bc)
  )RAW_RESULT");

  unsigned res = h.Perform();
  ASSERT_TRUE(res == 0 && "Setup failed");

  bool isOK = h.expectActions(exp);
  EXPECT_TRUE(isOK && "Actions dump does not have expected contents");
}

TEST(DriverTests, CompileToLllvmIRAsm) {
  DrvTestHarness h(A("-emit-llvm", "-S", "foo.go", nullptr));

  DECLARE_EXPECTED_OUTPUT(exp, R"RAW_RESULT(
    Action compile
      inputs:
        inputfile Artifact arg(foo.go)
      output:
        Artifact file(foo.ll)
  )RAW_RESULT");

  unsigned res = h.Perform();
  ASSERT_TRUE(res == 0 && "Setup failed");

  bool isOK = h.expectActions(exp);
  EXPECT_TRUE(isOK && "Actions dump does not have expected contents");
}

TEST(DriverTests, AssembleAction) {
  DrvTestHarness h(A("-c", "foo.s", "-o", "blah.o", nullptr));

  DECLARE_EXPECTED_OUTPUT(exp, R"RAW_RESULT(
    Action assemble
      inputs:
        inputfile Artifact arg(foo.s)
      output:
        Artifact arg(blah.o)
  )RAW_RESULT");

  unsigned res = h.Perform();
  ASSERT_TRUE(res == 0 && "Setup failed");

  bool isOK = h.expectActions(exp);
  EXPECT_TRUE(isOK && "Actions dump does not have expected contents");
}

TEST(DriverTests, CompileAndLinkAction) {
  DrvTestHarness h(A("foo1.go", "foo2.go", "-o", "foo.exe", nullptr));

  DECLARE_EXPECTED_OUTPUT(exp, R"RAW_RESULT(
    Action compile+assemble
      inputs:
        inputfile Artifact arg(foo1.go)
        inputfile Artifact arg(foo2.go)
      output:
        Artifact file(/tmp/out.compile+assemble.0)
    Action link
      inputs:
        compile+assemble
      output:
        Artifact arg(foo.exe)
  )RAW_RESULT");

  unsigned res = h.Perform();
  ASSERT_TRUE(res == 0 && "Setup failed");

  bool isOK = h.expectActions(exp);
  EXPECT_TRUE(isOK && "Actions dump does not have expected contents");
}

} // namespace
