//===- llvm/tools/gollvm/unittests/BackendCore/BackendDebugEmit.cpp -----===//
//
// Copyright 2018 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
//
//===----------------------------------------------------------------------===//

#include "TestUtils.h"
#include "go-llvm-backend.h"
#include "gtest/gtest.h"

using namespace goBackendUnitTests;

namespace {

// Insure that dbg.declare is emitted for a used-defined local
// variable. Remark: I worry that this unit test may be too brittle
// (vulnerable to spurious failures if other things in the bridge are
// changed). Perhaps there is some other way to verify this
// functionality.

class BackendDebugEmit : public testing::TestWithParam<llvm::CallingConv::ID> {
};

INSTANTIATE_TEST_CASE_P(
    UnitTest, BackendDebugEmit,
    goBackendUnitTests::CConvs,
    [](const testing::TestParamInfo<BackendDebugEmit::ParamType> &info) {
      std::string name = goBackendUnitTests::ccName(info.param);
      return name;
    });

TEST_P(BackendDebugEmit, TestSimpleDecl) {
  auto cc = GetParam();
  FcnTestHarness h(cc);
  Llvm_backend *be = h.be();
  BFunctionType *befty = mkFuncTyp(be, L_END);
  Bfunction *func = h.mkFunction("foo", befty);

  Btype *bu32t = be->integer_type(true, 32);
  h.mkLocal("x", bu32t);

  const char *exp = R"RAW_RESULT(
    define void @foo(i8* nest %nest.0) #0 {
    entry:
      %x = alloca i32
      store i32 0, i32* %x
      call void @llvm.dbg.declare(metadata i32* %x, metadata !5,
                                  metadata !DIExpression()), !dbg !12
      ret void
    }
  )RAW_RESULT";

  bool broken = h.finish(PreserveDebugInfo);
  EXPECT_FALSE(broken && "Module failed to verify.");

  bool isOK = h.expectValue(func->function(), exp);
  EXPECT_TRUE(isOK && "Function does not have expected contents");
}

TEST(BackendDebugEmit, TestSimpleDecl2Amd64) {
  // Test that parameters of empty function are handled correctly.
  FcnTestHarness h(llvm::CallingConv::X86_64_SysV);
  Llvm_backend *be = h.be();
  Btype *bi64t = be->integer_type(false, 64);
  Btype *bst = mkBackendStruct(be, bi64t, "f1",
                                   bi64t, "f2",
                                   bi64t, "f3",
                                   nullptr); // large struct, pass by reference
  BFunctionType *befty = mkFuncTyp(be, L_PARM, bst, L_END);
  Bfunction *func = h.mkFunction("foo", befty);

  // function with no code

  bool broken = h.finish(PreserveDebugInfo);
  EXPECT_FALSE(broken && "Module failed to verify.");

  const char *exp = R"RAW_RESULT(
    define void @foo(i8* nest %nest.0, { i64, i64, i64 }* byval %p0) #0 {
    entry:
      call void @llvm.dbg.declare(metadata { i64, i64, i64 }* %p0, metadata !5,
                                  metadata !DIExpression()), !dbg !18
      ret void
    }
  )RAW_RESULT";

  bool isOK = h.expectValue(func->function(), exp);
  EXPECT_TRUE(isOK && "Function does not have expected contents");
}

TEST(BackendDebugEmit, TestSimpleDecl2Arm64) {
  // Test that parameters of empty function are handled correctly.
  FcnTestHarness h(llvm::CallingConv::ARM_AAPCS);
  Llvm_backend *be = h.be();
  Btype *bi64t = be->integer_type(false, 64);
  Btype *bst = mkBackendStruct(be, bi64t, "f1", bi64t, "f2", bi64t, "f3",
                               nullptr); // large struct, pass by reference
  BFunctionType *befty = mkFuncTyp(be, L_PARM, bst, L_END);
  Bfunction *func = h.mkFunction("foo", befty);

  // function with no code

  bool broken = h.finish(PreserveDebugInfo);
  EXPECT_FALSE(broken && "Module failed to verify.");

  const char *exp = R"RAW_RESULT(
    define void @foo(i8* nest %nest.0, { i64, i64, i64 }* %p0) #0 {
    entry:
      call void @llvm.dbg.declare(metadata { i64, i64, i64 }* %p0, metadata !5,
                                  metadata !DIExpression()), !dbg !18
      ret void
    }
  )RAW_RESULT";

  bool isOK = h.expectValue(func->function(), exp);
  EXPECT_TRUE(isOK && "Function does not have expected contents");
}

// This test is designed to make sure that debug meta-data generation
// handles corner clases like vars with zero size (empty struct).

// working propery
TEST_P(BackendDebugEmit, MoreComplexVarDecls) {
  auto cc = GetParam();
  FcnTestHarness h(cc);
  Llvm_backend *be = h.be();

  Btype *bi32t = be->integer_type(false, 32);
  Btype *set = mkBackendStruct(be, nullptr); // struct with no fields
  Bexpression *val10 = mkInt64Const(be, int64_t(10));
  Btype *beat = be->array_type(set, val10);
  Btype *bc64t = be->complex_type(64);
  Btype *bc128t = be->complex_type(128);

  BFunctionType *befty1 = mkFuncTyp(be,
                                    L_RES, set,
                                    L_PARM, set,
                                    L_PARM, beat,
                                    L_PARM, bi32t,
                                    L_PARM, bc64t,
                                    L_PARM, bc128t,
                                    L_PARM, set,
                                    L_PARM, beat,
                                    L_END);
  Bfunction *func = h.mkFunction("foo", befty1);
  BFunctionType *befty2 = mkFuncTyp(be,
                                    L_RES, set,
                                    L_END);
  Bfunction *func2 = mkFuncFromType(be, "bar", befty2);

  h.mkLocal("la", set);
  h.mkLocal("lb", beat);
  h.mkLocal("lc", bi32t);

  Location loc;
  std::vector<Bvariable *> vlist;
  vlist.push_back(be->local_variable(func, "n1", set, nullptr, false, loc));
  vlist.push_back(be->local_variable(func, "n2", beat, nullptr, false, loc));
  vlist.push_back(be->local_variable(func, "n3", bi32t, nullptr, false, loc));
  h.newBlock(&vlist);

  Bexpression *fn2 = be->function_code_expression(func2, loc);
  std::vector<Bexpression *> noargs;
  Bexpression *call2 =
      be->call_expression(func2, fn2, noargs, nullptr,  h.loc());
  h.addStmt(be->init_statement(func, vlist[0], call2));
  h.addStmt(be->init_statement(func, vlist[1], be->zero_expression(beat)));
  h.addStmt(be->init_statement(func, vlist[2], be->zero_expression(bi32t)));

  // return foo(f1, f2, 4, f1, f2)
  Bexpression *fn = be->function_code_expression(func, loc);
  Bvariable *p0 = func->getNthParamVar(0);
  Bvariable *p1 = func->getNthParamVar(1);
  std::vector<Bexpression *> args;
  args.push_back(be->var_expression(p0, loc));
  args.push_back(be->var_expression(p1, loc));
  args.push_back(mkInt32Const(be, 4));
  args.push_back(be->var_expression(p0, loc));
  args.push_back(be->var_expression(p1, loc));
  Bexpression *call = be->call_expression(func, fn, args, nullptr, h.loc());
  std::vector<Bexpression *> rvals;
  rvals.push_back(call);
  h.mkReturn(rvals);

  bool broken = h.finish(PreserveDebugInfo);
  EXPECT_FALSE(broken && "Module failed to verify.");

  std::string fdump = repr(func->function());
  std::vector<std::string> tokens = tokenize(fdump);
  unsigned declcount = 0;
  for (auto t : tokens)
    if (t == "@llvm.dbg.declare(metadata")
      declcount += 1;

  // seven formals and six locals => 13 var decls
  EXPECT_EQ(declcount, 13u);
  if (declcount != 13)
    std::cerr << fdump;
}

TEST_P(BackendDebugEmit, TestDeadLocalVar) {
  auto cc = GetParam();
  // Test that dead local variable doesn't cause problem.
  FcnTestHarness h(cc);
  Llvm_backend *be = h.be();
  BFunctionType *befty = mkFuncTyp(be, L_END);
  Bfunction *func = h.mkFunction("foo", befty);

  h.mkReturn(std::vector<Bexpression*>{});
  Btype *bu32t = be->integer_type(true, 32);
  h.mkLocal("x", bu32t);

  const char *exp = R"RAW_RESULT(
    define void @foo(i8* nest %nest.0) #0 !dbg !5 {
    entry:
      %x = alloca i32
      ret void, !dbg !10
    }
  )RAW_RESULT";

  bool broken = h.finish(PreserveDebugInfo);
  EXPECT_FALSE(broken && "Module failed to verify.");

  bool isOK = h.expectValue(func->function(), exp);
  EXPECT_TRUE(isOK && "Function does not have expected contents");
}

TEST_P(BackendDebugEmit, TestGlobalVarDebugEmit) {
  auto cc = GetParam();
  FcnTestHarness h(cc, "foo");
  Llvm_backend *be = h.be();
  Location loc = h.loc();

  Btype *bi32t = be->integer_type(false, 32);
  Bvariable *g1 =
      be->global_variable("_bar", "bar", bi32t,
                          true,         /* is_external */
                          false,        /* is_hidden */
                          false,        /* unique_section */
                          loc);
  be->global_variable_set_init(g1, mkInt32Const(be, 101));

  bool broken = h.finish(PreserveDebugInfo);
  EXPECT_FALSE(broken && "Module failed to verify.");

  // This is a long way from verifying that the debug meta-data is in fact
  // completely correct, but at least it checks that the global
  // wasn't skipped.
  bool ok = h.expectModuleDumpContains("!DIGlobalVariable(name: \"bar\",");
  EXPECT_TRUE(ok);
}

TEST_P(BackendDebugEmit, TestDebugPrefixMap) {
  auto cc = GetParam();
  FcnTestHarness h(cc);
  Llvm_backend *be = h.be();
  Btype *bi64t = be->integer_type(false, 64);
  BFunctionType *befty = mkFuncTyp(be, L_PARM, bi64t, L_RES, bi64t, L_END);

  llvm::StringRef from2("/bar");
  llvm::StringRef to2("/something");
  be->addDebugPrefix(std::make_pair(from2, to2));

  Location loc = h.newFileLineLoc("/bar/another/barcode.go", 11);
  Bfunction *func = h.mkFunction("bar", befty);
  Bvariable *p0 = func->getNthParamVar(0);
  Bexpression *vec = be->var_expression(p0, loc);
  h.mkReturn(std::vector<Bexpression*>{vec});

  bool broken = h.finish(PreserveDebugInfo);
  EXPECT_FALSE(broken && "Module failed to verify.");

  // Check for remapped source file.
  bool ok = h.expectModuleDumpContains("!DIFile(filename: \"barcode.go\", directory: \"/something/another\")");
  EXPECT_TRUE(ok);
}

TEST_P(BackendDebugEmit, TestFileLineDirectives) {
  auto cc = GetParam();
  FcnTestHarness h(cc);
  Llvm_backend *be = h.be();
  Btype *bi64t = be->integer_type(false, 64);
  BFunctionType *befty = mkFuncTyp(be, L_PARM, bi64t, L_RES, bi64t, L_END);
  Bfunction *func = h.mkFunction("bar", befty);
  Bvariable *p0 = func->getNthParamVar(0);

  Location loc = h.newFileLineLoc("watermelon.go", 43);
  Bexpression *vex1 = be->var_expression(p0, loc);
  Bvariable *xv = h.mkLocal("x", bi64t, vex1);

  loc = h.newFileLineLoc("kiwifruit.go", 43);
  Bexpression *vex2 = be->var_expression(p0, loc);
  Bexpression *vex3 = be->var_expression(xv, loc);
  h.mkAssign(vex2, vex3);

  loc = h.newFileLineLoc("apple.go", 11);
  Bexpression *vex4 = be->var_expression(p0, loc);
  h.mkReturn(std::vector<Bexpression*>{vex4});

  bool broken = h.finish(PreserveDebugInfo);
  EXPECT_FALSE(broken && "Module failed to verify.");

  // Three of the constructs above had different files applied to them
  // (equivalent of Go //line directive); make sure that the files
  // appear in the meta-data.
  bool ok = h.expectModuleDumpContains("!DIFile(filename: \"watermelon.go\", directory: \"\")");
  EXPECT_TRUE(ok);
  ok = h.expectModuleDumpContains("!DIFile(filename: \"kiwifruit.go\", directory: \"\")");
  EXPECT_TRUE(ok);
  ok = h.expectModuleDumpContains("!DIFile(filename: \"apple.go\", directory: \"\")");
  EXPECT_TRUE(ok);
}

} // namespace
