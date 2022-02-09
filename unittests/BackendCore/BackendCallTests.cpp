//===- llvm/tools/gollvm/unittests/BackendCore/BackendCallTests.cpp ------===//
//
// Copyright 2018 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
//
//===----------------------------------------------------------------------===//

#include "TestUtils.h"
#include "go-llvm-backend.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Function.h"
#include "gtest/gtest.h"

using namespace llvm;
using namespace goBackendUnitTests;

namespace {

class BackendCallTests : public testing::TestWithParam<llvm::CallingConv::ID> {
};

INSTANTIATE_TEST_SUITE_P(
    UnitTest, BackendCallTests,
    goBackendUnitTests::CConvs,
    [](const testing::TestParamInfo<BackendCallTests::ParamType> &info) {
      std::string name = goBackendUnitTests::ccName(info.param);
      return name;
    });

TEST_P(BackendCallTests, TestSimpleCall) {
  auto cc = GetParam();
  FcnTestHarness h(cc, "foo");
  Llvm_backend *be = h.be();
  Bfunction *func = h.func();
  Location loc;

  Btype *bi64t = be->integer_type(false, 64);
  Btype *bpi64t = be->pointer_type(bi64t);
  Bexpression *fn = be->function_code_expression(func, loc);
  std::vector<Bexpression *> args;
  args.push_back(mkInt32Const(be, int64_t(3)));
  args.push_back(mkInt32Const(be, int64_t(6)));
  args.push_back(be->zero_expression(bpi64t));
  Bexpression *call = be->call_expression(func, fn, args, nullptr, h.loc());
  Bvariable *x = h.mkLocal("x", bi64t, call);
  h.mkReturn(be->var_expression(x, loc));

  DECLARE_EXPECTED_OUTPUT(exp, R"RAW_RESULT(
    %call.0 = call addrspace(0) i64 @foo(i8* nest undef, i32 3, i32 6, i64* null)
    store i64 %call.0, i64* %x, align 8
    %x.ld.0 = load i64, i64* %x, align 8
    ret i64 %x.ld.0
  )RAW_RESULT");

  bool isOK = h.expectBlock(exp);
  EXPECT_TRUE(isOK && "Block does not have expected contents");

  bool broken = h.finish(PreserveDebugInfo);
  EXPECT_FALSE(broken && "Module failed to verify.");
}

TEST_P(BackendCallTests, CallToVoid) {
  auto cc = GetParam();
  FcnTestHarness h(cc, "foo");
  Llvm_backend *be = h.be();
  Bfunction *func = h.func();
  Location loc;

  // Declare a function bar with no args and no return.
  Btype *befty = mkFuncTyp(be, L_END);
  unsigned fflags = (Backend::function_is_visible |
                     Backend::function_is_declaration);
  Bfunction *befcn = be->function(befty, "bar", "bar", fflags, loc);

  // Create call to it
  Bexpression *fn = be->function_code_expression(befcn, loc);
  std::vector<Bexpression *> args;
  Bexpression *call = be->call_expression(func, fn, args, nullptr, loc);
  h.mkExprStmt(call);

  DECLARE_EXPECTED_OUTPUT(exp, R"RAW_RESULT(
    call addrspace(0) void @bar(i8* nest undef)
  )RAW_RESULT");

  bool isOK = h.expectBlock(exp);
  EXPECT_TRUE(isOK && "Block does not have expected contents");

  bool broken = h.finish(StripDebugInfo);
  EXPECT_FALSE(broken && "Module failed to verify.");
}

TEST_P(BackendCallTests, MultiReturnCall) {
  auto cc = GetParam();
  FcnTestHarness h(cc);
  Llvm_backend *be = h.be();

  // Create function with multiple returns
  Btype *bi64t = be->integer_type(false, 64);
  Btype *bi32t = be->integer_type(false, 32);
  Btype *bi8t = be->integer_type(false, 8);
  BFunctionType *befty1 = mkFuncTyp(be,
                                    L_PARM, be->pointer_type(bi8t),
                                    L_RES, be->pointer_type(bi8t),
                                    L_RES, be->pointer_type(bi32t),
                                    L_RES, be->pointer_type(bi64t),
                                    L_RES, bi64t,
                                    L_END);
  Bfunction *func = h.mkFunction("foo", befty1);

  // Emit a suitable return suitable for "foo" as declared above.
  // This returns a constant expression.
  std::vector<Bexpression *> rvals = {
    be->nil_pointer_expression(),
    be->nil_pointer_expression(),
    be->nil_pointer_expression(),
    mkInt64Const(be, 101) };
  Bstatement *s1 = h.mkReturn(rvals, FcnTestHarness::NoAppend);

  {
    DECLARE_EXPECTED_OUTPUT(exp, R"RAW_RESULT(
      %cast.0 = bitcast { i8*, i32*, i64*, i64 }* %sret.formal.0 to i8*
      call addrspace(0) void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %cast.0, i8* align 8 bitcast ({ i8*, i32*, i64*, i64 }* @const.0 to i8*), i64 32, i1 false)
      ret void
    )RAW_RESULT");

    bool isOK = h.expectStmt(s1, exp);
    EXPECT_TRUE(isOK && "First return stmt does not have expected contents");
  }

  // This is intended to be something like
  //
  //  return p8, nil, nil, 101
  //
  Bvariable *p1 = func->getNthParamVar(0);
  Bexpression *vex = be->var_expression(p1, Location());
  std::vector<Bexpression *> rvals2 = {
    vex,
    be->nil_pointer_expression(),
    be->nil_pointer_expression(),
    mkInt64Const(be, 101) };
  Bstatement *s2 = h.mkReturn(rvals2, FcnTestHarness::NoAppend);

  {
    DECLARE_EXPECTED_OUTPUT(exp, R"RAW_RESULT(
    %p0.ld.0 = load i8*, i8** %p0.addr, align 8
    %field.0 = getelementptr inbounds { i8*, i32*, i64*, i64 }, { i8*, i32*, i64*, i64 }* %tmp.0, i32 0, i32 0
    store i8* %p0.ld.0, i8** %field.0, align 8
    %field.1 = getelementptr inbounds { i8*, i32*, i64*, i64 }, { i8*, i32*, i64*, i64 }* %tmp.0, i32 0, i32 1
    store i32* null, i32** %field.1, align 8
    %field.2 = getelementptr inbounds { i8*, i32*, i64*, i64 }, { i8*, i32*, i64*, i64 }* %tmp.0, i32 0, i32 2
    store i64* null, i64** %field.2, align 8
    %field.3 = getelementptr inbounds { i8*, i32*, i64*, i64 }, { i8*, i32*, i64*, i64 }* %tmp.0, i32 0, i32 3
    store i64 101, i64* %field.3, align 8
    %cast.2 = bitcast { i8*, i32*, i64*, i64 }* %sret.formal.0 to i8*
    %cast.3 = bitcast { i8*, i32*, i64*, i64 }* %tmp.0 to i8*
    call addrspace(0) void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %cast.2, i8* align 8 %cast.3, i64 32, i1 false)
    ret void
    )RAW_RESULT");

    bool isOK = h.expectStmt(s2, exp);
    EXPECT_TRUE(isOK && "Second return stmt does not have expected contents");
  }

  // If statement
  Location loc;
  Bexpression *ve2 = be->var_expression(p1, Location());
  Bexpression *npe = be->nil_pointer_expression();
  Bexpression *cmp = be->binary_expression(OPERATOR_EQEQ, ve2, npe, loc);
  h.mkIf(cmp, s1, s2);

  bool broken = h.finish(StripDebugInfo);
  EXPECT_FALSE(broken && "Module failed to verify.");
}

TEST_P(BackendCallTests, CallToNoReturnFunction) {
  auto cc = GetParam();
  FcnTestHarness h(cc);
  Llvm_backend *be = h.be();
  BFunctionType *befty = mkFuncTyp(be, L_END);
  Bfunction *func = h.mkFunction("foo", befty);
  Location loc;

  // Declare a function 'noret' with no args and no return.
  unsigned fflags = (Backend::function_is_visible |
                     Backend::function_is_declaration |
                     Backend::function_does_not_return);
  Bfunction *nrfcn = be->function(befty, "noret", "noret", fflags, loc);

  // Create a block containing two no-return calls. The intent here is to make
  // sure that the bridge detects and deletes instructions appearing downstream
  // of a no-return call.
  std::vector<Bexpression *> args;
  Bexpression *fn1 = be->function_code_expression(nrfcn, loc);
  Bexpression *call1 = be->call_expression(nrfcn, fn1, args, nullptr, loc);
  Bstatement *cs1 = h.mkExprStmt(call1, FcnTestHarness::NoAppend);
  Bblock *nrcblock = mkBlockFromStmt(be, func, cs1);
  Bexpression *fn2 = be->function_code_expression(nrfcn, loc);
  Bexpression *call2 = be->call_expression(nrfcn, fn2, args, nullptr, loc);
  Bstatement *cs2 = h.mkExprStmt(call2, FcnTestHarness::NoAppend);
  addStmtToBlock(be, nrcblock, cs2);

  // Create an "if" statement branching to the block above
  Bexpression *cond = be->boolean_constant_expression(true);
  h.mkIf(cond, be->block_statement(nrcblock), nullptr,
         FcnTestHarness::YesAppend);

  DECLARE_EXPECTED_OUTPUT(exp, R"RAW_RESULT(
    define void @foo(i8* nest %nest.0) #0 {
    entry:
      br i1 true, label %then.0, label %else.0

    then.0:                                           ; preds = %entry
      call void @noret(i8* nest undef)
      unreachable

    fallthrough.0:                                    ; preds = %else.0
      ret void

    else.0:                                           ; preds = %entry
      br label %fallthrough.0
    }
  )RAW_RESULT");

  bool broken = h.finish(StripDebugInfo);
  EXPECT_FALSE(broken && "Module failed to verify.");

  bool isOK = h.expectValue(func->function(), exp);
  EXPECT_TRUE(isOK && "Function does not have expected contents");
}

// TODO: We should have written a test for static link, but because in static
// link mode, the return type of getg is a temporary type, such as %"type
// 0xaaaafc36b690", this value is not fixed, so we can not determine the
// expected result. However, the processing method of static link and dynamic
// link is the same, but the instruction is slightly different.
TEST(BackendCallTests, TestMakeGetgDynamicArm64) {
  FcnTestHarness h(llvm::CallingConv::ARM_AAPCS, "foo");
  Llvm_backend *be = h.be();
  be->module().setPICLevel(llvm::PICLevel::BigPIC);
  Bfunction *func = h.func();
  Location loc;

  // Declare a function "func runtime.getg() *int64", in fact, the prototype of
  // runtime.getg is "func runtime.getg() *g", but it is difficult to construct
  // the structure of g, so we use *int64 to simulate.
  Btype *bi64t = be->integer_type(false, 64);
  Btype *bpi64t = be->pointer_type(bi64t);
  Btype *befty1 = mkFuncTyp(be, L_RES, bpi64t, L_END);
  unsigned fflags =
      (Backend::function_is_visible | Backend::function_is_declaration);
  Bfunction *befcn1 =
      be->function(befty1, "runtime.getg", "runtime.getg", fflags, loc);

  // Declare a function bar with no args and no return.
  Btype *befty2 = mkFuncTyp(be, L_END);
  Bfunction *befcn2 = be->function(befty2, "bar", "bar", fflags, loc);

  // x := getg()
  Bexpression *fn = be->function_code_expression(befcn1, loc);
  std::vector<Bexpression *> args;
  Bexpression *call = be->call_expression(func, fn, args, nullptr, loc);
  Bvariable *x = h.mkLocal("x", bpi64t, call);
  Bexpression *vex = be->var_expression(x, loc);

  // Create call to bar()
  fn = be->function_code_expression(befcn2, loc);
  call = be->call_expression(func, fn, args, nullptr, loc);
  h.mkExprStmt(call);

  // y := getg()
  fn = be->function_code_expression(befcn1, loc);
  call = be->call_expression(func, fn, args, nullptr, loc);
  Bvariable *y = h.mkLocal("y", bpi64t, call);
  Bexpression *vey = be->var_expression(y, loc);

  // z := *x + *y, this makes no sense, just to make x and y be used.
  Bexpression *xpy = be->binary_expression(
      OPERATOR_PLUS, be->indirect_expression(bi64t, vex, false, loc),
      be->indirect_expression(bi64t, vey, false, loc), loc);
  Bvariable *z = h.mkLocal("z", bi64t, xpy);
  // return z
  h.mkReturn(be->var_expression(z, loc));

  DECLARE_EXPECTED_OUTPUT(exp, R"RAW_RESULT(
    %asmcall.0 = call addrspace(0) i64* asm sideeffect "adrp x0, :tlsdesc:runtime.g\0Aldr  $0, [x0, :tlsdesc_lo12:runtime.g]\0Aadd  x0, x0, :tlsdesc_lo12:runtime.g\0A.tlsdesccall runtime.g\0Ablr  $0\0Amrs  $0, TPIDR_EL0\0Aldr  $0, [$0, x0]\0A", "=r,~{x0}"()
    store i64* %asmcall.0, i64** %x, align 8
    call addrspace(0) void @bar(i8* nest undef)
    %asmcall.1 = call addrspace(0) i64* asm sideeffect "adrp x0, :tlsdesc:runtime.g\0Aldr  $0, [x0, :tlsdesc_lo12:runtime.g]\0Aadd  x0, x0, :tlsdesc_lo12:runtime.g\0A.tlsdesccall runtime.g\0Ablr  $0\0Amrs  $0, TPIDR_EL0\0Aldr  $0, [$0, x0]\0A", "=r,~{x0}"()
    store i64* %asmcall.1, i64** %y, align 8
    %x.ld.0 = load i64*, i64** %x, align 8
    %.ld.0 = load i64, i64* %x.ld.0, align 8
    %y.ld.0 = load i64*, i64** %y, align 8
    %.ld.1 = load i64, i64* %y.ld.0, align 8
    %add.0 = add i64 %.ld.0, %.ld.1
    store i64 %add.0, i64* %z, align 8
    %z.ld.0 = load i64, i64* %z, align 8
    ret i64 %z.ld.0
  )RAW_RESULT");

  bool isOK = h.expectBlock(exp);
  EXPECT_TRUE(isOK && "Block does not have expected contents");

  bool broken = h.finish(StripDebugInfo);
  EXPECT_FALSE(broken && "Module failed to verify.");
}

} // namespace
