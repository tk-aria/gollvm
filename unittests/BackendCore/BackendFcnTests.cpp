//===- llvm/tools/gollvm/unittests/BackendCore/BackendFcnTests.cpp ------===//
//
// Copyright 2018 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
//
//===----------------------------------------------------------------------===//

#include "TestUtils.h"
#include "go-llvm-backend.h"
#include "llvm/IR/Function.h"
#include "gtest/gtest.h"

using namespace llvm;
using namespace goBackendUnitTests;

namespace {

class BackendFcnTests : public testing::TestWithParam<llvm::CallingConv::ID> {};

INSTANTIATE_TEST_CASE_P(
    UnitTest, BackendFcnTests,
    goBackendUnitTests::CConvs,
    [](const testing::TestParamInfo<BackendFcnTests::ParamType> &info) {
      std::string name = goBackendUnitTests::ccName(info.param);
      return name;
    });

TEST_P(BackendFcnTests, MakeEmptyFunction) {
  auto cc = GetParam();
  // Create empty function
  FcnTestHarness h(cc);
  Llvm_backend *be = h.be();
  BFunctionType *befty1 = mkFuncTyp(be, L_END);
  h.mkFunction("foo", befty1);

  DECLARE_EXPECTED_OUTPUT(exp, R"RAW_RESULT(
    )RAW_RESULT");

  bool isOK = h.expectBlock(exp);
  EXPECT_TRUE(isOK && "Block does not have expected contents");

  bool broken = h.finish(PreserveDebugInfo);
  EXPECT_FALSE(broken && "Module failed to verify.");
}

TEST_P(BackendFcnTests, MakeFuncWithLotsOfArgs) {
  auto cc = GetParam();
  // Create empty function
  FcnTestHarness h(cc);
  Llvm_backend *be = h.be();
  Btype *bi32t = be->integer_type(false, 32);
  Btype *bi64t = be->integer_type(false, 64);
  Bexpression *val10 = mkInt64Const(be, int64_t(10));
  Btype *at10 = be->array_type(bi64t, val10);
  Btype *st3 = mkBackendThreeFieldStruct(be);
  BFunctionType *befty1 = mkFuncTyp(be,
                                    L_RCV, st3,
                                    L_PARM, at10,
                                    L_PARM, be->bool_type(),
                                    L_PARM, bi32t,
                                    L_PARM, bi64t,
                                    L_PARM, be->pointer_type(st3),
                                    L_RES, bi32t,
                                    L_RES, bi64t,
                                    L_END);
  h.mkFunction("foo", befty1);

  DECLARE_EXPECTED_OUTPUT(exp, R"RAW_RESULT(
    )RAW_RESULT");

  bool isOK = h.expectBlock(exp);
  EXPECT_TRUE(isOK && "Block does not have expected contents");

  bool broken = h.finish(PreserveDebugInfo);
  EXPECT_FALSE(broken && "Module failed to verify.");
}

TEST_P(BackendFcnTests, MakeFunction) {
  LLVMContext C;
  auto cc = GetParam();
  std::unique_ptr<Backend> be(go_get_backend(C, cc));

  Btype *bi64t = be->integer_type(false, 64);
  Btype *bi32t = be->integer_type(false, 32);

  // func foo(i1, i2 int32) int64 { }
  BFunctionType *befty =
      mkFuncTyp(be.get(), L_PARM, bi32t, L_PARM, bi32t, L_RES, bi64t, L_END);

  const bool is_visible[2] = {true, false};
  const bool is_inlinable[2] = {true, false};
  const bool only_inline[2] = {true, false};
  bool split_stack[2] = {true, false};
  bool is_noret[2] = {true, false};
  Location loc;
  unsigned count = 0;
  for (auto vis : is_visible) {
    for (auto inl : is_inlinable) {
      for (auto only_inl : only_inline) {
        // Assume all inlinable functions are visible.
        if (only_inl && !vis)
          continue;
        for (auto nosplit : split_stack) {
          for (auto noret : is_noret) {
            unsigned fflags =
                (Backend::function_is_declaration |
                 (vis ? Backend::function_is_visible : 0) |
                 (inl ? Backend::function_is_inlinable : 0) |
                 (nosplit ? Backend::function_no_split_stack : 0) |
                 (noret ? Backend::function_does_not_return : 0) |
                 (only_inl ? Backend::function_only_inline : 0));

            std::stringstream ss;
            ss << "fcn" << count++;
            Bfunction *befcn =
                be->function(befty, "_foo", ss.str(), fflags, loc);
            llvm::Function *llfunc = befcn->function();
            ASSERT_TRUE(llfunc != NULL);
            EXPECT_EQ(llfunc->getName(), ss.str());
            EXPECT_FALSE(llfunc->isVarArg());
            EXPECT_EQ(llfunc->hasFnAttribute(Attribute::NoInline), !inl);
            EXPECT_EQ(llfunc->hasFnAttribute(Attribute::NoReturn), noret);
            EXPECT_EQ(llfunc->hasInternalLinkage(), !vis);
            EXPECT_EQ(llfunc->hasExternalLinkage(), vis && !only_inl);
            EXPECT_EQ(llfunc->hasAvailableExternallyLinkage(), vis && only_inl);
            EXPECT_EQ(befcn->splitStack() == Bfunction::YesSplit, !nosplit);
          }
        }
      }
    }
  }

  // Error function
  Bfunction *be_error_fcn = be->error_function();
  ASSERT_TRUE(be_error_fcn != NULL);

  // Try to create a function with an error type -- we should
  // get back error_function
  unsigned fflags = (Backend::function_is_declaration |
                     Backend::function_is_visible);
  Bfunction *mistake = be->function(be->error_type(), "bad", "bad",
                                    fflags, loc);
  EXPECT_EQ(mistake, be_error_fcn);
}

TEST_P(BackendFcnTests, BuiltinFunctionsMisc) {
  LLVMContext C;
  auto cc = GetParam();
  std::unique_ptr<Backend> be(go_get_backend(C, cc));

  std::unordered_set<Bfunction *> results;
  std::vector<std::string> tocheck = {
      "__sync_fetch_and_add_1",  "__sync_fetch_and_add_2",
      "__sync_fetch_and_add_4",  "__sync_fetch_and_add_8",
      "__builtin_trap",          "__builtin_expect",
      "__builtin_memcmp",        "__builtin_ctz",
      "__builtin_ctzll",         "__builtin_bswap32",
      "__builtin_bswap64",       "__builtin_return_address",
      "__builtin_frame_address", "__builtin_unreachable"
  };
  for (auto fname : tocheck) {
    Bfunction *bfcn = be->lookup_builtin(fname);
    ASSERT_TRUE(bfcn != NULL);
    EXPECT_TRUE(results.find(bfcn) == results.end());
    results.insert(bfcn);
  }
  EXPECT_TRUE(results.size() == tocheck.size());
}

TEST_P(BackendFcnTests, BuiltinFunctionsTrig) {
  LLVMContext C;
  auto cc = GetParam();
  std::unique_ptr<Backend> be(go_get_backend(C, cc));

  std::unordered_set<Bfunction *> results;
  std::vector<std::string> tocheck = {
      "acos",  "asin", "atan",  "atan2", "ceil",  "cos",   "exp",
      "expm1", "fabs", "floor", "fmod",  "log",   "log1p", "log10",
      "log2",  "sin",  "sqrt",  "tan",   "trunc", "ldexp",
  };
  for (auto fname : tocheck) {

    // function
    Bfunction *bfcn = be->lookup_builtin(fname);
    ASSERT_TRUE(bfcn != NULL);
    EXPECT_TRUE(results.find(bfcn) == results.end());
    results.insert(bfcn);

    // builtin variant
    char nbuf[128];
    sprintf(nbuf, "__builtin_%s", fname.c_str());
    Bfunction *bifcn = be->lookup_builtin(nbuf);
    EXPECT_TRUE(bifcn != NULL);
    EXPECT_TRUE(bifcn == bfcn);
  }
  EXPECT_TRUE(results.size() == tocheck.size());
}

TEST_P(BackendFcnTests, MakeBlocks) {
  LLVMContext C;
  auto cc = GetParam();
  std::unique_ptr<Backend> be(go_get_backend(C, cc));
  Bfunction *bfcn = mkFunci32o64(be.get(), "foo");
  const std::vector<Bvariable *> vars;
  Bblock *bb = be->block(bfcn, nullptr, vars, Location(), Location());
  ASSERT_TRUE(bb != nullptr);
}

TEST_P(BackendFcnTests, MakeFuncWithRecursiveTypeParam) {
  auto cc = GetParam();
  // Create empty function
  FcnTestHarness h(cc);
  Llvm_backend *be = h.be();
  Location loc;

  // type P *P
  Btype *cpht = be->placeholder_pointer_type("ph", loc, false);
  Btype *cpt = be->circular_pointer_type(cpht, false);
  be->set_placeholder_pointer_type(cpht, be->pointer_type(cpt));

  // struct A { f2 bool, fn *A }
  Btype *php = be->placeholder_pointer_type("ph", loc, false);
  std::vector<Backend::Btyped_identifier> fields = {
      Backend::Btyped_identifier("f1", be->bool_type(), Location()),
      Backend::Btyped_identifier("fn", php, Location())
  };
  Btype *bst = be->struct_type(fields);
  Btype *bpst = be->pointer_type(bst);
  be->set_placeholder_pointer_type(php, bpst);
  Btype *bi64t = be->integer_type(false, 64);
  BFunctionType *befty1 = mkFuncTyp(be,
                                    L_RCV, php,
                                    L_PARM, cpht,
                                    L_PARM, bpst,
                                    L_PARM, be->bool_type(),
                                    L_PARM, bst,
                                    L_RES, bi64t,
                                    L_END);
  h.mkFunction("foo", befty1);

  DECLARE_EXPECTED_OUTPUT(exp, R"RAW_RESULT(
    )RAW_RESULT");

  bool isOK = h.expectBlock(exp);
  EXPECT_TRUE(isOK && "Block does not have expected contents");

  bool broken = h.finish(PreserveDebugInfo);
  EXPECT_FALSE(broken && "Module failed to verify.");
}

TEST_P(BackendFcnTests, MakeMultipleDeclarations) {
  auto cc = GetParam();
  FcnTestHarness h(cc, "foo");
  Llvm_backend *be = h.be();
  Location loc;

  // If a function of a given name/type is declared more than once,
  // we expect to get back the original decl on the second time.
  // For definitions, a new function will be created each time (although
  // this could certainly be changed if needed);
  Btype *bi32t = be->integer_type(false, 32);
  Btype *bi64t = be->integer_type(false, 64);
  BFunctionType *befty1 = mkFuncTyp(be, L_RES, bi32t, L_PARM, bi64t, L_END);
  BFunctionType *befty2 = mkFuncTyp(be, L_RES, bi64t, L_PARM, bi64t, L_END);
  unsigned fflags =
      (Backend::function_is_declaration |  Backend::function_is_visible |
       Backend::function_is_inlinable);
  Bfunction *bf1 = be->function(befty1, "_foo", "bar", fflags, loc);
  Bfunction *bf2 = be->function(befty1, "_foo", "bar", fflags, loc);
  unsigned fflags2 = (Backend::function_is_visible |
                      Backend::function_is_inlinable);
  Bfunction *bf3 = be->function(befty1, "_foo", "bar", fflags2, loc);
  Bfunction *bf4 = be->function(befty2, "_foo", "bar", fflags, loc);
  EXPECT_EQ(bf1, bf2);
  EXPECT_NE(bf1, bf3);
  EXPECT_NE(bf2, bf4);

  bool broken = h.finish(PreserveDebugInfo);
  EXPECT_FALSE(broken && "Module failed to verify.");
}

TEST_P(BackendFcnTests, TestIntrinsicCall) {
  auto cc = GetParam();
  FcnTestHarness h(cc, "foo");
  Llvm_backend *be = h.be();
  Location loc;

  // var x uint64
  Btype *bu64t = be->integer_type(true, 64);
  Bvariable *x = h.mkLocal("x", bu64t);

  // __builtin_ctzll(x);
  Bfunction *bfcn = be->lookup_builtin("__builtin_ctzll");
  // Bexpression *fnexpr = be->function_code_expression(bfcn, loc);
  Bexpression *ve = be->var_expression(x, loc);
  Bexpression *call = h.mkCallExpr(be, bfcn, ve, nullptr);
  h.mkExprStmt(call);

  DECLARE_EXPECTED_OUTPUT(exp, R"RAW_RESULT(
    store i64 0, i64* %x
    %x.ld.0 = load i64, i64* %x
    %call.0 = call addrspace(0) i64 @llvm.cttz.i64(i64 %x.ld.0, i1 true)
  )RAW_RESULT");

  bool isOK = h.expectBlock(exp);
  EXPECT_TRUE(isOK && "Block does not have expected contents");

  bool broken = h.finish(StripDebugInfo);
  EXPECT_FALSE(broken && "Module failed to verify.");
}

TEST_P(BackendFcnTests, TestCallMemBuiltins) {
  auto cc = GetParam();
  FcnTestHarness h(cc, "foo");
  Llvm_backend *be = h.be();
  Location loc;

  // Test calls to memmove, memcpy, memcmp

  // var x, y uint64
  Btype *bu64t = be->integer_type(true, 64);
  Bvariable *x = h.mkLocal("x", bu64t);
  Bvariable *y = h.mkLocal("y", bu64t, mkUint64Const(be, 10101));

  // memcmp(&x,&y,sizeof(x))
  {
    Bfunction *bmemcmp = be->lookup_builtin("memcmp");
    Bexpression *vex = be->var_expression(x, loc);
    Bexpression *vey = be->var_expression(y, loc);
    Bexpression *call =
        h.mkCallExpr(be, bmemcmp,
                     be->address_expression(vex, loc),
                     be->address_expression(vey, loc),
                     mkUint64Const(be, be->type_size(bu64t)),
                     nullptr);
    h.mkExprStmt(call);
  }

  // memmove(&x,&y,sizeof(x))
  {
    Bfunction *bmemmove = be->lookup_builtin("memmove");
    Bexpression *vex = be->var_expression(x, loc);
    Bexpression *vey = be->var_expression(y, loc);
    Bexpression *call =
        h.mkCallExpr(be, bmemmove,
                     be->address_expression(vex, loc),
                     be->address_expression(vey, loc),
                     mkUint64Const(be, be->type_size(bu64t)),
                     nullptr);
    h.mkExprStmt(call);
  }

  // memcpy(&y,&x,sizeof(y))
  {
    Bfunction *bmemcpy = be->lookup_builtin("memcpy");
    Bexpression *vey = be->var_expression(y, loc);
    Bexpression *vex = be->var_expression(x, loc);
    Bexpression *call =
        h.mkCallExpr(be, bmemcpy, be->address_expression(vey, loc),
                     be->address_expression(vex, loc),
                     mkUint64Const(be, be->type_size(bu64t)), nullptr);
    h.mkExprStmt(call);
  }

  DECLARE_EXPECTED_OUTPUT(exp, R"RAW_RESULT(
    store i64 0, i64* %x
    store i64 10101, i64* %y
    %cast.0 = bitcast i64* %x to i8*
    %cast.1 = bitcast i64* %y to i8*
    %call.0 = call addrspace(0) i32 @memcmp(i8* %cast.0, i8* %cast.1, i64 8)
    %cast.2 = bitcast i64* %x to i8*
    %cast.3 = bitcast i64* %y to i8*
    call addrspace(0) void @llvm.memmove.p0i8.p0i8.i64(i8* %cast.2, i8* %cast.3, i64 8, i1 false)
    %cast.4 = bitcast i64* %y to i8*
    %cast.5 = bitcast i64* %x to i8*
    call addrspace(0) void @llvm.memcpy.p0i8.p0i8.i64(i8* %cast.4, i8* %cast.5, i64 8, i1 false)
  )RAW_RESULT");

  bool isOK = h.expectBlock(exp);
  EXPECT_TRUE(isOK && "Block does not have expected contents");

  bool broken = h.finish(StripDebugInfo);
  EXPECT_FALSE(broken && "Module failed to verify.");
}

TEST_P(BackendFcnTests, TestMultipleExternalFcnsWithSameName) {
  auto cc = GetParam();
  FcnTestHarness h(cc, "foo");
  Llvm_backend *be = h.be();
  Location loc;

  // Declare two external functions, each with the same name (syscall)
  // but with different types.

  // syscall(int64) int64
  Btype *bi64t = be->integer_type(false, 64);
  BFunctionType *btf1 = mkFuncTyp(be,
                                  L_PARM, bi64t,
                                  L_RES, bi64t,
                                  L_END);

  // syscall(int32) int32
  Btype *bi32t = be->integer_type(false, 32);
  BFunctionType *btf2 = mkFuncTyp(be,
                                  L_PARM, bi32t,
                                  L_RES, bi32t,
                                  L_END);

  // Now manufacture Bfunctions
  unsigned fflags = (Backend::function_is_visible |
                     Backend::function_is_declaration |
                     Backend::function_is_inlinable);
  Bfunction *bf1 = be->function(btf1, "syscall", "syscall", fflags, loc);
  Bfunction *bf2 = be->function(btf2, "syscall", "syscall", fflags, loc);

  // Create calls to the functions

  // x := syscall64(64)
  Bexpression *call64 = h.mkCallExpr(be, bf1, mkInt64Const(be, 64), nullptr);
  h.mkLocal("x", bi64t, call64);

  // y := syscall32(32)
  Bexpression *call32 = h.mkCallExpr(be, bf2, mkInt32Const(be, 32), nullptr);
  h.mkLocal("y", bi32t, call32);

  DECLARE_EXPECTED_OUTPUT(exp, R"RAW_RESULT(
    %call.0 = call addrspace(0) i64 @syscall(i8* nest undef, i64 64)
    store i64 %call.0, i64* %x
    %call.1 = call addrspace(0) i32 bitcast (i64 (i8*, i64)*
          @syscall to i32 (i8*, i32)*)(i8* nest undef, i32 32)
    store i32 %call.1, i32* %y
  )RAW_RESULT");

  bool isOK = h.expectBlock(exp);
  EXPECT_TRUE(isOK && "Block does not have expected contents");

  bool broken = h.finish(StripDebugInfo);
  EXPECT_FALSE(broken && "Module failed to verify.");
}

TEST_P(BackendFcnTests, TestDeclAndDefWithSameName) {
  auto cc = GetParam();
  FcnTestHarness h(cc, "foo");
  Llvm_backend *be = h.be();
  Location loc;

  // Make two functions, one declaration and one definition
  // with the same name.

  // first with same type.
  // bar() int32
  Btype *bi32t = be->integer_type(false, 32);
  BFunctionType *btf1 = mkFuncTyp(be,
                                  L_RES, bi32t,
                                  L_END);

  // then with different types.
  // baz() *struct{}
  Btype *bs0t = mkBackendStruct(be, nullptr);
  Btype *bps0t = be->pointer_type(bs0t);
  BFunctionType *btf2 = mkFuncTyp(be,
                                  L_RES, bps0t,
                                  L_END);

  // baz() *struct{ int32 x }
  Btype *bs1t = mkBackendStruct(be, bi32t, "x", nullptr);
  Btype *bps1t = be->pointer_type(bs1t);
  BFunctionType *btf3 = mkFuncTyp(be,
                                  L_RES, bps1t,
                                  L_END);

  // Now manufacture Bfunctions
  unsigned fflags1 = (Backend::function_is_visible |
                      Backend::function_is_inlinable);
  unsigned fflags2 = (Backend::function_is_visible |
                      Backend::function_is_declaration |
                      Backend::function_is_inlinable);

  // bar() declaration and definition
  Bfunction *bf1 = be->function(btf1, "bar", "bar", fflags2, loc);
  Bfunction *bf2 = be->function(btf1, "bar", "bar", fflags1, loc);

  // baz() declaration and definition
  Bfunction *bf3 = be->function(btf2, "baz", "baz", fflags2, loc);
  Bfunction *bf4 = be->function(btf3, "baz", "baz", fflags1, loc);

  // Create calls to the functions
  Bexpression *call1 = h.mkCallExpr(be, bf1, nullptr);
  h.mkLocal("a", bi32t, call1);

  Bexpression *call2 = h.mkCallExpr(be, bf2, nullptr);
  h.mkLocal("b", bi32t, call2);

  Bexpression *call3 = h.mkCallExpr(be, bf3, nullptr);
  h.mkLocal("x", bps0t, call3);

  Bexpression *call4 = h.mkCallExpr(be, bf4, nullptr);
  h.mkLocal("y", bps1t, call4);

  DECLARE_EXPECTED_OUTPUT(exp, R"RAW_RESULT(
    %call.0 = call addrspace(0) i32 @bar(i8* nest undef)
    store i32 %call.0, i32* %a
    %call.1 = call addrspace(0) i32 @bar(i8* nest undef)
    store i32 %call.1, i32* %b
    %call.2 = call addrspace(0) {}* bitcast ({ i32 }* (i8*)* @baz to {}* (i8*)*)(i8* nest undef)
    store {}* %call.2, {}** %x
    %call.3 = call addrspace(0) { i32 }* @baz(i8* nest undef)
    store { i32 }* %call.3, { i32 }** %y
  )RAW_RESULT");

  bool isOK = h.expectBlock(exp);
  EXPECT_TRUE(isOK && "Block does not have expected contents");

  bool broken = h.finish(StripDebugInfo);
  EXPECT_FALSE(broken && "Module failed to verify.");
}

} // namespace
