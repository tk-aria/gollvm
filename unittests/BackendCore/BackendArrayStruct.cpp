//===- llvm/tools/gollvm/unittests/BackendCore/BackendArrayStruct.cpp ---===//
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

//using namespace llvm;
using namespace goBackendUnitTests;

namespace {

class BackendArrayStructTests
    : public testing::TestWithParam<llvm::CallingConv::ID> {};

INSTANTIATE_TEST_CASE_P(
    UnitTest, BackendArrayStructTests,
    testing::Values(llvm::CallingConv::X86_64_SysV,
                    llvm::CallingConv::ARM_AAPCS),
    [](const testing::TestParamInfo<BackendArrayStructTests::ParamType> &info) {
      std::string name = goBackendUnitTests::ccName(info.param);
      return name;
    });

TEST_P(BackendArrayStructTests, TestStructFieldExprs) {
  auto cc = GetParam();
  FcnTestHarness h(cc, "foo");
  Llvm_backend *be = h.be();

  //
  // type X struct {
  //    f1 *bool
  //    f2 int32
  // }
  // var loc1 X
  //
  Location loc;
  Btype *bt = be->bool_type();
  Btype *pbt = be->pointer_type(bt);
  Btype *bi32t = be->integer_type(false, 32);
  Btype *s2t = mkBackendStruct(be, pbt, "f1", bi32t, "f2", nullptr);
  Bvariable *loc1 = h.mkLocal("loc1", s2t);

  // var loc2 *X = &loc1
  Btype *ps2t = be->pointer_type(s2t);
  Bexpression *bl1vex = be->var_expression(loc1, loc);
  Bexpression *adl1 = be->address_expression(bl1vex, loc);
  Bvariable *loc2 = h.mkLocal("loc2", ps2t, adl1);

  // var x int32
  // x = loc1.f2
  Bvariable *x = h.mkLocal("x", bi32t);
  Bexpression *vex = be->var_expression(x, loc);
  Bexpression *sex = be->var_expression(loc1, loc);
  Bexpression *fex = be->struct_field_expression(sex, 1, loc);
  h.mkAssign(vex, fex);

  // var b2 bool
  // loc1.f1 = &b2
  Bvariable *b2 = h.mkLocal("b2", bt);
  Bexpression *lvex = be->var_expression(loc1, loc);
  Bexpression *bfex = be->struct_field_expression(lvex, 0, loc);
  Bexpression *b2ex = be->var_expression(b2, loc);
  Bexpression *adb2 = be->address_expression(b2ex, loc);
  h.mkAssign(bfex, adb2);

  // loc2.f2 = 2 (equivalent to (*loc2).f2 = 2)
  Bexpression *lvexi = be->var_expression(loc2, loc);
  bool knValid = false;
  Bexpression *lindx = be->indirect_expression(s2t, lvexi, knValid, loc);
  Bexpression *bfex2 = be->struct_field_expression(lindx, 1, loc);
  Bexpression *bc2 = mkInt32Const(be, 2);
  h.mkAssign(bfex2, bc2);

  const char *exp = R"RAW_RESULT(
    %cast.0 = bitcast { i8*, i32 }* %loc1 to i8*
    %cast.1 = bitcast { i8*, i32 }* @const.0 to i8*
    call addrspace(0) void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %cast.0, i8* align 8 %cast.1, i64 16, i1 false)
    store { i8*, i32 }* %loc1, { i8*, i32 }** %loc2
    store i32 0, i32* %x
    %field.0 = getelementptr inbounds { i8*, i32 }, { i8*, i32 }* %loc1, i32 0, i32 1
    %loc1.field.ld.0 = load i32, i32* %field.0
    store i32 %loc1.field.ld.0, i32* %x
    store i8 0, i8* %b2
    %field.1 = getelementptr inbounds { i8*, i32 }, { i8*, i32 }* %loc1, i32 0, i32 0
    store i8* %b2, i8** %field.1
    %loc2.ld.0 = load { i8*, i32 }*, { i8*, i32 }** %loc2
    %field.2 = getelementptr inbounds { i8*, i32 }, { i8*, i32 }* %loc2.ld.0, i32 0, i32 1
    store i32 2, i32* %field.2
  )RAW_RESULT";

  bool isOK = h.expectBlock(exp);
  EXPECT_TRUE(isOK && "Block does not have expected contents");

  bool broken = h.finish(PreserveDebugInfo);
  EXPECT_FALSE(broken && "Module failed to verify.");
}

TEST_P(BackendArrayStructTests, TestStructFieldExprs2) {
  auto cc = GetParam();
  // Testing struct field expression for composites.
  FcnTestHarness h(cc);
  Llvm_backend *be = h.be();
  BFunctionType *befty = mkFuncTyp(be, L_END);
  Bfunction *func = h.mkFunction("foo", befty);

  // type X struct {
  //    f1 *bool
  //    f2 int32
  // }
  Location loc;
  Btype *bt = be->bool_type();
  Btype *pbt = be->pointer_type(bt);
  Btype *bi32t = be->integer_type(false, 32);
  Btype *s2t = mkBackendStruct(be, pbt, "f1", bi32t, "f2", nullptr);

  // Taking a field of non-constant composite.
  // var x, y int32
  // x = X{nil, y}.f2
  Bvariable *x = h.mkLocal("x", bi32t);
  Bvariable *y = h.mkLocal("y", bi32t);
  std::vector<Bexpression *> vals1;
  vals1.push_back(be->zero_expression(pbt));
  vals1.push_back(be->var_expression(y, loc));
  Bexpression *vex1 = be->var_expression(x, loc);
  Bexpression *sex1 = be->constructor_expression(s2t, vals1, loc);
  Bexpression *fex1 = be->struct_field_expression(sex1, 1, loc);
  h.mkAssign(vex1, fex1);

  // Taking a field of constant composite.
  // var z int32
  // z = X{nil, 42}.f2
  Bvariable *z = h.mkLocal("z", bi32t);
  std::vector<Bexpression *> vals2;
  vals2.push_back(be->zero_expression(pbt));
  vals2.push_back(mkInt32Const(be, int32_t(42)));
  Bexpression *vex2 = be->var_expression(z, loc);
  Bexpression *sex2 = be->constructor_expression(s2t, vals2, loc);
  Bexpression *fex2 = be->struct_field_expression(sex2, 1, loc);
  h.mkAssign(vex2, fex2);

  const char *exp = R"RAW_RESULT(
    define void @foo(i8* nest %nest.0) #0 {
    entry:
      %tmp.0 = alloca { i8*, i32 }
      %x = alloca i32
      %y = alloca i32
      %z = alloca i32
      store i32 0, i32* %x
      store i32 0, i32* %y
      %y.ld.0 = load i32, i32* %y
      %field.0 = getelementptr inbounds { i8*, i32 }, { i8*, i32 }* %tmp.0, i32 0, i32 0
      store i8* null, i8** %field.0
      %field.1 = getelementptr inbounds { i8*, i32 }, { i8*, i32 }* %tmp.0, i32 0, i32 1
      store i32 %y.ld.0, i32* %field.1
      %field.2 = getelementptr inbounds { i8*, i32 }, { i8*, i32 }* %tmp.0, i32 0, i32 1
      %.field.ld.0 = load i32, i32* %field.2
      store i32 %.field.ld.0, i32* %x
      store i32 0, i32* %z
      store i32 42, i32* %z
      ret void
    }
  )RAW_RESULT";

  bool broken = h.finish(StripDebugInfo);
  EXPECT_FALSE(broken && "Module failed to verify.");

  bool isOK = h.expectValue(func->function(), exp);
  EXPECT_TRUE(isOK && "Block does not have expected contents");
}

TEST_P(BackendArrayStructTests, TestArrayIndexingExprs) {
  auto cc = GetParam();
  // Testing array indexing expression for composites.
  FcnTestHarness h(cc);
  Llvm_backend *be = h.be();
  BFunctionType *befty = mkFuncTyp(be, L_END);
  Bfunction *func = h.mkFunction("foo", befty);

  // type T [4]int64
  Location loc;
  Bexpression *val4 = mkInt64Const(be, int64_t(4));
  Btype *bi64t = be->integer_type(false, 64);
  Btype *at4 = be->array_type(bi64t, val4);

  // Taking an element of non-constant composite.
  // var x, y int64
  // x = T{y, 3, 2, 1}[1]
  Bvariable *x = h.mkLocal("x", bi64t);
  Bvariable *y = h.mkLocal("y", bi64t);
  std::vector<unsigned long> indexes = { 0, 1, 2, 3 };
  std::vector<Bexpression *> vals1;
  vals1.push_back(be->var_expression(y, loc));
  vals1.push_back(mkInt64Const(be, 3));
  vals1.push_back(mkInt64Const(be, 2));
  vals1.push_back(mkInt64Const(be, 1));
  Bexpression *aex1 = be->array_constructor_expression(at4, indexes, vals1, loc);
  Bexpression *vex1 = be->var_expression(x, loc);
  Bexpression *bi32one = mkInt32Const(be, 1);
  Bexpression *eex1 = be->array_index_expression(aex1, bi32one, loc);
  h.mkAssign(vex1, eex1);

  // Taking an element of constant composite.
  // var z int64
  // z = T{4, 3, 2, 1}[1]
  Bvariable *z = h.mkLocal("z", bi64t);
  std::vector<Bexpression *> vals2;
  for (int64_t v : {4, 3, 2, 1})
    vals2.push_back(mkInt64Const(be, v));
  Bexpression *aex2 = be->array_constructor_expression(at4, indexes, vals2, loc);
  Bexpression *vex2 = be->var_expression(z, loc);
  Bexpression *eex2 = be->array_index_expression(aex2, bi32one, loc);
  h.mkAssign(vex2, eex2);

  // Taking an element of constant composite with non-constant index.
  // var w int64
  // w = T{4, 3, 2, 1}[x]
  Bvariable *w = h.mkLocal("w", bi64t);
  std::vector<Bexpression *> vals3;
  for (int64_t v : {4, 3, 2, 1})
    vals3.push_back(mkInt64Const(be, v));
  Bexpression *aex3 = be->array_constructor_expression(at4, indexes, vals3, loc);
  Bexpression *vex3 = be->var_expression(w, loc);
  Bexpression *iex3 = be->var_expression(x, loc);
  Bexpression *eex3 = be->array_index_expression(aex3, iex3, loc);
  h.mkAssign(vex3, eex3);

  const char *exp = R"RAW_RESULT(
    define void @foo(i8* nest %nest.0) #0 {
    entry:
      %tmp.0 = alloca [4 x i64]
      %x = alloca i64
      %y = alloca i64
      %z = alloca i64
      %w = alloca i64
      store i64 0, i64* %x
      store i64 0, i64* %y
      %y.ld.0 = load i64, i64* %y
      %index.0 = getelementptr [4 x i64], [4 x i64]* %tmp.0, i32 0, i32 0
      store i64 %y.ld.0, i64* %index.0
      %index.1 = getelementptr [4 x i64], [4 x i64]* %tmp.0, i32 0, i32 1
      store i64 3, i64* %index.1
      %index.2 = getelementptr [4 x i64], [4 x i64]* %tmp.0, i32 0, i32 2
      store i64 2, i64* %index.2
      %index.3 = getelementptr [4 x i64], [4 x i64]* %tmp.0, i32 0, i32 3
      store i64 1, i64* %index.3
      %index.4 = getelementptr [4 x i64], [4 x i64]* %tmp.0, i32 0, i32 1
      %.index.ld.0 = load i64, i64* %index.4
      store i64 %.index.ld.0, i64* %x
      store i64 0, i64* %z
      store i64 3, i64* %z
      store i64 0, i64* %w
      %x.ld.0 = load i64, i64* %x
      %index.5 = getelementptr [4 x i64], [4 x i64]* @const.0, i32 0, i64 %x.ld.0
      %.index.ld.1 = load i64, i64* %index.5
      store i64 %.index.ld.1, i64* %w
      ret void
    }
  )RAW_RESULT";

  bool broken = h.finish(StripDebugInfo);
  EXPECT_FALSE(broken && "Module failed to verify.");

  bool isOK = h.expectValue(func->function(), exp);
  EXPECT_TRUE(isOK && "Block does not have expected contents");
}

TEST_P(BackendArrayStructTests, CreateArrayConstructionExprs) {
  auto cc = GetParam();
  FcnTestHarness h(cc, "foo");
  Llvm_backend *be = h.be();

  // var aa [4]int64 = { 4, 3, 2, 1 }
  Location loc;
  Bexpression *val4 = mkInt64Const(be, int64_t(4));
  Btype *bi64t = be->integer_type(false, 64);
  Btype *at4 = be->array_type(bi64t, val4);
  std::vector<unsigned long> indexes1 = { 0, 1, 2, 3 };
  std::vector<Bexpression *> vals1;
  for (int64_t v : {4, 3, 2, 1})
    vals1.push_back(mkInt64Const(be, v));
  Bexpression *arcon1 =
      be->array_constructor_expression(at4, indexes1, vals1, loc);
  h.mkLocal("aa", at4, arcon1);

  // var ab [4]int64 = { 2:3 }
  std::vector<unsigned long> indexes2 = { 2 };
  std::vector<Bexpression *> vals2;
  vals2.push_back(mkInt64Const(be, int64_t(3)));
  Bexpression *arcon2 =
    be->array_constructor_expression(at4, indexes2, vals2, loc);
  h.mkLocal("ab", at4, arcon2);

  // var ac [4]int64 = { 1:z }
  Bvariable *z = h.mkLocal("z", bi64t);
  std::vector<unsigned long> indexes3 = { 1 };
  std::vector<Bexpression *> vals3;
  vals3.push_back(be->var_expression(z, loc));
  Bexpression *arcon3 =
      be->array_constructor_expression(at4, indexes3, vals3, loc);
  h.mkLocal("ac", at4, arcon3);

  const char *exp = R"RAW_RESULT(
    %cast.0 = bitcast [4 x i64]* %aa to i8*
    %cast.1 = bitcast [4 x i64]* @const.0 to i8*
    call addrspace(0) void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %cast.0, i8* align 8 %cast.1, i64 32, i1 false)
    %cast.2 = bitcast [4 x i64]* %ab to i8*
    %cast.3 = bitcast [4 x i64]* @const.1 to i8*
    call addrspace(0) void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %cast.2, i8* align 8 %cast.3, i64 32, i1 false)
    store i64 0, i64* %z
    %z.ld.0 = load i64, i64* %z
    %index.0 = getelementptr [4 x i64], [4 x i64]* %ac, i32 0, i32 0
    store i64 0, i64* %index.0
    %index.1 = getelementptr [4 x i64], [4 x i64]* %ac, i32 0, i32 1
    store i64 %z.ld.0, i64* %index.1
    %index.2 = getelementptr [4 x i64], [4 x i64]* %ac, i32 0, i32 2
    store i64 0, i64* %index.2
    %index.3 = getelementptr [4 x i64], [4 x i64]* %ac, i32 0, i32 3
    store i64 0, i64* %index.3
  )RAW_RESULT";

  bool isOK = h.expectBlock(exp);
  EXPECT_TRUE(isOK && "Block does not have expected contents");

  bool broken = h.finish(PreserveDebugInfo);
  EXPECT_FALSE(broken && "Module failed to verify.");
}

TEST_P(BackendArrayStructTests, CreateStructConstructionExprs) {
  auto cc = GetParam();
  FcnTestHarness h(cc, "foo");
  Llvm_backend *be = h.be();
  Bfunction *func = h.func();
  Location loc;

  // type X struct {
  //    f1 *int32
  //    f2 int32
  // }
  // func foo(param1, param2 int32) int64 {
  // var loc1 X = { nil, 101 }
  // var loc2 X = { &param1, loc1.f2 }

  // var loc1 X = { nil, 101 }
  Btype *bi32t = be->integer_type(false, 32);
  Btype *pbi32t = be->pointer_type(bi32t);
  Btype *s2t = mkBackendStruct(be, pbi32t, "f1", bi32t, "f2", nullptr);
  std::vector<Bexpression *> vals1;
  vals1.push_back(be->zero_expression(pbi32t));
  vals1.push_back(mkInt32Const(be, int32_t(101)));
  Bexpression *scon1 = be->constructor_expression(s2t, vals1, loc);
  Bvariable *loc1 = h.mkLocal("loc1", s2t, scon1);

  // var loc2 X = { &param1, loc1.f2 }
  Bvariable *p1 = func->getNthParamVar(0);
  Bexpression *ve1 = be->var_expression(p1, loc);
  Bexpression *adp = be->address_expression(ve1, loc);
  Bexpression *ve2 = be->var_expression(loc1, loc);
  Bexpression *fex = be->struct_field_expression(ve2, 1, loc);
  std::vector<Bexpression *> vals2;
  vals2.push_back(adp);
  vals2.push_back(fex);
  Bexpression *scon2 = be->constructor_expression(s2t, vals2, loc);
  h.mkLocal("loc2", s2t, scon2);

  const char *exp = R"RAW_RESULT(
    %cast.0 = bitcast { i32*, i32 }* %loc1 to i8*
    %cast.1 = bitcast { i32*, i32 }* @const.0 to i8*
    call addrspace(0) void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %cast.0, i8* align 8 %cast.1, i64 16, i1 false)
    %field.0 = getelementptr inbounds { i32*, i32 }, { i32*, i32 }* %loc1, i32 0, i32 1
    %loc1.field.ld.0 = load i32, i32* %field.0
    %field.1 = getelementptr inbounds { i32*, i32 }, { i32*, i32 }* %loc2, i32 0, i32 0
    store i32* %param1.addr, i32** %field.1
    %field.2 = getelementptr inbounds { i32*, i32 }, { i32*, i32 }* %loc2, i32 0, i32 1
    store i32 %loc1.field.ld.0, i32* %field.2
  )RAW_RESULT";

  bool isOK = h.expectBlock(exp);
  EXPECT_TRUE(isOK && "Block does not have expected contents");

  bool broken = h.finish(PreserveDebugInfo);
  EXPECT_FALSE(broken && "Module failed to verify.");
}

TEST_P(BackendArrayStructTests, CreateNestedStructConstructionExprs) {
  auto cc = GetParam();
  FcnTestHarness h(cc, "foo");
  Llvm_backend *be = h.be();
  Bfunction *func = h.func();
  Location loc;

  // type X struct {
  //    f1 *int32
  //    f2 int32
  // }
  // type Y struct {
  //    f1 X
  //    f2 float32
  // }
  Btype *bi32t = be->integer_type(false, 32);
  Btype *bf32t = be->float_type(32);
  Btype *pbi32t = be->pointer_type(bi32t);
  Btype *sxt = mkBackendStruct(be, pbi32t, "f1", bi32t, "f2", nullptr);
  Btype *syt = mkBackendStruct(be, sxt, "f1", bf32t, "f2", nullptr);

  // var l1 Y = Y{ X{nil, 3}, 3.0}
  std::vector<Bexpression *> vals1;
  Bvariable *p1 = func->getNthParamVar(0);
  Bexpression *ve1 = be->var_expression(p1, loc);
  Bexpression *adp = be->address_expression(ve1, loc);
  vals1.push_back(adp);
  vals1.push_back(mkInt32Const(be, int32_t(3)));
  Bexpression *scon1 = be->constructor_expression(sxt, vals1, loc);
  std::vector<Bexpression *> vals2;
  vals2.push_back(scon1);
  Bexpression *ci3 = mkInt32Const(be, int32_t(3));
  vals2.push_back(be->convert_expression(bf32t, ci3, loc));
  Bexpression *scon2 = be->constructor_expression(syt, vals2, loc);
  Bvariable *loc1 = h.mkLocal("loc1", syt);
  Bexpression *vex = be->var_expression(loc1, loc);
  h.mkAssign(vex, scon2);

  const char *exp = R"RAW_RESULT(
    %cast.0 = bitcast { { i32*, i32 }, float }* %loc1 to i8*
    %cast.1 = bitcast { { i32*, i32 }, float }* @const.0 to i8*
    call addrspace(0) void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %cast.0, i8* align 8 %cast.1, i64 24, i1 false)
    %field.0 = getelementptr inbounds { i32*, i32 }, { i32*, i32 }* %tmp.0, i32 0, i32 0
    store i32* %param1.addr, i32** %field.0
    %field.1 = getelementptr inbounds { i32*, i32 }, { i32*, i32 }* %tmp.0, i32 0, i32 1
    store i32 3, i32* %field.1
    %field.2 = getelementptr inbounds { { i32*, i32 }, float }, { { i32*, i32 }, float }* %loc1, i32 0, i32 0
    %cast.2 = bitcast { i32*, i32 }* %field.2 to i8*
    %cast.3 = bitcast { i32*, i32 }* %tmp.0 to i8*
    call addrspace(0) void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %cast.2, i8* align 8 %cast.3, i64 16, i1 false)
    %field.3 = getelementptr inbounds { { i32*, i32 }, float }, { { i32*, i32 }, float }* %loc1, i32 0, i32 1
    store float 3.000000e+00, float* %field.3
  )RAW_RESULT";

  bool isOK = h.expectBlock(exp);
  EXPECT_TRUE(isOK && "Block does not have expected contents");

  bool broken = h.finish(PreserveDebugInfo);
  EXPECT_FALSE(broken && "Module failed to verify.");
}

TEST_P(BackendArrayStructTests, CreateStructConstructionExprs2) {
  auto cc = GetParam();
  FcnTestHarness h(cc);
  Llvm_backend *be = h.be();

  Btype *bi32t = be->integer_type(false, 32);
  Btype *pbi32t = be->pointer_type(bi32t);
  Btype *s2t = mkBackendStruct(be, pbi32t, "f1", bi32t, "f2", nullptr);
  Btype *ps2t = be->pointer_type(s2t);
  BFunctionType *befty1 = mkFuncTyp(be,
                                    L_PARM, ps2t,
                                    L_PARM, pbi32t,
                                    L_END);
  Bfunction *func = h.mkFunction("blah", befty1);
  Location loc;

  // *p0 = { p1, 101 }
  Bvariable *p0 = func->getNthParamVar(0);
  Bvariable *p1 = func->getNthParamVar(1);
  Bexpression *ve = be->var_expression(p0, loc);
  Bexpression *dex = be->indirect_expression(s2t, ve, false, loc);
  std::vector<Bexpression *> vals;
  vals.push_back(be->var_expression(p1, loc));
  vals.push_back(mkInt32Const(be, int32_t(101)));
  Bexpression *scon = be->constructor_expression(s2t, vals, loc);
  h.mkAssign(dex, scon);

  const char *exp = R"RAW_RESULT(
    %p0.ld.0 = load { i32*, i32 }*, { i32*, i32 }** %p0.addr
    %p1.ld.0 = load i32*, i32** %p1.addr
    %field.0 = getelementptr inbounds { i32*, i32 }, { i32*, i32 }* %p0.ld.0, i32 0, i32 0
    store i32* %p1.ld.0, i32** %field.0
    %field.1 = getelementptr inbounds { i32*, i32 }, { i32*, i32 }* %p0.ld.0, i32 0, i32 1
    store i32 101, i32* %field.1
  )RAW_RESULT";

  bool isOK = h.expectBlock(exp);
  EXPECT_TRUE(isOK && "Block does not have expected contents");

  bool broken = h.finish(PreserveDebugInfo);
  EXPECT_FALSE(broken && "Module failed to verify.");
}

TEST_P(BackendArrayStructTests, CreateStructConstructionExprs3) {
  auto cc = GetParam();
  // Test struct construction involving global variables.
  FcnTestHarness h(cc, "foo");
  Llvm_backend *be = h.be();
  Location loc;

  // type T struct {
  //    f1 int32
  // }
  Btype *bi32t = be->integer_type(false, 32);
  Btype *s1t = mkBackendStruct(be, bi32t, "f1", nullptr);

  // Construct a struct with a global var field
  // var x int32  // global
  // var t = T{x} // global
  Bvariable *x = be->global_variable("x", "x", bi32t, false, /* is_external */
                                     false, /* is_hidden */
                                     false, /* unique_section */
                                     loc);
  Bexpression *xvex = be->var_expression(x, loc);
  std::vector<Bexpression *> vals1 = {xvex};
  Bexpression *scon1 = be->constructor_expression(s1t, vals1, loc);
  Bvariable *t = be->global_variable("t", "t", s1t, false, /* is_external */
                                     false, /* is_hidden */
                                     false, /* unique_section */
                                     loc);
  Bexpression *tvex = be->var_expression(t, loc);
  h.mkAssign(tvex, scon1);

  // Construct a struct with a field from a field of global var
  // var t2 = T{t.x}
  Bexpression *tvex2 = be->var_expression(t, loc);
  Bexpression *fex = be->struct_field_expression(tvex2, 0, loc);
  std::vector<Bexpression *> vals2 = {fex};
  Bexpression *scon2 = be->constructor_expression(s1t, vals2, loc);
  h.mkLocal("t2", s1t, scon2);

  const char *exp = R"RAW_RESULT(
    %x.ld.0 = load i32, i32* @x
    store i32 %x.ld.0, i32* getelementptr inbounds ({ i32 }, { i32 }* @t, i32 0, i32 0)
    %t.field.ld.0 = load i32, i32* getelementptr inbounds ({ i32 }, { i32 }* @t, i32 0, i32 0)
    %field.2 = getelementptr inbounds { i32 }, { i32 }* %t2, i32 0, i32 0
    store i32 %t.field.ld.0, i32* %field.2
  )RAW_RESULT";

  bool isOK = h.expectBlock(exp);
  EXPECT_TRUE(isOK && "Block does not have expected contents");

  bool broken = h.finish(PreserveDebugInfo);
  EXPECT_FALSE(broken && "Module failed to verify.");
}

TEST_P(BackendArrayStructTests, CreateArrayIndexingExprs) {
  auto cc = GetParam();
  FcnTestHarness h(cc, "foo");
  Llvm_backend *be = h.be();

  // var aa [4]int64 = { 4, 3, 2, 1 }
  Location loc;
  Bexpression *val4 = mkInt64Const(be, int64_t(4));
  Btype *bi64t = be->integer_type(false, 64);
  Btype *at4 = be->array_type(bi64t, val4);
  std::vector<unsigned long> indexes1 = { 0, 1, 2, 3 };
  std::vector<Bexpression *> vals1;
  for (int64_t v : {4, 3, 2, 1})
    vals1.push_back(mkInt64Const(be, v));
  Bexpression *arcon1 =
    be->array_constructor_expression(at4, indexes1, vals1, loc);
  Bvariable *aa = h.mkLocal("aa", at4, arcon1);

  // aa[1]
  Bexpression *bi32one = mkInt32Const(be, 1);
  Bexpression *vea1 = be->var_expression(aa, loc);
  Bexpression *aa1 = be->array_index_expression(vea1, bi32one, loc);

  // aa[3]
  Bexpression *bi64three = mkInt64Const(be, 3);
  Bexpression *vea2 = be->var_expression(aa, loc);
  Bexpression *aa2 = be->array_index_expression(vea2, bi64three, loc);

  // aa[aa[3]]
  Bexpression *vea3 = be->var_expression(aa, loc);
  Bexpression *aa3 = be->array_index_expression(vea3, aa2, loc);

  // aa[aa[1]]
  Bexpression *vea4 = be->var_expression(aa, loc);
  Bexpression *aa4 = be->array_index_expression(vea4, aa1, loc);

  // aa[aa[1]] = aa[aa[3]]
  h.mkAssign(aa4, aa3);

  const char *exp = R"RAW_RESULT(
    %cast.0 = bitcast [4 x i64]* %aa to i8*
    %cast.1 = bitcast [4 x i64]* @const.0 to i8*
    call addrspace(0) void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %cast.0, i8* align 8 %cast.1, i64 32, i1 false)
    %index.0 = getelementptr [4 x i64], [4 x i64]* %aa, i32 0, i32 1
    %aa.index.ld.0 = load i64, i64* %index.0
    %index.1 = getelementptr [4 x i64], [4 x i64]* %aa, i32 0, i64 %aa.index.ld.0
    %index.2 = getelementptr [4 x i64], [4 x i64]* %aa, i32 0, i64 3
    %aa.index.ld.1 = load i64, i64* %index.2
    %index.3 = getelementptr [4 x i64], [4 x i64]* %aa, i32 0, i64 %aa.index.ld.1
    %aa.index.ld.2 = load i64, i64* %index.3
    store i64 %aa.index.ld.2, i64* %index.1
  )RAW_RESULT";

  bool isOK = h.expectBlock(exp);
  EXPECT_TRUE(isOK && "Block does not have expected contents");

  bool broken = h.finish(PreserveDebugInfo);
  EXPECT_FALSE(broken && "Module failed to verify.");
}

TEST_P(BackendArrayStructTests, CreateComplexIndexingAndFieldExprs) {
  auto cc = GetParam();
  FcnTestHarness h(cc, "foo");

  // Create type that incorporates structures, arrays, and pointers:
  //
  //   type sA struct {
  //      x, y int64
  //   }
  //   type asA [4]*sA
  //   type sB struct {
  //      y  bool
  //      ar asA
  //      n  bool
  //   }
  //   type psB *sB
  //   type t [10]psB
  //
  Llvm_backend *be = h.be();
  Btype *bi64t = be->integer_type(false, 64);
  Btype *sA = mkBackendStruct(be, bi64t, "x", bi64t, "y", nullptr);
  Btype *psA = be->pointer_type(sA);
  Bexpression *val4 = mkInt64Const(be, int64_t(4));
  Btype *asA = be->array_type(psA, val4);
  Btype *bt = be->bool_type();
  Btype *sB = mkBackendStruct(be, bt, "y", asA, "ar", bt, "n", nullptr);
  Btype *psB = be->pointer_type(sB);
  Bexpression *val10 = mkInt64Const(be, int64_t(10));
  Btype *t = be->array_type(psB, val10);
  Location loc;

  // var t1 t
  Bvariable *t1 = h.mkLocal("t1", t);

  // t1[7].ar[3].x = 5
  {
    Bexpression *vt = be->var_expression(t1, loc);
    Bexpression *bi32sev = mkInt32Const(be, 7);
    Bexpression *ti7 = be->array_index_expression(vt, bi32sev, loc);
    bool knValid = true;
    Bexpression *iti7 = be->indirect_expression(sB, ti7, knValid, loc);
    Bexpression *far = be->struct_field_expression(iti7, 1, loc);
    Bexpression *bi32three = mkInt32Const(be, 3);
    Bexpression *ar3 = be->array_index_expression(far, bi32three, loc);
    Bexpression *iar3 = be->indirect_expression(sA, ar3, knValid, loc);
    Bexpression *fx = be->struct_field_expression(iar3, 0, loc);
    Bexpression *bi64five = mkInt64Const(be, 5);
    h.mkAssign(fx, bi64five);

    const char *exp = R"RAW_RESULT(
      %cast.0 = bitcast [10 x { i8, [4 x { i64, i64 }*], i8 }*]* %t1 to i8*
      %cast.1 = bitcast [10 x { i8, [4 x { i64, i64 }*], i8 }*]* @const.0 to i8*
      call addrspace(0) void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %cast.0, i8* align 8 %cast.1, i64 80, i1 false)
      %index.0 = getelementptr [10 x { i8, [4 x { i64, i64 }*], i8 }*], [10 x { i8, [4 x { i64, i64 }*], i8 }*]* %t1, i32 0, i32 7
      %t1.index.ld.0 = load { i8, [4 x { i64, i64 }*], i8 }*, { i8, [4 x { i64, i64 }*], i8 }** %index.0
      %field.0 = getelementptr inbounds { i8, [4 x { i64, i64 }*], i8 }, { i8, [4 x { i64, i64 }*], i8 }* %t1.index.ld.0, i32 0, i32 1
      %index.1 = getelementptr [4 x { i64, i64 }*], [4 x { i64, i64 }*]* %field.0, i32 0, i32 3
      %.field.index.ld.0 = load { i64, i64 }*, { i64, i64 }** %index.1
      %field.1 = getelementptr inbounds { i64, i64 }, { i64, i64 }* %.field.index.ld.0, i32 0, i32 0
      store i64 5, i64* %field.1
    )RAW_RESULT";

    bool isOK = h.expectBlock(exp);
    EXPECT_TRUE(isOK && "Block does not have expected contents");
  }

  h.newBlock();

  // q := t1[0].ar[0].y
  {
    Bexpression *vt = be->var_expression(t1, loc);
    Bexpression *bi32zero = mkInt32Const(be, 0);
    Bexpression *ti0 = be->array_index_expression(vt, bi32zero, loc);
    bool knValid = true;
    Bexpression *iti0 = be->indirect_expression(sB, ti0, knValid, loc);
    Bexpression *far = be->struct_field_expression(iti0, 1, loc);
    Bexpression *ar3 = be->array_index_expression(far, bi32zero, loc);
    Bexpression *iar3 = be->indirect_expression(sA, ar3, knValid, loc);
    Bexpression *fx = be->struct_field_expression(iar3, 1, loc);
    h.mkLocal("q", bi64t, fx);

    const char *exp = R"RAW_RESULT(
      %index.2 = getelementptr [10 x { i8, [4 x { i64, i64 }*], i8 }*], [10 x { i8, [4 x { i64, i64 }*], i8 }*]* %t1, i32 0, i32 0
      %t1.index.ld.1 = load { i8, [4 x { i64, i64 }*], i8 }*, { i8, [4 x { i64, i64 }*], i8 }** %index.2
      %field.2 = getelementptr inbounds { i8, [4 x { i64, i64 }*], i8 }, { i8, [4 x { i64, i64 }*], i8 }* %t1.index.ld.1, i32 0, i32 1
      %index.3 = getelementptr [4 x { i64, i64 }*], [4 x { i64, i64 }*]* %field.2, i32 0, i32 0
      %.field.index.ld.1 = load { i64, i64 }*, { i64, i64 }** %index.3
      %field.3 = getelementptr inbounds { i64, i64 }, { i64, i64 }* %.field.index.ld.1, i32 0, i32 1
      %.field.ld.0 = load i64, i64* %field.3
      store i64 %.field.ld.0, i64* %q
    )RAW_RESULT";

    bool isOK = h.expectBlock(exp);
    EXPECT_TRUE(isOK && "Block does not have expected contents");
  }

  bool broken = h.finish(PreserveDebugInfo);
  EXPECT_FALSE(broken && "Module failed to verify.");
}

TEST_P(BackendArrayStructTests, TestStructAssignment) {
  auto cc = GetParam();
  FcnTestHarness h(cc, "foo");
  Llvm_backend *be = h.be();

  // type T1 struct { f1 bool }
  // type T2 struct { f1, f2, f3, f4, f5, f6 int64 }
  Location loc;
  Btype *bt = be->bool_type();
  Btype *pbt = be->pointer_type(bt);
  Btype *bi64t = be->integer_type(false, 64);
  Btype *s1t = mkBackendStruct(be, pbt, "f1", nullptr);
  Btype *s2t = mkBackendStruct(be, bi64t, "f1", bi64t, "f2", bi64t, "f3",
                               bi64t, "f4", bi64t, "f5", bi64t, "f6", nullptr);

  // var x1, y1 T1
  // var x2, y2 T2
  Bvariable *x1 = h.mkLocal("x1", s1t);
  Bvariable *y1 = h.mkLocal("y1", s1t);
  Bvariable *x2 = h.mkLocal("x2", s2t);
  Bvariable *y2 = h.mkLocal("y2", s2t);

  // x1 = y1
  // x2 = y2
  Bexpression *ve1 = be->var_expression(x1, loc);
  Bexpression *ve2 = be->var_expression(y1, loc);
  h.mkAssign(ve1, ve2);
  Bexpression *ve3 = be->var_expression(x2, loc);
  Bexpression *ve4 = be->var_expression(y2, loc);
  h.mkAssign(ve3, ve4);

  const char *exp = R"RAW_RESULT(
    %cast.0 = bitcast { i8* }* %x1 to i8*
    %cast.1 = bitcast { i8* }* @const.0 to i8*
    call addrspace(0) void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %cast.0, i8* align 8 %cast.1, i64 8, i1 false)
    %cast.2 = bitcast { i8* }* %y1 to i8*
    %cast.3 = bitcast { i8* }* @const.0 to i8*
    call addrspace(0) void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %cast.2, i8* align 8 %cast.3, i64 8, i1 false)
    %cast.4 = bitcast { i64, i64, i64, i64, i64, i64 }* %x2 to i8*
    %cast.5 = bitcast { i64, i64, i64, i64, i64, i64 }* @const.1 to i8*
    call addrspace(0) void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %cast.4, i8* align 8 %cast.5, i64 48, i1 false)
    %cast.6 = bitcast { i64, i64, i64, i64, i64, i64 }* %y2 to i8*
    %cast.7 = bitcast { i64, i64, i64, i64, i64, i64 }* @const.1 to i8*
    call addrspace(0) void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %cast.6, i8* align 8 %cast.7, i64 48, i1 false)
    %cast.8 = bitcast { i8* }* %x1 to i8*
    %cast.9 = bitcast { i8* }* %y1 to i8*
    call addrspace(0) void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %cast.8, i8* align 8 %cast.9, i64 8, i1 false)
    %cast.10 = bitcast { i64, i64, i64, i64, i64, i64 }* %x2 to i8*
    %cast.11 = bitcast { i64, i64, i64, i64, i64, i64 }* %y2 to i8*
    call addrspace(0) void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %cast.10, i8* align 8 %cast.11, i64 48, i1 false)
  )RAW_RESULT";

  bool isOK = h.expectBlock(exp);
  EXPECT_TRUE(isOK && "Block does not have expected contents");

  bool broken = h.finish(PreserveDebugInfo);
  EXPECT_FALSE(broken && "Module failed to verify.");
}

TEST_P(BackendArrayStructTests, TestStructFieldAddressExpr) {
  auto cc = GetParam();
  // Test address expression of struct field.
  FcnTestHarness h(cc, "foo");
  Llvm_backend *be = h.be();
  Location loc;

  // type T struct {
  //    f1 int32
  // }
  Btype *bi32t = be->integer_type(false, 32);
  Btype *bpi32t = be->pointer_type(bi32t);
  Btype *s1t = mkBackendStruct(be, bi32t, "f1", nullptr);

  // var t1 T // local
  // var t2 T // global
  // var a1 = &t1.f1
  // var a2 = &t2.f1
  Bvariable *t1 = h.mkLocal("t1", s1t);
  Bexpression *t1vex = be->var_expression(t1, loc);
  Bexpression *fex1 = be->struct_field_expression(t1vex, 0, loc);
  Bexpression *aex1 = be->address_expression(fex1, loc);
  h.mkLocal("a1", bpi32t, aex1);

  Bvariable *t2 = be->global_variable("t2", "t2", s1t, false, /* is_external */
                                      false, /* is_hidden */
                                      false, /* unique_section */
                                      loc);
  Bexpression *t2vex = be->var_expression(t2, loc);
  Bexpression *fex2 = be->struct_field_expression(t2vex, 0, loc);
  Bexpression *aex2 = be->address_expression(fex2, loc);
  h.mkLocal("a2", bpi32t, aex2);

  const char *exp = R"RAW_RESULT(
    %cast.0 = bitcast { i32 }* %t1 to i8*
    %cast.1 = bitcast { i32 }* @const.0 to i8*
    call addrspace(0) void @llvm.memcpy.p0i8.p0i8.i64(i8* align 4 %cast.0, i8* align 4 %cast.1, i64 4, i1 false)
    %field.0 = getelementptr inbounds { i32 }, { i32 }* %t1, i32 0, i32 0
    store i32* %field.0, i32** %a1
    store i32* getelementptr inbounds ({ i32 }, { i32 }* @t2, i32 0, i32 0), i32** %a2
  )RAW_RESULT";

  bool isOK = h.expectBlock(exp);
  EXPECT_TRUE(isOK && "Block does not have expected contents");

  bool broken = h.finish(PreserveDebugInfo);
  EXPECT_FALSE(broken && "Module failed to verify.");
}

} // namespace
