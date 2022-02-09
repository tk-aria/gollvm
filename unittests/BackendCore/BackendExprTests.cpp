//===- llvm/tools/gollvm/unittests/BackendCore/BackendExprTests.cpp -----===//
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

class BackendExprTests : public testing::TestWithParam<llvm::CallingConv::ID> {
};

INSTANTIATE_TEST_SUITE_P(
    UnitTest, BackendExprTests,
    goBackendUnitTests::CConvs,
    [](const testing::TestParamInfo<BackendExprTests::ParamType> &info) {
      std::string name = goBackendUnitTests::ccName(info.param);
      return name;
    });

TEST_P(BackendExprTests, MakeBoolConstExpr) {
  llvm::LLVMContext C;
  auto cc = GetParam();
  std::unique_ptr<Backend> be(go_get_backend(C, cc));

  // Boolean constants
  Bexpression *trueval = be->boolean_constant_expression(true);
  ASSERT_TRUE(trueval != nullptr);
  EXPECT_EQ(repr(trueval->value()), "i8 1");
  Bexpression *falseval = be->boolean_constant_expression(false);
  ASSERT_TRUE(falseval != nullptr);
  EXPECT_EQ(repr(falseval->value()), "i8 0");
}

TEST_P(BackendExprTests, MakeIntConstExpr) {
  llvm::LLVMContext C;
  auto cc = GetParam();
  std::unique_ptr<Backend> be(go_get_backend(C, cc));

  // Integer constants, signed and unsigned
  Btype *bi64t = be->integer_type(false, 64);
  ASSERT_TRUE(bi64t != nullptr);
  static const int64_t i64tvals[] = {-9223372036854775807, 0, 1, 17179869184,
                                     9223372036854775807};
  for (auto val : i64tvals) {
    mpz_t mpz_val;
    memset(&mpz_val, '0', sizeof(mpz_val));
    mpz_init_set_si(mpz_val, val);
    Bexpression *beval = be->integer_constant_expression(bi64t, mpz_val);
    ASSERT_TRUE(beval != nullptr);
    EXPECT_EQ(beval->value(), llvm::ConstantInt::getSigned(bi64t->type(), val));
    mpz_clear(mpz_val);
  }

  Btype *bu64t = be->integer_type(true, 64);
  ASSERT_TRUE(bu64t != nullptr);
  static const uint64_t u64tvals[] = {0, 1, 9223372036854775807ull,
                                      17293822569102704639ull};
  for (auto val : u64tvals) {
    mpz_t mpz_val;
    memset(&mpz_val, '0', sizeof(mpz_val));
    mpz_init_set_ui(mpz_val, val);
    Bexpression *beval = be->integer_constant_expression(bu64t, mpz_val);
    ASSERT_TRUE(beval != nullptr);
    EXPECT_EQ(beval->value(), llvm::ConstantInt::get(bu64t->type(), val));
    mpz_clear(mpz_val);
  }

  // Frontend will occasionally create create a signed -1 value with
  // mpz_init_set_si and then hand this value off to to the bridge
  // with a smaller unsigned type. Verify that this case works
  // correctly.
  mpz_t mpz_val;
  memset(&mpz_val, '0', sizeof(mpz_val));
  mpz_init_set_si(mpz_val, int64_t(-1));
  Btype *bu32t = be->integer_type(true, 32);
  Bexpression *beval = be->integer_constant_expression(bu32t, mpz_val);
  ASSERT_TRUE(beval != nullptr);
  uint32_t um1 = uint32_t(-1);
  EXPECT_EQ(beval->value(), llvm::ConstantInt::get(bu32t->type(), um1));
  mpz_clear(mpz_val);
}

TEST_P(BackendExprTests, MakeFloatConstExpr) {
  llvm::LLVMContext C;
  auto cc = GetParam();
  std::unique_ptr<Backend> be(go_get_backend(C, cc));

  // Float constants
  Btype *bf32t = be->float_type(32);
  ASSERT_TRUE(bf32t != nullptr);
  static const float f32vals[] = {3.402823466e+38F, 0.0f, 1.1f,
                                  1.175494351e-38F};
  for (auto val : f32vals) {
    mpfr_t mpfr_val;

    mpfr_init(mpfr_val);
    mpfr_set_flt(mpfr_val, val, GMP_RNDN);
    Bexpression *beval = be->float_constant_expression(bf32t, mpfr_val);
    ASSERT_TRUE(beval != nullptr);
    EXPECT_EQ(beval->value(),
              llvm::ConstantFP::get(bf32t->type(), static_cast<double>(val)));

    mpfr_set_flt(mpfr_val, -val, GMP_RNDN);
    Bexpression *nbeval = be->float_constant_expression(bf32t, mpfr_val);
    ASSERT_TRUE(nbeval != nullptr);
    EXPECT_EQ(nbeval->value(),
              llvm::ConstantFP::get(bf32t->type(), static_cast<double>(-val)));

    mpfr_clear(mpfr_val);
  }

  // Double constants
  Btype *bf64t = be->float_type(64);
  ASSERT_TRUE(bf64t != nullptr);
  static const double f64vals[] = {1.7976931348623158e+308, 0.0f, 1.1f,
                                   2.2250738585072014e-308};
  for (auto val : f64vals) {
    mpfr_t mpfr_val;

    mpfr_init(mpfr_val);
    mpfr_set_d(mpfr_val, val, GMP_RNDN);
    Bexpression *beval = be->float_constant_expression(bf64t, mpfr_val);
    ASSERT_TRUE(beval != nullptr);
    EXPECT_EQ(beval->value(), llvm::ConstantFP::get(bf64t->type(), val));

    mpfr_set_d(mpfr_val, -val, GMP_RNDN);
    Bexpression *nbeval = be->float_constant_expression(bf64t, mpfr_val);
    ASSERT_TRUE(nbeval != nullptr);
    EXPECT_EQ(nbeval->value(), llvm::ConstantFP::get(bf64t->type(), -val));

    mpfr_clear(mpfr_val);
  }
}

TEST_P(BackendExprTests, MakeComplexConstExpr) {
  llvm::LLVMContext C;
  auto cc = GetParam();
  std::unique_ptr<Backend> be(go_get_backend(C, cc));

  // Complex constants
  Btype *bc64t = be->complex_type(64);
  Btype *bc128t = be->complex_type(128);
  Btype *bf32t = be->float_type(32);
  Btype *bf64t = be->float_type(64);
  static const double f64vals[] = {1.7976931348623158e+308, 0.0, 1.1,
                                   2.2250738585072014e-308};
  for (auto valr : f64vals)
    for (auto vali : f64vals) {
      mpc_t mpc_val;
      mpc_init2(mpc_val, 256);
      mpc_set_d_d(mpc_val, valr, vali, GMP_RNDN);

      Bexpression *bc64val = be->complex_constant_expression(bc64t, mpc_val);
      ASSERT_TRUE(bc64val != nullptr);
      llvm::StructType *llc64st = llvm::cast<llvm::StructType>(bc64t->type());
      llvm::SmallVector<llvm::Constant *, 2> llf32vals(2);
      llf32vals[0] = llvm::ConstantFP::get(bf32t->type(), valr);
      llf32vals[1] = llvm::ConstantFP::get(bf32t->type(), vali);
      EXPECT_EQ(bc64val->value(), llvm::ConstantStruct::get(llc64st, llf32vals));

      Bexpression *bc128val = be->complex_constant_expression(bc128t, mpc_val);
      ASSERT_TRUE(bc128val != nullptr);
      llvm::StructType *llc128st = llvm::cast<llvm::StructType>(bc128t->type());
      llvm::SmallVector<llvm::Constant *, 2> llf64vals(2);
      llf64vals[0] = llvm::ConstantFP::get(bf64t->type(), valr);
      llf64vals[1] = llvm::ConstantFP::get(bf64t->type(), vali);
      EXPECT_EQ(bc128val->value(), llvm::ConstantStruct::get(llc128st, llf64vals));

      mpc_clear(mpc_val);
    }
}

TEST_P(BackendExprTests, MakeZeroValueExpr) {
  llvm::LLVMContext C;
  auto cc = GetParam();
  std::unique_ptr<Backend> bep(go_get_backend(C, cc));
  Backend *be = bep.get();

  // Zero value expressions for various types
  Btype *bt = be->bool_type();
  ASSERT_TRUE(bt != nullptr);
  Bexpression *bzero = be->zero_expression(bt);
  ASSERT_TRUE(bzero != nullptr);
  EXPECT_EQ(repr(bzero->value()), "i8 0");
  Btype *pbt = be->pointer_type(bt);
  Bexpression *bpzero = be->zero_expression(pbt);
  ASSERT_TRUE(bpzero != nullptr);
  EXPECT_EQ(repr(bpzero->value()), "i8* null");
  Btype *bi32t = be->integer_type(false, 32);
  Btype *s2t = mkBackendStruct(be, pbt, "f1", bi32t, "f2", nullptr);
  Bexpression *bszero = be->zero_expression(s2t);
  ASSERT_TRUE(bszero != nullptr);
  Btype *bct = be->complex_type(128);
  Bexpression *bczero = be->zero_expression(bct);
  ASSERT_TRUE(bczero != nullptr);
  EXPECT_EQ(repr(bczero->value()), "{ double, double } zeroinitializer");

  // Error handling
  EXPECT_EQ(be->zero_expression(be->error_type()), be->error_expression());
}

TEST_P(BackendExprTests, TestConversionExpressions) {
  auto cc = GetParam();
  FcnTestHarness h(cc, "foo");
  Llvm_backend *be = h.be();
  Location loc;

  // Trivial / no-op conversion
  Btype *bt = be->bool_type();
  ASSERT_TRUE(bt != nullptr);
  Bexpression *bzero = be->zero_expression(bt);
  Bexpression *bcon = be->convert_expression(bt, bzero, Location());
  ASSERT_TRUE(bcon != nullptr);
  EXPECT_EQ(bzero->value(), bcon->value());

  // Casting one pointer to another. This is the equivalent of
  // type S struct {
  //   f1, f2 int32
  // }
  // var x int64
  // ((*S)&x).f1 = 22
  //
  Btype *bi64t = be->integer_type(false, 64);
  Bvariable *xv = h.mkLocal("x", bi64t);
  Btype *bi32t = be->integer_type(false, 32);
  Btype *s2t = mkBackendStruct(be, bi32t, "f1", bi32t, "f2", nullptr);
  Btype *ps2t = be->pointer_type(s2t);
  Bexpression *vex = be->var_expression(xv, loc);
  Bexpression *adx = be->address_expression(vex, loc);
  Bexpression *cast = be->convert_expression(ps2t, adx, loc);
  Bexpression *dex = be->indirect_expression(s2t, cast, false, loc);
  Bexpression *fex = be->struct_field_expression(dex, 1, loc);
  h.mkAssign(fex, mkInt32Const(be, 22));

  DECLARE_EXPECTED_OUTPUT(exp, R"RAW_RESULT(
    store i64 0, i64* %x, align 8
    %cast.0 = bitcast i64* %x to { i32, i32 }*
    %field.0 = getelementptr inbounds { i32, i32 }, { i32, i32 }* %cast.0, i32 0, i32 1
    store i32 22, i32* %field.0, align 4
  )RAW_RESULT");

  bool isOK = h.expectBlock(exp);
  EXPECT_TRUE(isOK && "Block does not have expected contents");

  // Error handling
  Bexpression *econ =
      be->convert_expression(be->error_type(), bzero, Location());
  EXPECT_EQ(econ, be->error_expression());

  bool broken = h.finish(StripDebugInfo);
  EXPECT_FALSE(broken && "Module failed to verify.");
}

TEST_P(BackendExprTests, TestMoreConversionExpressions) {
  auto cc = GetParam();
  FcnTestHarness h(cc, "foo");
  Llvm_backend *be = h.be();
  Bfunction *func = h.func();
  Location loc;

  Btype *bi32t = be->integer_type(false, 32);
  Btype *bpi32t = be->pointer_type(bi32t);

  // *(*int32)parm3 = 5
  {
    Bvariable *p3 = func->getNthParamVar(2);
    Bexpression *ve = be->var_expression(p3, loc);
    Bexpression *conv = be->convert_expression(bpi32t, ve, loc);
    Bexpression *dex = be->indirect_expression(bi32t, conv, false, loc);
    h.mkAssign(dex, mkInt32Const(be, 5));
  }

  // var p float64
  // *(*int32)(uintptr(p)) = 5
  {
    Btype *bf64t = be->float_type(64);
    Btype *bu64t = be->integer_type(true, 64);
    Bvariable *p = h.mkLocal("p", bf64t);
    Bexpression *ve = be->var_expression(p, loc);
    Bexpression *conv = be->convert_expression(bu64t, ve, loc);
    Bexpression *conv2 = be->convert_expression(bpi32t, conv, loc);
    Bexpression *dex = be->indirect_expression(bi32t, conv2, false, loc);
    h.mkAssign(dex, mkInt32Const(be, 5));
  }

  DECLARE_EXPECTED_OUTPUT(exp, R"RAW_RESULT(
    %param3.ld.0 = load i64*, i64** %param3.addr, align 8
    %cast.0 = bitcast i64* %param3.ld.0 to i32*
    store i32 5, i32* %cast.0, align 4
    store double 0.000000e+00, double* %p, align 8
    %p.ld.0 = load double, double* %p, align 8
    %ftoui.0 = fptoui double %p.ld.0 to i64
    %itpcast.0 = inttoptr i64 %ftoui.0 to i32*
    store i32 5, i32* %itpcast.0, align 4
  )RAW_RESULT");

  bool isOK = h.expectBlock(exp);
  EXPECT_TRUE(isOK && "Block does not have expected contents");

  bool broken = h.finish(StripDebugInfo);
  EXPECT_FALSE(broken && "Module failed to verify.");
}

TEST_P(BackendExprTests, TestFloatConversionExpressions) {
  auto cc = GetParam();
  FcnTestHarness h(cc);
  Llvm_backend *be = h.be();
  Btype *bf32t = be->float_type(32);
  Btype *bf64t = be->float_type(64);
  Btype *bi32t = be->integer_type(false, 32);
  Btype *bi64t = be->integer_type(false, 64);
  Btype *bu32t = be->integer_type(true, 32);
  Btype *bu64t = be->integer_type(true, 64);
  BFunctionType *befty1 = mkFuncTyp(be,
                                    L_PARM, bf32t,
                                    L_PARM, bf64t,
                                    L_PARM, bi32t,
                                    L_PARM, bi64t,
                                    L_PARM, bu32t,
                                    L_PARM, bu64t,
                                    L_END);
  Bfunction *func = h.mkFunction("foo", befty1);
  Location loc = h.loc();

  std::vector<Bvariable *> parms;
  for (unsigned ii = 0; ii < 6; ii++)
    parms.push_back(func->getNthParamVar(ii));

  // Generate pairwise conversions between permutations of p0 -- p5.
  for (unsigned jj = 0; jj < 6; ++jj) {
    for (unsigned ii = 0; ii < jj; ++ii) {
      Bvariable *vii = parms[ii];
      Bvariable *vjj = parms[jj];
      Bexpression *vel1 = be->var_expression(vii, loc);
      Bexpression *ver1 = be->var_expression(vjj, loc);
      Bexpression *conv1 = be->convert_expression(vel1->btype(), ver1, loc);
      h.mkAssign(vel1, conv1);
      Bexpression *vel2 = be->var_expression(vjj, loc);
      Bexpression *ver2 = be->var_expression(vii, loc);
      Bexpression *conv2 = be->convert_expression(vel2->btype(), ver2, loc);
      h.mkAssign(vel2, conv2);
    }
  }

  DECLARE_EXPECTED_OUTPUT(exp, R"RAW_RESULT(
    %p1.ld.0 = load double, double* %p1.addr, align 8
    %fptrunc.0 = fptrunc double %p1.ld.0 to float
    store float %fptrunc.0, float* %p0.addr, align 4
    %p0.ld.0 = load float, float* %p0.addr, align 4
    %fpext.0 = fpext float %p0.ld.0 to double
    store double %fpext.0, double* %p1.addr, align 8
    %p2.ld.0 = load i32, i32* %p2.addr, align 4
    %sitof.0 = sitofp i32 %p2.ld.0 to float
    store float %sitof.0, float* %p0.addr, align 4
    %p0.ld.1 = load float, float* %p0.addr, align 4
    %ftosi.0 = fptosi float %p0.ld.1 to i32
    store i32 %ftosi.0, i32* %p2.addr, align 4
    %p2.ld.1 = load i32, i32* %p2.addr, align 4
    %sitof.1 = sitofp i32 %p2.ld.1 to double
    store double %sitof.1, double* %p1.addr, align 8
    %p1.ld.1 = load double, double* %p1.addr, align 8
    %ftosi.1 = fptosi double %p1.ld.1 to i32
    store i32 %ftosi.1, i32* %p2.addr, align 4
    %p3.ld.0 = load i64, i64* %p3.addr, align 8
    %sitof.2 = sitofp i64 %p3.ld.0 to float
    store float %sitof.2, float* %p0.addr, align 4
    %p0.ld.2 = load float, float* %p0.addr, align 4
    %ftosi.2 = fptosi float %p0.ld.2 to i64
    store i64 %ftosi.2, i64* %p3.addr, align 8
    %p3.ld.1 = load i64, i64* %p3.addr, align 8
    %sitof.3 = sitofp i64 %p3.ld.1 to double
    store double %sitof.3, double* %p1.addr, align 8
    %p1.ld.2 = load double, double* %p1.addr, align 8
    %ftosi.3 = fptosi double %p1.ld.2 to i64
    store i64 %ftosi.3, i64* %p3.addr, align 8
    %p3.ld.2 = load i64, i64* %p3.addr, align 8
    %trunc.0 = trunc i64 %p3.ld.2 to i32
    store i32 %trunc.0, i32* %p2.addr, align 4
    %p2.ld.2 = load i32, i32* %p2.addr, align 4
    %sext.0 = sext i32 %p2.ld.2 to i64
    store i64 %sext.0, i64* %p3.addr, align 8
    %p4.ld.0 = load i32, i32* %p4.addr, align 4
    %uitof.0 = uitofp i32 %p4.ld.0 to float
    store float %uitof.0, float* %p0.addr, align 4
    %p0.ld.3 = load float, float* %p0.addr, align 4
    %ftoui.0 = fptoui float %p0.ld.3 to i32
    store i32 %ftoui.0, i32* %p4.addr, align 4
    %p4.ld.1 = load i32, i32* %p4.addr, align 4
    %uitof.1 = uitofp i32 %p4.ld.1 to double
    store double %uitof.1, double* %p1.addr, align 8
    %p1.ld.3 = load double, double* %p1.addr, align 8
    %ftoui.1 = fptoui double %p1.ld.3 to i32
    store i32 %ftoui.1, i32* %p4.addr, align 4
    %p4.ld.2 = load i32, i32* %p4.addr, align 4
    store i32 %p4.ld.2, i32* %p2.addr, align 4
    %p2.ld.3 = load i32, i32* %p2.addr, align 4
    store i32 %p2.ld.3, i32* %p4.addr, align 4
    %p4.ld.3 = load i32, i32* %p4.addr, align 4
    %zext.0 = zext i32 %p4.ld.3 to i64
    store i64 %zext.0, i64* %p3.addr, align 8
    %p3.ld.3 = load i64, i64* %p3.addr, align 8
    %trunc.1 = trunc i64 %p3.ld.3 to i32
    store i32 %trunc.1, i32* %p4.addr, align 4
    %p5.ld.0 = load i64, i64* %p5.addr, align 8
    %uitof.2 = uitofp i64 %p5.ld.0 to float
    store float %uitof.2, float* %p0.addr, align 4
    %p0.ld.4 = load float, float* %p0.addr, align 4
    %ftoui.2 = fptoui float %p0.ld.4 to i64
    store i64 %ftoui.2, i64* %p5.addr, align 8
    %p5.ld.1 = load i64, i64* %p5.addr, align 8
    %uitof.3 = uitofp i64 %p5.ld.1 to double
    store double %uitof.3, double* %p1.addr, align 8
    %p1.ld.4 = load double, double* %p1.addr, align 8
    %ftoui.3 = fptoui double %p1.ld.4 to i64
    store i64 %ftoui.3, i64* %p5.addr, align 8
    %p5.ld.2 = load i64, i64* %p5.addr, align 8
    %trunc.2 = trunc i64 %p5.ld.2 to i32
    store i32 %trunc.2, i32* %p2.addr, align 4
    %p2.ld.4 = load i32, i32* %p2.addr, align 4
    %sext.1 = sext i32 %p2.ld.4 to i64
    store i64 %sext.1, i64* %p5.addr, align 8
    %p5.ld.3 = load i64, i64* %p5.addr, align 8
    store i64 %p5.ld.3, i64* %p3.addr, align 8
    %p3.ld.4 = load i64, i64* %p3.addr, align 8
    store i64 %p3.ld.4, i64* %p5.addr, align 8
    %p5.ld.4 = load i64, i64* %p5.addr, align 8
    %trunc.3 = trunc i64 %p5.ld.4 to i32
    store i32 %trunc.3, i32* %p4.addr, align 4
    %p4.ld.4 = load i32, i32* %p4.addr, align 4
    %zext.1 = zext i32 %p4.ld.4 to i64
    store i64 %zext.1, i64* %p5.addr, align 8
  )RAW_RESULT");

  bool isOK = h.expectBlock(exp);
  EXPECT_TRUE(isOK && "Block does not have expected contents");

  bool broken = h.finish(StripDebugInfo);
  EXPECT_FALSE(broken && "Module failed to verify.");
}

TEST_P(BackendExprTests, TestComplexConversionExpression) {
  auto cc = GetParam();
  FcnTestHarness h(cc);
  Llvm_backend *be = h.be();
  BFunctionType *befty = mkFuncTyp(be, L_END);
  Bfunction *func = h.mkFunction("foo", befty);
  Location loc;

  Btype *bc64t = be->complex_type(64);
  Btype *bc128t = be->complex_type(128);

  // var a, b complex64
  // var x, y complex128
  Bvariable *a = h.mkLocal("a", bc64t);
  Bvariable *b = h.mkLocal("b", bc64t);
  Bvariable *x = h.mkLocal("x", bc128t);
  Bvariable *y = h.mkLocal("y", bc128t);

  // a = complex64(x)
  Bexpression *avex1 = be->var_expression(a, loc);
  Bexpression *xvex1 = be->var_expression(x, loc);
  Bexpression *convex1 = be->convert_expression(bc64t, xvex1, loc);
  h.mkAssign(avex1, convex1);

  // y = complex128(b)
  Bexpression *yvex2 = be->var_expression(y, loc);
  Bexpression *bvex2 = be->var_expression(b, loc);
  Bexpression *convex2 = be->convert_expression(bc128t, bvex2, loc);
  h.mkAssign(yvex2, convex2);

  // No-op conversions
  // a = complex64(b)
  Bexpression *avex3 = be->var_expression(a, loc);
  Bexpression *bvex3 = be->var_expression(b, loc);
  Bexpression *convex3 = be->convert_expression(bc64t, bvex3, loc);
  h.mkAssign(avex3, convex3);

  // x = complex128(y)
  Bexpression *xvex4 = be->var_expression(x, loc);
  Bexpression *yvex4 = be->var_expression(y, loc);
  Bexpression *convex4 = be->convert_expression(bc128t, yvex4, loc);
  h.mkAssign(xvex4, convex4);

  DECLARE_EXPECTED_OUTPUT(exp, R"RAW_RESULT(
    define void @foo(i8* nest %nest.0) #0 {
  entry:
    %tmp.3 = alloca { double, double }, align 8
    %tmp.2 = alloca { float, float }, align 4
    %tmp.1 = alloca { float, float }, align 4
    %tmp.0 = alloca { double, double }, align 8
    %a = alloca { float, float }, align 4
    %b = alloca { float, float }, align 4
    %x = alloca { double, double }, align 8
    %y = alloca { double, double }, align 8
    %cast.0 = bitcast { float, float }* %a to i8*
    call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 4 %cast.0, i8* align 4 bitcast ({ float, float }* @const.0 to i8*), i64 8, i1 false)
    %cast.1 = bitcast { float, float }* %b to i8*
    call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 4 %cast.1, i8* align 4 bitcast ({ float, float }* @const.0 to i8*), i64 8, i1 false)
    %cast.2 = bitcast { double, double }* %x to i8*
    call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %cast.2, i8* align 8 bitcast ({ double, double }* @const.1 to i8*), i64 16, i1 false)
    %cast.3 = bitcast { double, double }* %y to i8*
    call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %cast.3, i8* align 8 bitcast ({ double, double }* @const.1 to i8*), i64 16, i1 false)
    %cast.4 = bitcast { double, double }* %tmp.0 to i8*
    %cast.5 = bitcast { double, double }* %x to i8*
    call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %cast.4, i8* align 8 %cast.5, i64 16, i1 false)
    %field.0 = getelementptr inbounds { double, double }, { double, double }* %tmp.0, i32 0, i32 0
    %.real.ld.0 = load double, double* %field.0, align 8
    %fptrunc.0 = fptrunc double %.real.ld.0 to float
    %field.1 = getelementptr inbounds { double, double }, { double, double }* %tmp.0, i32 0, i32 1
    %.imag.ld.0 = load double, double* %field.1, align 8
    %fptrunc.1 = fptrunc double %.imag.ld.0 to float
    %field.2 = getelementptr inbounds { float, float }, { float, float }* %tmp.1, i32 0, i32 0
    store float %fptrunc.0, float* %field.2, align 4
    %field.3 = getelementptr inbounds { float, float }, { float, float }* %tmp.1, i32 0, i32 1
    store float %fptrunc.1, float* %field.3, align 4
    %cast.6 = bitcast { float, float }* %a to i8*
    %cast.7 = bitcast { float, float }* %tmp.1 to i8*
    call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 4 %cast.6, i8* align 4 %cast.7, i64 8, i1 false)
    %cast.8 = bitcast { float, float }* %tmp.2 to i8*
    %cast.9 = bitcast { float, float }* %b to i8*
    call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 4 %cast.8, i8* align 4 %cast.9, i64 8, i1 false)
    %field.4 = getelementptr inbounds { float, float }, { float, float }* %tmp.2, i32 0, i32 0
    %.real.ld.1 = load float, float* %field.4, align 4
    %fpext.0 = fpext float %.real.ld.1 to double
    %field.5 = getelementptr inbounds { float, float }, { float, float }* %tmp.2, i32 0, i32 1
    %.imag.ld.1 = load float, float* %field.5, align 4
    %fpext.1 = fpext float %.imag.ld.1 to double
    %field.6 = getelementptr inbounds { double, double }, { double, double }* %tmp.3, i32 0, i32 0
    store double %fpext.0, double* %field.6, align 8
    %field.7 = getelementptr inbounds { double, double }, { double, double }* %tmp.3, i32 0, i32 1
    store double %fpext.1, double* %field.7, align 8
    %cast.10 = bitcast { double, double }* %y to i8*
    %cast.11 = bitcast { double, double }* %tmp.3 to i8*
    call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %cast.10, i8* align 8 %cast.11, i64 16, i1 false)
    %cast.12 = bitcast { float, float }* %a to i8*
    %cast.13 = bitcast { float, float }* %b to i8*
    call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 4 %cast.12, i8* align 4 %cast.13, i64 8, i1 false)
    %cast.14 = bitcast { double, double }* %x to i8*
    %cast.15 = bitcast { double, double }* %y to i8*
    call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %cast.14, i8* align 8 %cast.15, i64 16, i1 false)
    ret void
  }
  )RAW_RESULT");

  bool broken = h.finish(StripDebugInfo);
  EXPECT_FALSE(broken && "Module failed to verify.");

  bool isOK = h.expectValue(func->function(), exp);
  EXPECT_TRUE(isOK && "Function does not have expected contents");
}

TEST_P(BackendExprTests, MakeVarExpressions) {
  auto cc = GetParam();
  FcnTestHarness h(cc, "foo");
  Llvm_backend *be = h.be();
  Location loc;

  Btype *bi64t = be->integer_type(false, 64);
  Bvariable *loc1 = h.mkLocal("loc1", bi64t, mkInt64Const(be, 10));

  // We should get a distinct Bexpression each time we create a new
  // var expression.
  Bexpression *ve1 = be->var_expression(loc1, loc);
  EXPECT_EQ(repr(ve1->value()), "%loc1 = alloca i64, align 8");
  h.mkExprStmt(ve1);
  Bexpression *ve2 = be->var_expression(loc1, loc);
  h.mkExprStmt(ve2);
  EXPECT_EQ(repr(ve2->value()), "%loc1 = alloca i64, align 8");
  EXPECT_NE(ve1, ve2);

  // Same here.
  Bexpression *ve3 = be->var_expression(loc1, loc);
  Bexpression *ve3r = be->var_expression(loc1, loc);
  EXPECT_EQ(repr(ve3->value()), "%loc1 = alloca i64, align 8");
  h.mkAssign(ve3, ve3r);
  Bexpression *ve4 = be->var_expression(loc1, loc);
  Bexpression *ve4r = be->var_expression(loc1, loc);
  EXPECT_EQ(repr(ve4->value()), "%loc1 = alloca i64, align 8");
  EXPECT_NE(ve3, ve4);
  h.mkAssign(ve4, ve4r);

  bool broken = h.finish(PreserveDebugInfo);
  EXPECT_FALSE(broken && "Module failed to verify.");
}

TEST_P(BackendExprTests, TestCompareOps) {
  auto cc = GetParam();
  FcnTestHarness h(cc, "foo");
  Llvm_backend *be = h.be();
  Bfunction *func = h.func();

  Operator optotest[] = {OPERATOR_EQEQ, OPERATOR_NOTEQ, OPERATOR_LT,
                         OPERATOR_LE,   OPERATOR_GT,    OPERATOR_GE};

  Btype *bi64t = be->integer_type(false, 64);
  Btype *bui64t = be->integer_type(true, 64);
  Btype *bf64t = be->float_type(64);
  Bvariable *x = h.mkLocal("x", bi64t);
  Bvariable *y = h.mkLocal("y", bui64t);
  Bvariable *z = h.mkLocal("z", bf64t);
  Bexpression *beic = mkInt64Const(be, 9);
  Bexpression *beuc = mkUint64Const(be, 9);
  Bexpression *befc = mkFloat64Const(be, 9.0);
  std::vector<std::pair<Bexpression *, Bvariable *>> valtotest;
  valtotest.push_back(std::make_pair(beic, x));
  valtotest.push_back(std::make_pair(beuc, y));
  valtotest.push_back(std::make_pair(befc, z));

  Location loc;
  for (unsigned tidx = 0; tidx < valtotest.size(); ++tidx) {
    Bexpression *bleft = valtotest[tidx].first;
    Bvariable *bv = valtotest[tidx].second;
    for (auto op : optotest) {
      Bexpression *bright = be->var_expression(bv, loc);
      Bexpression *cmp = be->binary_expression(op, bleft, bright, Location());
      Bstatement *es = be->expression_statement(func, cmp);
      h.addStmt(es);
    }
  }

  DECLARE_EXPECTED_OUTPUT(exp, R"RAW_RESULT(
    store i64 0, i64* %x, align 8
    store i64 0, i64* %y, align 8
    store double 0.000000e+00, double* %z, align 8
    %x.ld.0 = load i64, i64* %x, align 8
    %icmp.0 = icmp eq i64 9, %x.ld.0
    %zext.0 = zext i1 %icmp.0 to i8
    %x.ld.1 = load i64, i64* %x, align 8
    %icmp.1 = icmp ne i64 9, %x.ld.1
    %zext.1 = zext i1 %icmp.1 to i8
    %x.ld.2 = load i64, i64* %x, align 8
    %icmp.2 = icmp slt i64 9, %x.ld.2
    %zext.2 = zext i1 %icmp.2 to i8
    %x.ld.3 = load i64, i64* %x, align 8
    %icmp.3 = icmp sle i64 9, %x.ld.3
    %zext.3 = zext i1 %icmp.3 to i8
    %x.ld.4 = load i64, i64* %x, align 8
    %icmp.4 = icmp sgt i64 9, %x.ld.4
    %zext.4 = zext i1 %icmp.4 to i8
    %x.ld.5 = load i64, i64* %x, align 8
    %icmp.5 = icmp sge i64 9, %x.ld.5
    %zext.5 = zext i1 %icmp.5 to i8
    %y.ld.0 = load i64, i64* %y, align 8
    %icmp.6 = icmp eq i64 9, %y.ld.0
    %zext.6 = zext i1 %icmp.6 to i8
    %y.ld.1 = load i64, i64* %y, align 8
    %icmp.7 = icmp ne i64 9, %y.ld.1
    %zext.7 = zext i1 %icmp.7 to i8
    %y.ld.2 = load i64, i64* %y, align 8
    %icmp.8 = icmp ult i64 9, %y.ld.2
    %zext.8 = zext i1 %icmp.8 to i8
    %y.ld.3 = load i64, i64* %y, align 8
    %icmp.9 = icmp ule i64 9, %y.ld.3
    %zext.9 = zext i1 %icmp.9 to i8
    %y.ld.4 = load i64, i64* %y, align 8
    %icmp.10 = icmp ugt i64 9, %y.ld.4
    %zext.10 = zext i1 %icmp.10 to i8
    %y.ld.5 = load i64, i64* %y, align 8
    %icmp.11 = icmp uge i64 9, %y.ld.5
    %zext.11 = zext i1 %icmp.11 to i8
    %z.ld.0 = load double, double* %z, align 8
    %fcmp.0 = fcmp oeq double 9.000000e+00, %z.ld.0
    %zext.12 = zext i1 %fcmp.0 to i8
    %z.ld.1 = load double, double* %z, align 8
    %fcmp.1 = fcmp une double 9.000000e+00, %z.ld.1
    %zext.13 = zext i1 %fcmp.1 to i8
    %z.ld.2 = load double, double* %z, align 8
    %fcmp.2 = fcmp olt double 9.000000e+00, %z.ld.2
    %zext.14 = zext i1 %fcmp.2 to i8
    %z.ld.3 = load double, double* %z, align 8
    %fcmp.3 = fcmp ole double 9.000000e+00, %z.ld.3
    %zext.15 = zext i1 %fcmp.3 to i8
    %z.ld.4 = load double, double* %z, align 8
    %fcmp.4 = fcmp ogt double 9.000000e+00, %z.ld.4
    %zext.16 = zext i1 %fcmp.4 to i8
    %z.ld.5 = load double, double* %z, align 8
    %fcmp.5 = fcmp oge double 9.000000e+00, %z.ld.5
    %zext.17 = zext i1 %fcmp.5 to i8
  )RAW_RESULT");

  bool isOK = h.expectBlock(exp);
  EXPECT_TRUE(isOK && "Block does not have expected contents");

  bool broken = h.finish(StripDebugInfo);
  EXPECT_FALSE(broken && "Module failed to verify.");
}

TEST_P(BackendExprTests, TestArithOps) {
  auto cc = GetParam();
  FcnTestHarness h(cc, "foo");
  Llvm_backend *be = h.be();
  Bfunction *func = h.func();

  Operator optotest[] = {OPERATOR_PLUS,OPERATOR_MINUS};

  Btype *bi64t = be->integer_type(false, 64);
  Btype *bf64t = be->float_type(64);
  Bvariable *x = h.mkLocal("x", bi64t);
  Bvariable *y = h.mkLocal("y", bf64t);
  Bexpression *beic = mkInt64Const(be, 9);
  Bexpression *befc = mkFloat64Const(be, 9.0);
  std::vector<std::pair<Bexpression *, Bvariable *>> valtotest;
  valtotest.push_back(std::make_pair(beic, x));
  valtotest.push_back(std::make_pair(befc, y));

  Location loc;
  for (unsigned tidx = 0; tidx < valtotest.size(); ++tidx) {
    Bexpression *bleft = valtotest[tidx].first;
    Bvariable *bv = valtotest[tidx].second;
    for (auto op : optotest) {
      Bexpression *bright = be->var_expression(bv, loc);
      Bexpression *cmp = be->binary_expression(op, bleft, bright, loc);
      Bstatement *es = be->expression_statement(func, cmp);
      h.addStmt(es);
    }
  }

  DECLARE_EXPECTED_OUTPUT(exp, R"RAW_RESULT(
    store i64 0, i64* %x, align 8
    store double 0.000000e+00, double* %y, align 8
    %x.ld.0 = load i64, i64* %x, align 8
    %add.0 = add i64 9, %x.ld.0
    %x.ld.1 = load i64, i64* %x, align 8
    %sub.0 = sub i64 9, %x.ld.1
    %y.ld.0 = load double, double* %y, align 8
    %fadd.0 = fadd double 9.000000e+00, %y.ld.0
    %y.ld.1 = load double, double* %y, align 8
    %fsub.0 = fsub double 9.000000e+00, %y.ld.1
  )RAW_RESULT");

  bool isOK = h.expectBlock(exp);
  EXPECT_TRUE(isOK && "Block does not have expected contents");

  bool broken = h.finish(StripDebugInfo);
  EXPECT_FALSE(broken && "Module failed to verify.");
}

TEST_P(BackendExprTests, TestMoreArith) {
  auto cc = GetParam();
  FcnTestHarness h(cc, "foo");
  Llvm_backend *be = h.be();

  // var x int64, y = 9, z = 10, w = 11
  Btype *bi64t = be->integer_type(false, 64);
  Bvariable *x = h.mkLocal("x", bi64t);
  Bvariable *y = h.mkLocal("y", bi64t, mkInt64Const(be, 9));
  Bvariable *z = h.mkLocal("z", bi64t, mkInt64Const(be, 10));
  Bvariable *w = h.mkLocal("w", bi64t, mkInt64Const(be, 11));

  // x = y + z + w
  Location loc;
  Bexpression *vey = be->var_expression(y, loc);
  Bexpression *vez = be->var_expression(z, loc);
  Bexpression *vew = be->var_expression(w, loc);
  Bexpression *ypz = be->binary_expression(OPERATOR_PLUS, vey, vez, loc);
  Bexpression *ypzpw = be->binary_expression(OPERATOR_PLUS, ypz, vew, loc);
  Bexpression *vex = be->var_expression(x, loc);
  h.mkAssign(vex, ypzpw);

  DECLARE_EXPECTED_OUTPUT(exp, R"RAW_RESULT(
    store i64 0, i64* %x, align 8
    store i64 9, i64* %y, align 8
    store i64 10, i64* %z, align 8
    store i64 11, i64* %w, align 8
    %y.ld.0 = load i64, i64* %y, align 8
    %z.ld.0 = load i64, i64* %z, align 8
    %add.0 = add i64 %y.ld.0, %z.ld.0
    %w.ld.0 = load i64, i64* %w, align 8
    %add.1 = add i64 %add.0, %w.ld.0
    store i64 %add.1, i64* %x, align 8
  )RAW_RESULT");

  bool isOK = h.expectBlock(exp);
  EXPECT_TRUE(isOK && "Block does not have expected contents");

  bool broken = h.finish(StripDebugInfo);
  EXPECT_FALSE(broken && "Module failed to verify.");
}

TEST_P(BackendExprTests, TestLogicalOps) {
  auto cc = GetParam();
  FcnTestHarness h(cc, "foo");
  Llvm_backend *be = h.be();
  Bfunction *func = h.func();

  Operator optotest[] = {OPERATOR_ANDAND, OPERATOR_OROR, OPERATOR_AND,
                         OPERATOR_OR, OPERATOR_XOR, OPERATOR_BITCLEAR};

  Btype *bi64t = be->integer_type(false, 64);
  Btype *bui64t = be->integer_type(true, 64);
  Btype *bt = be->bool_type();
  Bvariable *x = h.mkLocal("x", bi64t);
  Bvariable *y = h.mkLocal("y", bui64t);
  Bvariable *z = h.mkLocal("z", bt);
  Bvariable *x2 = h.mkLocal("x2", bi64t);
  Bvariable *y2 = h.mkLocal("y2", bui64t);
  Bvariable *z2 = h.mkLocal("z2", bt);
  std::vector<std::pair<Bvariable *, Bvariable *>> valtotest;
  valtotest.push_back(std::make_pair(x, x2));
  valtotest.push_back(std::make_pair(y, y2));
  valtotest.push_back(std::make_pair(z, z2));

  Location loc;
  for (unsigned tidx = 0; tidx < valtotest.size(); ++tidx) {
    Bvariable *bvl = valtotest[tidx].first;
    Bvariable *bvr = valtotest[tidx].second;
    for (auto op : optotest) {
      Bexpression *bleft = be->var_expression(bvl, loc);
      Bexpression *bright = be->var_expression(bvr, loc);
      Bexpression *cmp = be->binary_expression(op, bleft, bright, Location());
      Bstatement *es = be->expression_statement(func, cmp);
      h.addStmt(es);
    }
  }

  DECLARE_EXPECTED_OUTPUT(exp, R"RAW_RESULT(
    store i64 0, i64* %x, align 8
    store i64 0, i64* %y, align 8
    store i8 0, i8* %z, align 1
    store i64 0, i64* %x2, align 8
    store i64 0, i64* %y2, align 8
    store i8 0, i8* %z2, align 1
    %x.ld.0 = load i64, i64* %x, align 8
    %x2.ld.0 = load i64, i64* %x2, align 8
    %iand.0 = and i64 %x.ld.0, %x2.ld.0
    %x.ld.1 = load i64, i64* %x, align 8
    %x2.ld.1 = load i64, i64* %x2, align 8
    %ior.0 = or i64 %x.ld.1, %x2.ld.1
    %x.ld.2 = load i64, i64* %x, align 8
    %x2.ld.2 = load i64, i64* %x2, align 8
    %iand.1 = and i64 %x.ld.2, %x2.ld.2
    %x.ld.3 = load i64, i64* %x, align 8
    %x2.ld.3 = load i64, i64* %x2, align 8
    %ior.1 = or i64 %x.ld.3, %x2.ld.3
    %x.ld.4 = load i64, i64* %x, align 8
    %x2.ld.4 = load i64, i64* %x2, align 8
    %xor.0 = xor i64 %x.ld.4, %x2.ld.4
    %x.ld.5 = load i64, i64* %x, align 8
    %x2.ld.5 = load i64, i64* %x2, align 8
    %iand.2 = and i64 %x.ld.5, %x2.ld.5
    %y.ld.0 = load i64, i64* %y, align 8
    %y2.ld.0 = load i64, i64* %y2, align 8
    %iand.3 = and i64 %y.ld.0, %y2.ld.0
    %y.ld.1 = load i64, i64* %y, align 8
    %y2.ld.1 = load i64, i64* %y2, align 8
    %ior.2 = or i64 %y.ld.1, %y2.ld.1
    %y.ld.2 = load i64, i64* %y, align 8
    %y2.ld.2 = load i64, i64* %y2, align 8
    %iand.4 = and i64 %y.ld.2, %y2.ld.2
    %y.ld.3 = load i64, i64* %y, align 8
    %y2.ld.3 = load i64, i64* %y2, align 8
    %ior.3 = or i64 %y.ld.3, %y2.ld.3
    %y.ld.4 = load i64, i64* %y, align 8
    %y2.ld.4 = load i64, i64* %y2, align 8
    %xor.1 = xor i64 %y.ld.4, %y2.ld.4
    %y.ld.5 = load i64, i64* %y, align 8
    %y2.ld.5 = load i64, i64* %y2, align 8
    %iand.5 = and i64 %y.ld.5, %y2.ld.5
    %z.ld.0 = load i8, i8* %z, align 1
    %z2.ld.0 = load i8, i8* %z2, align 1
    %iand.6 = and i8 %z.ld.0, %z2.ld.0
    %z.ld.1 = load i8, i8* %z, align 1
    %z2.ld.1 = load i8, i8* %z2, align 1
    %ior.4 = or i8 %z.ld.1, %z2.ld.1
    %z.ld.2 = load i8, i8* %z, align 1
    %z2.ld.2 = load i8, i8* %z2, align 1
    %iand.7 = and i8 %z.ld.2, %z2.ld.2
    %z.ld.3 = load i8, i8* %z, align 1
    %z2.ld.3 = load i8, i8* %z2, align 1
    %ior.5 = or i8 %z.ld.3, %z2.ld.3
    %z.ld.4 = load i8, i8* %z, align 1
    %z2.ld.4 = load i8, i8* %z2, align 1
    %xor.2 = xor i8 %z.ld.4, %z2.ld.4
    %z.ld.5 = load i8, i8* %z, align 1
    %z2.ld.5 = load i8, i8* %z2, align 1
    %iand.8 = and i8 %z.ld.5, %z2.ld.5
  )RAW_RESULT");

  bool isOK = h.expectBlock(exp);
  EXPECT_TRUE(isOK && "Block does not have expected contents");

  bool broken = h.finish(StripDebugInfo);
  EXPECT_FALSE(broken && "Module failed to verify.");
}

TEST_P(BackendExprTests, TestMulDiv) {
  auto cc = GetParam();
  FcnTestHarness h(cc, "foo");
  Llvm_backend *be = h.be();
  Bfunction *func = h.func();

  Operator optotest[] = {OPERATOR_MULT, OPERATOR_DIV, OPERATOR_MOD};

  Btype *bi16t = be->integer_type(false, 16);
  Btype *bu16t = be->integer_type(true, 16);
  Btype *bf64t = be->float_type(64);
  Bvariable *x = h.mkLocal("x", bi16t);
  Bvariable *y = h.mkLocal("y", bu16t);
  Bvariable *z = h.mkLocal("z", bf64t);
  Bexpression *beic = mkIntConst(be, -17, 16u);
  Bexpression *beuc = mkUIntConst(be, 13, 16u);
  Bexpression *befc = mkFloat64Const(be, 9.0);
  std::vector<std::pair<Bexpression *, Bvariable *>> valtotest;
  valtotest.push_back(std::make_pair(beic, x));
  valtotest.push_back(std::make_pair(beuc, y));
  valtotest.push_back(std::make_pair(befc, z));

  Location loc;
  for (unsigned tidx = 0; tidx < valtotest.size(); ++tidx) {
    Bexpression *bleft = valtotest[tidx].first;
    Bvariable *bv = valtotest[tidx].second;
    for (auto op : optotest) {
      if (op == OPERATOR_MOD && bleft->btype()->type()->isFloatingPointTy())
        continue;
      Bexpression *bright = be->var_expression(bv, loc);
      Bexpression *cmp = be->binary_expression(op, bleft, bright, Location());
      Bstatement *es = be->expression_statement(func, cmp);
      h.addStmt(es);
    }
  }

  DECLARE_EXPECTED_OUTPUT(exp, R"RAW_RESULT(
    store i16 0, i16* %x, align 2
    store i16 0, i16* %y, align 2
    store double 0.000000e+00, double* %z, align 8
    %x.ld.0 = load i16, i16* %x, align 2
    %mul.0 = mul i16 -17, %x.ld.0
    %x.ld.1 = load i16, i16* %x, align 2
    %div.0 = sdiv i16 -17, %x.ld.1
    %x.ld.2 = load i16, i16* %x, align 2
    %mod.0 = srem i16 -17, %x.ld.2
    %y.ld.0 = load i16, i16* %y, align 2
    %mul.1 = mul i16 13, %y.ld.0
    %y.ld.1 = load i16, i16* %y, align 2
    %div.1 = udiv i16 13, %y.ld.1
    %y.ld.2 = load i16, i16* %y, align 2
    %mod.1 = urem i16 13, %y.ld.2
    %z.ld.0 = load double, double* %z, align 8
    %fmul.0 = fmul double 9.000000e+00, %z.ld.0
    %z.ld.1 = load double, double* %z, align 8
    %fdiv.0 = fdiv double 9.000000e+00, %z.ld.1
  )RAW_RESULT");

  bool isOK = h.expectBlock(exp);
  EXPECT_TRUE(isOK && "Block does not have expected contents");

  bool broken = h.finish(StripDebugInfo);
  EXPECT_FALSE(broken && "Module failed to verify.");
}

TEST_P(BackendExprTests, TestShift) {
  auto cc = GetParam();
  FcnTestHarness h(cc, "foo");
  Llvm_backend *be = h.be();
  Bfunction *func = h.func();

  Operator optotest[] = {OPERATOR_LSHIFT, OPERATOR_RSHIFT};

  Btype *bi64t = be->integer_type(false, 64);
  Btype *bu64t = be->integer_type(true, 64);
  Btype *bu32t = be->integer_type(true, 32);
  Bvariable *x = h.mkLocal("x", bi64t);
  Bvariable *y = h.mkLocal("y", bu64t);
  Bvariable *s = h.mkLocal("s", bu64t);
  Bvariable *z = h.mkLocal("z", bu32t);
  std::vector<std::pair<Bvariable *, Bvariable *>> valtotest;
  valtotest.push_back(std::make_pair(x, s)); // signed shifts
  valtotest.push_back(std::make_pair(y, s)); // unsigned shifts

  Location loc;
  for (unsigned tidx = 0; tidx < valtotest.size(); ++tidx) {
    Bvariable *bvl = valtotest[tidx].first;
    Bvariable *bvr = valtotest[tidx].second;
    for (auto op : optotest) {
      Bexpression *bleft = be->var_expression(bvl, loc);
      Bexpression *bright = be->var_expression(bvr, loc);
      Bexpression *cmp = be->binary_expression(op, bleft, bright, Location());
      Bstatement *es = be->expression_statement(func, cmp);
      h.addStmt(es);
    }
  }

  // Verify correct behavior when type of shift amount does not match
  // type of shift value.
  {
    Bexpression *bleft = be->var_expression(x, loc);
    Bexpression *bright = be->var_expression(z, loc);
    Bexpression *mix = be->binary_expression(OPERATOR_LSHIFT, bleft, bright,
                                             Location());
    Bstatement *es = be->expression_statement(func, mix);
    h.addStmt(es);
  }
  {
    Bexpression *bleft = be->var_expression(z, loc);
    Bexpression *bright = be->var_expression(y, loc);
    Bexpression *mix = be->binary_expression(OPERATOR_RSHIFT, bleft, bright,
                                             Location());
    Bstatement *es = be->expression_statement(func, mix);
    h.addStmt(es);
  }

  DECLARE_EXPECTED_OUTPUT(exp, R"RAW_RESULT(
    store i64 0, i64* %x, align 8
    store i64 0, i64* %y, align 8
    store i64 0, i64* %s, align 8
    store i32 0, i32* %z, align 4
    %x.ld.0 = load i64, i64* %x, align 8
    %s.ld.0 = load i64, i64* %s, align 8
    %shl.0 = shl i64 %x.ld.0, %s.ld.0
    %x.ld.1 = load i64, i64* %x, align 8
    %s.ld.1 = load i64, i64* %s, align 8
    %shr.0 = ashr i64 %x.ld.1, %s.ld.1
    %y.ld.0 = load i64, i64* %y, align 8
    %s.ld.2 = load i64, i64* %s, align 8
    %shl.1 = shl i64 %y.ld.0, %s.ld.2
    %y.ld.1 = load i64, i64* %y, align 8
    %s.ld.3 = load i64, i64* %s, align 8
    %shr.1 = lshr i64 %y.ld.1, %s.ld.3
    %x.ld.2 = load i64, i64* %x, align 8
    %z.ld.0 = load i32, i32* %z, align 4
    %zext.0 = zext i32 %z.ld.0 to i64
    %shl.2 = shl i64 %x.ld.2, %zext.0
    %z.ld.1 = load i32, i32* %z, align 4
    %y.ld.2 = load i64, i64* %y, align 8
    %trunc.0 = trunc i64 %y.ld.2 to i32
    %shr.2 = lshr i32 %z.ld.1, %trunc.0
  )RAW_RESULT");

  bool isOK = h.expectBlock(exp);
  EXPECT_TRUE(isOK && "Block does not have expected contents");

  bool broken = h.finish(StripDebugInfo);
  EXPECT_FALSE(broken && "Module failed to verify.");
}

TEST_P(BackendExprTests, TestComplexOps) {
  auto cc = GetParam();
  FcnTestHarness h(cc);
  Llvm_backend *be = h.be();
  BFunctionType *befty = mkFuncTyp(be, L_END);
  Bfunction *func = h.mkFunction("foo", befty);
  Location loc;

  Operator optotest[] = {OPERATOR_PLUS, OPERATOR_MINUS, OPERATOR_MULT,
                         OPERATOR_EQEQ, OPERATOR_NOTEQ};

  // var x, y, z complex128
  // var b bool
  Btype *bc128t = be->complex_type(128);
  Btype *bt = be->bool_type();
  Bvariable *x = h.mkLocal("x", bc128t);
  Bvariable *y = h.mkLocal("y", bc128t);
  Bvariable *z = h.mkLocal("z", bc128t);
  Bvariable *b = h.mkLocal("b", bt);

  for (auto op : optotest) {
    Bexpression *bleft = be->var_expression(x, loc);
    Bexpression *bright = be->var_expression(y, loc);
    Bexpression *bop = be->binary_expression(op, bleft, bright, Location());
    Bexpression *bvex = be->var_expression(op == OPERATOR_EQEQ || op == OPERATOR_NOTEQ ? b : z,
                                           loc);
    h.mkAssign(bvex, bop);
  }

  DECLARE_EXPECTED_OUTPUT(exp, R"RAW_RESULT(
    define void @foo(i8* nest %nest.0) #0 {
  entry:
    %tmp.12 = alloca { double, double }, align 8
    %tmp.11 = alloca { double, double }, align 8
    %tmp.10 = alloca { double, double }, align 8
    %tmp.9 = alloca { double, double }, align 8
    %tmp.8 = alloca { double, double }, align 8
    %tmp.7 = alloca { double, double }, align 8
    %tmp.6 = alloca { double, double }, align 8
    %tmp.5 = alloca { double, double }, align 8
    %tmp.4 = alloca { double, double }, align 8
    %tmp.3 = alloca { double, double }, align 8
    %tmp.2 = alloca { double, double }, align 8
    %tmp.1 = alloca { double, double }, align 8
    %tmp.0 = alloca { double, double }, align 8
    %x = alloca { double, double }, align 8
    %y = alloca { double, double }, align 8
    %z = alloca { double, double }, align 8
    %b = alloca i8, align 1
    %cast.0 = bitcast { double, double }* %x to i8*
    call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %cast.0, i8* align 8 bitcast ({ double, double }* @const.0 to i8*), i64 16, i1 false)
    %cast.1 = bitcast { double, double }* %y to i8*
    call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %cast.1, i8* align 8 bitcast ({ double, double }* @const.0 to i8*), i64 16, i1 false)
    %cast.2 = bitcast { double, double }* %z to i8*
    call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %cast.2, i8* align 8 bitcast ({ double, double }* @const.0 to i8*), i64 16, i1 false)
    store i8 0, i8* %b, align 1
    %cast.3 = bitcast { double, double }* %tmp.0 to i8*
    %cast.4 = bitcast { double, double }* %x to i8*
    call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %cast.3, i8* align 8 %cast.4, i64 16, i1 false)
    %cast.5 = bitcast { double, double }* %tmp.1 to i8*
    %cast.6 = bitcast { double, double }* %y to i8*
    call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %cast.5, i8* align 8 %cast.6, i64 16, i1 false)
    %field.0 = getelementptr inbounds { double, double }, { double, double }* %tmp.0, i32 0, i32 0
    %.real.ld.0 = load double, double* %field.0, align 8
    %field.1 = getelementptr inbounds { double, double }, { double, double }* %tmp.1, i32 0, i32 0
    %.real.ld.1 = load double, double* %field.1, align 8
    %fadd.0 = fadd double %.real.ld.0, %.real.ld.1
    %field.2 = getelementptr inbounds { double, double }, { double, double }* %tmp.0, i32 0, i32 1
    %.imag.ld.0 = load double, double* %field.2, align 8
    %field.3 = getelementptr inbounds { double, double }, { double, double }* %tmp.1, i32 0, i32 1
    %.imag.ld.1 = load double, double* %field.3, align 8
    %fadd.1 = fadd double %.imag.ld.0, %.imag.ld.1
    %field.4 = getelementptr inbounds { double, double }, { double, double }* %tmp.2, i32 0, i32 0
    store double %fadd.0, double* %field.4, align 8
    %field.5 = getelementptr inbounds { double, double }, { double, double }* %tmp.2, i32 0, i32 1
    store double %fadd.1, double* %field.5, align 8
    %cast.7 = bitcast { double, double }* %z to i8*
    %cast.8 = bitcast { double, double }* %tmp.2 to i8*
    call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %cast.7, i8* align 8 %cast.8, i64 16, i1 false)
    %cast.9 = bitcast { double, double }* %tmp.3 to i8*
    %cast.10 = bitcast { double, double }* %x to i8*
    call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %cast.9, i8* align 8 %cast.10, i64 16, i1 false)
    %cast.11 = bitcast { double, double }* %tmp.4 to i8*
    %cast.12 = bitcast { double, double }* %y to i8*
    call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %cast.11, i8* align 8 %cast.12, i64 16, i1 false)
    %field.6 = getelementptr inbounds { double, double }, { double, double }* %tmp.3, i32 0, i32 0
    %.real.ld.2 = load double, double* %field.6, align 8
    %field.7 = getelementptr inbounds { double, double }, { double, double }* %tmp.4, i32 0, i32 0
    %.real.ld.3 = load double, double* %field.7, align 8
    %fsub.0 = fsub double %.real.ld.2, %.real.ld.3
    %field.8 = getelementptr inbounds { double, double }, { double, double }* %tmp.3, i32 0, i32 1
    %.imag.ld.2 = load double, double* %field.8, align 8
    %field.9 = getelementptr inbounds { double, double }, { double, double }* %tmp.4, i32 0, i32 1
    %.imag.ld.3 = load double, double* %field.9, align 8
    %fsub.1 = fsub double %.imag.ld.2, %.imag.ld.3
    %field.10 = getelementptr inbounds { double, double }, { double, double }* %tmp.5, i32 0, i32 0
    store double %fsub.0, double* %field.10, align 8
    %field.11 = getelementptr inbounds { double, double }, { double, double }* %tmp.5, i32 0, i32 1
    store double %fsub.1, double* %field.11, align 8
    %cast.13 = bitcast { double, double }* %z to i8*
    %cast.14 = bitcast { double, double }* %tmp.5 to i8*
    call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %cast.13, i8* align 8 %cast.14, i64 16, i1 false)
    %cast.15 = bitcast { double, double }* %tmp.6 to i8*
    %cast.16 = bitcast { double, double }* %x to i8*
    call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %cast.15, i8* align 8 %cast.16, i64 16, i1 false)
    %cast.17 = bitcast { double, double }* %tmp.7 to i8*
    %cast.18 = bitcast { double, double }* %y to i8*
    call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %cast.17, i8* align 8 %cast.18, i64 16, i1 false)
    %field.12 = getelementptr inbounds { double, double }, { double, double }* %tmp.6, i32 0, i32 0
    %.real.ld.4 = load double, double* %field.12, align 8
    %field.13 = getelementptr inbounds { double, double }, { double, double }* %tmp.7, i32 0, i32 0
    %.real.ld.5 = load double, double* %field.13, align 8
    %fmul.0 = fmul double %.real.ld.4, %.real.ld.5
    %field.14 = getelementptr inbounds { double, double }, { double, double }* %tmp.6, i32 0, i32 1
    %.imag.ld.4 = load double, double* %field.14, align 8
    %field.15 = getelementptr inbounds { double, double }, { double, double }* %tmp.7, i32 0, i32 1
    %.imag.ld.5 = load double, double* %field.15, align 8
    %fmul.1 = fmul double %.imag.ld.4, %.imag.ld.5
    %fsub.2 = fsub double %fmul.0, %fmul.1
    %field.16 = getelementptr inbounds { double, double }, { double, double }* %tmp.6, i32 0, i32 0
    %.field.ld.0 = load double, double* %field.16, align 8
    %field.17 = getelementptr inbounds { double, double }, { double, double }* %tmp.7, i32 0, i32 1
    %.field.ld.1 = load double, double* %field.17, align 8
    %fmul.2 = fmul double %.field.ld.0, %.field.ld.1
    %field.18 = getelementptr inbounds { double, double }, { double, double }* %tmp.6, i32 0, i32 1
    %.field.ld.2 = load double, double* %field.18, align 8
    %field.19 = getelementptr inbounds { double, double }, { double, double }* %tmp.7, i32 0, i32 0
    %.field.ld.3 = load double, double* %field.19, align 8
    %fmul.3 = fmul double %.field.ld.2, %.field.ld.3
    %fadd.2 = fadd double %fmul.2, %fmul.3
    %field.20 = getelementptr inbounds { double, double }, { double, double }* %tmp.8, i32 0, i32 0
    store double %fsub.2, double* %field.20, align 8
    %field.21 = getelementptr inbounds { double, double }, { double, double }* %tmp.8, i32 0, i32 1
    store double %fadd.2, double* %field.21, align 8
    %cast.19 = bitcast { double, double }* %z to i8*
    %cast.20 = bitcast { double, double }* %tmp.8 to i8*
    call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %cast.19, i8* align 8 %cast.20, i64 16, i1 false)
    %cast.21 = bitcast { double, double }* %tmp.9 to i8*
    %cast.22 = bitcast { double, double }* %x to i8*
    call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %cast.21, i8* align 8 %cast.22, i64 16, i1 false)
    %cast.23 = bitcast { double, double }* %tmp.10 to i8*
    %cast.24 = bitcast { double, double }* %y to i8*
    call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %cast.23, i8* align 8 %cast.24, i64 16, i1 false)
    %field.22 = getelementptr inbounds { double, double }, { double, double }* %tmp.9, i32 0, i32 0
    %.real.ld.6 = load double, double* %field.22, align 8
    %field.23 = getelementptr inbounds { double, double }, { double, double }* %tmp.10, i32 0, i32 0
    %.real.ld.7 = load double, double* %field.23, align 8
    %fcmp.0 = fcmp oeq double %.real.ld.6, %.real.ld.7
    %zext.0 = zext i1 %fcmp.0 to i8
    %field.24 = getelementptr inbounds { double, double }, { double, double }* %tmp.9, i32 0, i32 1
    %.imag.ld.6 = load double, double* %field.24, align 8
    %field.25 = getelementptr inbounds { double, double }, { double, double }* %tmp.10, i32 0, i32 1
    %.imag.ld.7 = load double, double* %field.25, align 8
    %fcmp.1 = fcmp oeq double %.imag.ld.6, %.imag.ld.7
    %zext.1 = zext i1 %fcmp.1 to i8
    %iand.0 = and i8 %zext.0, %zext.1
    store i8 %iand.0, i8* %b, align 1
    %cast.25 = bitcast { double, double }* %tmp.11 to i8*
    %cast.26 = bitcast { double, double }* %x to i8*
    call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %cast.25, i8* align 8 %cast.26, i64 16, i1 false)
    %cast.27 = bitcast { double, double }* %tmp.12 to i8*
    %cast.28 = bitcast { double, double }* %y to i8*
    call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %cast.27, i8* align 8 %cast.28, i64 16, i1 false)
    %field.26 = getelementptr inbounds { double, double }, { double, double }* %tmp.11, i32 0, i32 0
    %.real.ld.8 = load double, double* %field.26, align 8
    %field.27 = getelementptr inbounds { double, double }, { double, double }* %tmp.12, i32 0, i32 0
    %.real.ld.9 = load double, double* %field.27, align 8
    %fcmp.2 = fcmp une double %.real.ld.8, %.real.ld.9
    %zext.2 = zext i1 %fcmp.2 to i8
    %field.28 = getelementptr inbounds { double, double }, { double, double }* %tmp.11, i32 0, i32 1
    %.imag.ld.8 = load double, double* %field.28, align 8
    %field.29 = getelementptr inbounds { double, double }, { double, double }* %tmp.12, i32 0, i32 1
    %.imag.ld.9 = load double, double* %field.29, align 8
    %fcmp.3 = fcmp une double %.imag.ld.8, %.imag.ld.9
    %zext.3 = zext i1 %fcmp.3 to i8
    %ior.0 = or i8 %zext.2, %zext.3
    store i8 %ior.0, i8* %b, align 1
    ret void
  }
  )RAW_RESULT");

  bool broken = h.finish(StripDebugInfo);
  EXPECT_FALSE(broken && "Module failed to verify.");

  bool isOK = h.expectValue(func->function(), exp);
  EXPECT_TRUE(isOK && "Block does not have expected contents");
}

TEST_P(BackendExprTests, TestComplexExpressions) {
  auto cc = GetParam();
  FcnTestHarness h(cc, "foo");
  Llvm_backend *be = h.be();
  Location loc;

  // var a, b float64
  // var x complex128
  Btype *bf64t = be->float_type(64);
  Btype *bc128t = be->complex_type(128);
  Bvariable *a = h.mkLocal("a", bf64t);
  Bvariable *b = h.mkLocal("b", bf64t);
  Bvariable *x = h.mkLocal("x", bc128t);

  // a = real(x)
  Bexpression *avex1 = be->var_expression(a, loc);
  Bexpression *xvex1 = be->var_expression(x, loc);
  Bexpression *realex = be->real_part_expression(xvex1, loc);
  h.mkAssign(avex1, realex);

  // b = imag(x)
  Bexpression *bvex2 = be->var_expression(b, loc);
  Bexpression *xvex2 = be->var_expression(x, loc);
  Bexpression *imagex = be->imag_part_expression(xvex2, loc);
  h.mkAssign(bvex2, imagex);

  // x = complex(b, a)
  Bexpression *xvex3 = be->var_expression(x, loc);
  Bexpression *bvex3 = be->var_expression(b, loc);
  Bexpression *avex3 = be->var_expression(a, loc);
  Bexpression *compex = be->complex_expression(bvex3, avex3, loc);
  h.mkAssign(xvex3, compex);

  DECLARE_EXPECTED_OUTPUT(exp, R"RAW_RESULT(
    store double 0.000000e+00, double* %a, align 8
    store double 0.000000e+00, double* %b, align 8
    %cast.0 = bitcast { double, double }* %x to i8*
    call addrspace(0) void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %cast.0, i8* align 8 bitcast ({ double, double }* @const.0 to i8*), i64 16, i1 false)
    %field.0 = getelementptr inbounds { double, double }, { double, double }* %x, i32 0, i32 0
    %x.real.ld.0 = load double, double* %field.0, align 8
    store double %x.real.ld.0, double* %a, align 8
    %field.1 = getelementptr inbounds { double, double }, { double, double }* %x, i32 0, i32 1
    %x.imag.ld.0 = load double, double* %field.1, align 8
    store double %x.imag.ld.0, double* %b, align 8
    %b.ld.0 = load double, double* %b, align 8
    %a.ld.0 = load double, double* %a, align 8
    %field.2 = getelementptr inbounds { double, double }, { double, double }* %x, i32 0, i32 0
    store double %b.ld.0, double* %field.2, align 8
    %field.3 = getelementptr inbounds { double, double }, { double, double }* %x, i32 0, i32 1
    store double %a.ld.0, double* %field.3, align 8
  )RAW_RESULT");

  bool isOK = h.expectBlock(exp);
  EXPECT_TRUE(isOK && "Block does not have expected contents");

  bool broken = h.finish(StripDebugInfo);
  EXPECT_FALSE(broken && "Module failed to verify.");
}

TEST_P(BackendExprTests, CreateStringConstantExpressions) {
  auto cc = GetParam();
  FcnTestHarness h(cc, "foo");
  Llvm_backend *be = h.be();

  {
    Bexpression *snil = be->string_constant_expression("");
    DECLARE_EXPECTED_OUTPUT(exp, R"RAW_RESULT(
        i8* null
      )RAW_RESULT");
    bool isOK = h.expectValue(snil->value(), exp);
    EXPECT_TRUE(isOK && "Value does not have expected contents");
  }

  {
    Bexpression *sblah = be->string_constant_expression("blah");
    DECLARE_EXPECTED_OUTPUT(exp, R"RAW_RESULT(
      i8* getelementptr inbounds ([5 x i8], [5 x i8]* @const.0, i32 0, i32 0)
    )RAW_RESULT");
    bool isOK = h.expectValue(sblah->value(), exp);
    EXPECT_TRUE(isOK && "Value does not have expected contents");
  }

  bool broken = h.finish(StripDebugInfo);
  EXPECT_FALSE(broken && "Module failed to verify.");
}

TEST_P(BackendExprTests, TestConditionalExpression1) {
  auto cc = GetParam();
  FcnTestHarness h(cc);
  Llvm_backend *be = h.be();
  BFunctionType *befty1 = mkFuncTyp(be, L_END);
  Bfunction *func = h.mkFunction("foo", befty1);
  Location loc = h.loc();

  // Local vars
  Btype *bi64t = be->integer_type(false, 64);
  Bvariable *pv1 = h.mkLocal("a", bi64t);
  Bvariable *pv2 = h.mkLocal("b", bi64t);

  // Two calls, no type
  Bexpression *call1 = h.mkCallExpr(be, func, nullptr);
  Bexpression *call2 = h.mkCallExpr(be, func, nullptr);
  Bexpression *vex1 = be->var_expression(pv1, loc);
  Bexpression *vex2 = be->var_expression(pv2, loc);
  Bexpression *cmp = be->binary_expression(OPERATOR_LT, vex1, vex2, loc);
  Bexpression *condex = be->conditional_expression(func, nullptr, cmp, call1,
                                                   call2, loc);
  h.mkExprStmt(condex);

  DECLARE_EXPECTED_OUTPUT(exp, R"RAW_RESULT(
    define void @foo(i8* nest %nest.0) #0 {
  entry:
    %a = alloca i64, align 8
    %b = alloca i64, align 8
    store i64 0, i64* %a, align 8
    store i64 0, i64* %b, align 8
    %a.ld.0 = load i64, i64* %a, align 8
    %b.ld.0 = load i64, i64* %b, align 8
    %icmp.0 = icmp slt i64 %a.ld.0, %b.ld.0
    %zext.0 = zext i1 %icmp.0 to i8
    %trunc.0 = trunc i8 %zext.0 to i1
    br i1 %trunc.0, label %then.0, label %else.0
  
  then.0:                                           ; preds = %entry
    call void @foo(i8* nest undef)
    br label %fallthrough.0
  
  fallthrough.0:                                    ; preds = %else.0, %then.0
    ret void
  
  else.0:                                           ; preds = %entry
    call void @foo(i8* nest undef)
    br label %fallthrough.0
  }
  )RAW_RESULT");

  bool broken = h.finish(StripDebugInfo);
  EXPECT_FALSE(broken && "Module failed to verify.");

  bool isOK = h.expectValue(func->function(), exp);
  EXPECT_TRUE(isOK && "Block does not have expected contents");
}

TEST_P(BackendExprTests, TestConditionalExpression2) {
  auto cc = GetParam();
  FcnTestHarness h(cc);
  Llvm_backend *be = h.be();
  BFunctionType *befty1 = mkFuncTyp(be, L_END);
  Bfunction *func = h.mkFunction("foo", befty1);
  Location loc;

  // Local vars
  Btype *bi64t = be->integer_type(false, 64);
  Bvariable *pv1 = h.mkLocal("a", bi64t);

  // Call on true branch,
  Bexpression *call1 = h.mkCallExpr(be, func, nullptr);
  Bexpression *ve = be->var_expression(pv1, loc);
  Bexpression *cmp = be->binary_expression(OPERATOR_LT,
                                           mkInt64Const(be, int64_t(3)),
                                          mkInt64Const(be, int64_t(4)), loc);
  Bexpression *condex = be->conditional_expression(func, ve->btype(),
                                                   cmp, call1,
                                                   ve, loc);
  h.mkExprStmt(condex);

  DECLARE_EXPECTED_OUTPUT(exp, R"RAW_RESULT(
    define void @foo(i8* nest %nest.0) #0 {
  entry:
    %a = alloca i64, align 8
    %tmpv.0 = alloca i64, align 8
    store i64 0, i64* %a, align 8
    br i1 true, label %then.0, label %else.0
  
  then.0:                                           ; preds = %entry
    call void @foo(i8* nest undef)
    br label %fallthrough.0
  
  fallthrough.0:                                    ; preds = %else.0, %then.0
    %tmpv.0.ld.0 = load i64, i64* %tmpv.0, align 8
    ret void
  
  else.0:                                           ; preds = %entry
    %a.ld.0 = load i64, i64* %a, align 8
    store i64 %a.ld.0, i64* %tmpv.0, align 8
    br label %fallthrough.0
  }
  )RAW_RESULT");

  bool broken = h.finish(StripDebugInfo);
  EXPECT_FALSE(broken && "Module failed to verify.");

  bool isOK = h.expectValue(func->function(), exp);
  EXPECT_TRUE(isOK && "Block does not have expected contents");
}

TEST(BackendExprTests, TestConditionalExpression3Amd64) {
  FcnTestHarness h(llvm::CallingConv::X86_64_SysV);
  Llvm_backend *be = h.be();
  Btype *bi32t = be->integer_type(false, 32);
  Btype *abt = be->array_type(bi32t, mkInt64Const(be, int64_t(16)));
  Btype *s2t = mkBackendStruct(be, abt, "f1", bi32t, "f2", nullptr);
  BFunctionType *befty1 = mkFuncTyp(be,
                                    L_RES, s2t,
                                    L_PARM, s2t,
                                    L_PARM, bi32t, L_END);
  Bfunction *func = h.mkFunction("foo", befty1);
  Location loc;

  // Local var with conditional expression as init
  Bvariable *p0v = func->getNthParamVar(0);
  Bvariable *p1v = func->getNthParamVar(1);
  Bexpression *vep1 = be->var_expression(p1v, loc);
  Bexpression *cmp = be->binary_expression(OPERATOR_LT, vep1,
                                           mkInt32Const(be, int32_t(7)), loc);
  Bexpression *vep0 = be->var_expression(p0v, loc);
  Bexpression *bzero = be->zero_expression(s2t);

  Bexpression *cond = be->conditional_expression(func, s2t, cmp,
                                                 vep0, bzero, loc);
  h.mkLocal("a", s2t, cond);

  DECLARE_EXPECTED_OUTPUT(exp, R"RAW_RESULT(
    define void @foo({ [16 x i32], i32 }* sret({ [16 x i32], i32 }) %sret.formal.0, i8* nest %nest.0, { [16 x i32], i32 }* byval({ [16 x i32], i32 }) %p0, i32 %p1) #0 {
  entry:
    %p1.addr = alloca i32, align 4
    %a = alloca { [16 x i32], i32 }, align 4
    %tmpv.0 = alloca { [16 x i32], i32 }, align 4
    store i32 %p1, i32* %p1.addr, align 4
    %p1.ld.0 = load i32, i32* %p1.addr, align 4
    %icmp.0 = icmp slt i32 %p1.ld.0, 7
    %zext.0 = zext i1 %icmp.0 to i8
    %trunc.0 = trunc i8 %zext.0 to i1
    br i1 %trunc.0, label %then.0, label %else.0
  
  then.0:                                           ; preds = %entry
    %cast.0 = bitcast { [16 x i32], i32 }* %tmpv.0 to i8*
    %cast.1 = bitcast { [16 x i32], i32 }* %p0 to i8*
    call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 4 %cast.0, i8* align 4 %cast.1, i64 68, i1 false)
    br label %fallthrough.0
  
  fallthrough.0:                                    ; preds = %else.0, %then.0
    %cast.3 = bitcast { [16 x i32], i32 }* %a to i8*
    %cast.4 = bitcast { [16 x i32], i32 }* %tmpv.0 to i8*
    call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 4 %cast.3, i8* align 4 %cast.4, i64 68, i1 false)
    ret void
  
  else.0:                                           ; preds = %entry
    %cast.2 = bitcast { [16 x i32], i32 }* %tmpv.0 to i8*
    call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 4 %cast.2, i8* align 4 bitcast ({ [16 x i32], i32 }* @const.0 to i8*), i64 68, i1 false)
    br label %fallthrough.0
  }
  )RAW_RESULT");

  bool broken = h.finish(StripDebugInfo);
  EXPECT_FALSE(broken && "Module failed to verify.");

  bool isOK = h.expectValue(func->function(), exp);
  EXPECT_TRUE(isOK && "Block does not have expected contents");
}

TEST(BackendExprTests, TestConditionalExpression3Arm64) {
  FcnTestHarness h(llvm::CallingConv::ARM_AAPCS);
  Llvm_backend *be = h.be();
  Btype *bi32t = be->integer_type(false, 32);
  Btype *abt = be->array_type(bi32t, mkInt64Const(be, int64_t(16)));
  // type s2t struct {
  //   f1 [16]int32
  //   f2 int32
  // }
  Btype *s2t = mkBackendStruct(be, abt, "f1", bi32t, "f2", nullptr);

  // func foo(p0v s2t, p1v int32) (a s2t)
  BFunctionType *befty1 =
      mkFuncTyp(be, L_RES, s2t, L_PARM, s2t, L_PARM, bi32t, L_END);
  Bfunction *func = h.mkFunction("foo", befty1);
  Location loc;

  // Local var with conditional expression as init
  Bvariable *p0v = func->getNthParamVar(0);
  Bvariable *p1v = func->getNthParamVar(1);
  Bexpression *vep1 = be->var_expression(p1v, loc);
  // p1v < 7
  Bexpression *cmp = be->binary_expression(OPERATOR_LT, vep1,
                                           mkInt32Const(be, int32_t(7)), loc);
  Bexpression *vep0 = be->var_expression(p0v, loc);
  Bexpression *bzero = be->zero_expression(s2t);

  // if (p1v < 7) {
  //   a = p0v
  // } else {
  //   a = nil
  // }
  Bexpression *cond =
      be->conditional_expression(func, s2t, cmp, vep0, bzero, loc);
  h.mkLocal("a", s2t, cond);

  DECLARE_EXPECTED_OUTPUT(exp, R"RAW_RESULT(
    define void @foo({ [16 x i32], i32 }* sret({ [16 x i32], i32 }) %sret.formal.0, i8* nest %nest.0, { [16 x i32], i32 }* %p0, i32 %p1) #0 {
  entry:
    %p1.addr = alloca i32, align 4
    %a = alloca { [16 x i32], i32 }, align 4
    %tmpv.0 = alloca { [16 x i32], i32 }, align 4
    store i32 %p1, i32* %p1.addr, align 4
    %p1.ld.0 = load i32, i32* %p1.addr, align 4
    %icmp.0 = icmp slt i32 %p1.ld.0, 7
    %zext.0 = zext i1 %icmp.0 to i8
    %trunc.0 = trunc i8 %zext.0 to i1
    br i1 %trunc.0, label %then.0, label %else.0
  
  then.0:                                           ; preds = %entry
    %cast.0 = bitcast { [16 x i32], i32 }* %tmpv.0 to i8*
    %cast.1 = bitcast { [16 x i32], i32 }* %p0 to i8*
    call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 4 %cast.0, i8* align 4 %cast.1, i64 68, i1 false)
    br label %fallthrough.0
  
  fallthrough.0:                                    ; preds = %else.0, %then.0
    %cast.3 = bitcast { [16 x i32], i32 }* %a to i8*
    %cast.4 = bitcast { [16 x i32], i32 }* %tmpv.0 to i8*
    call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 4 %cast.3, i8* align 4 %cast.4, i64 68, i1 false)
    ret void
  
  else.0:                                           ; preds = %entry
    %cast.2 = bitcast { [16 x i32], i32 }* %tmpv.0 to i8*
    call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 4 %cast.2, i8* align 4 bitcast ({ [16 x i32], i32 }* @const.0 to i8*), i64 68, i1 false)
    br label %fallthrough.0
  }
  )RAW_RESULT");

  bool broken = h.finish(StripDebugInfo);
  EXPECT_FALSE(broken && "Module failed to verify.");

  bool isOK = h.expectValue(func->function(), exp);
  EXPECT_TRUE(isOK && "Block does not have expected contents");
}

TEST_P(BackendExprTests, TestCompoundExpression) {
  auto cc = GetParam();
  FcnTestHarness h(cc, "foo");
  Llvm_backend *be = h.be();
  Bfunction *func = h.func();
  Location loc;

  // var x int64 = 0
  // x = 5
  // x
  Btype *bi64t = be->integer_type(false, 64);
  Bvariable *xv = h.mkLocal("x", bi64t);
  Bexpression *vex = be->var_expression(xv, loc);
  Bstatement *st = be->assignment_statement(func, vex,
                                            mkInt64Const(be, 5), loc);
  Bexpression *vex2 = be->var_expression(xv, loc);
  Bexpression *ce = be->compound_expression(st, vex2, loc);
  Bstatement *es = be->expression_statement(func, ce);
  h.addStmt(es);

  DECLARE_EXPECTED_OUTPUT(exp, R"RAW_RESULT(
    define i64 @foo(i8* nest %nest.0, i32 %param1, i32 %param2, i64* %param3) #0 {
  entry:
    %param1.addr = alloca i32, align 4
    %param2.addr = alloca i32, align 4
    %param3.addr = alloca i64*, align 8
    %x = alloca i64, align 8
    store i32 %param1, i32* %param1.addr, align 4
    store i32 %param2, i32* %param2.addr, align 4
    store i64* %param3, i64** %param3.addr, align 8
    store i64 0, i64* %x, align 8
    store i64 5, i64* %x, align 8
    %x.ld.0 = load i64, i64* %x, align 8
    ret i64 0
  }
  )RAW_RESULT");

  bool broken = h.finish(StripDebugInfo);
  EXPECT_FALSE(broken && "Module failed to verify.");

  // Note that this
  bool isOK = h.expectValue(func->function(), exp);
  EXPECT_TRUE(isOK && "Function does not have expected contents");
}

TEST_P(BackendExprTests, TestCompoundExpression2) {
  auto cc = GetParam();
  FcnTestHarness h(cc, "foo");
  Llvm_backend *be = h.be();
  Bfunction *func = h.func();
  Location loc;

  // var x int64
  // var y = (x = 5; struct{int64; int64}{x, x})
  Btype *bi64t = be->integer_type(false, 64);
  Bvariable *xv = h.mkLocal("x", bi64t);
  Bexpression *vex = be->var_expression(xv, loc);
  Bstatement *st = be->assignment_statement(func, vex,
                                            mkInt64Const(be, 5), loc);
  Bexpression *vex1 = be->var_expression(xv, loc);
  Bexpression *vex2 = be->var_expression(xv, loc);
  Btype *bst = mkTwoFieldStruct(be, bi64t, bi64t);
  Bexpression *sce = be->constructor_expression(bst, {vex1, vex2}, loc);
  Bexpression *ce = be->compound_expression(st, sce, loc);

  Bvariable *yv = h.mkLocal("y", bst);
  Bexpression *yvex = be->var_expression(yv, loc);
  Bstatement *st2 = be->assignment_statement(func, yvex, ce, loc);
  h.addStmt(st2);

  DECLARE_EXPECTED_OUTPUT(exp, R"RAW_RESULT(
    define i64 @foo(i8* nest %nest.0, i32 %param1, i32 %param2, i64* %param3) #0 {
  entry:
    %tmp.0 = alloca { i64, i64 }, align 8
    %param1.addr = alloca i32, align 4
    %param2.addr = alloca i32, align 4
    %param3.addr = alloca i64*, align 8
    %x = alloca i64, align 8
    %y = alloca { i64, i64 }, align 8
    store i32 %param1, i32* %param1.addr, align 4
    store i32 %param2, i32* %param2.addr, align 4
    store i64* %param3, i64** %param3.addr, align 8
    store i64 0, i64* %x, align 8
    %cast.0 = bitcast { i64, i64 }* %y to i8*
    call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %cast.0, i8* align 8 bitcast ({ i64, i64 }* @const.0 to i8*), i64 16, i1 false)
    store i64 5, i64* %x, align 8
    %x.ld.0 = load i64, i64* %x, align 8
    %x.ld.1 = load i64, i64* %x, align 8
    %field.0 = getelementptr inbounds { i64, i64 }, { i64, i64 }* %tmp.0, i32 0, i32 0
    store i64 %x.ld.0, i64* %field.0, align 8
    %field.1 = getelementptr inbounds { i64, i64 }, { i64, i64 }* %tmp.0, i32 0, i32 1
    store i64 %x.ld.1, i64* %field.1, align 8
    %cast.1 = bitcast { i64, i64 }* %y to i8*
    %cast.2 = bitcast { i64, i64 }* %tmp.0 to i8*
    call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %cast.1, i8* align 8 %cast.2, i64 16, i1 false)
    ret i64 0
  }
  )RAW_RESULT");

  bool broken = h.finish(StripDebugInfo);
  EXPECT_FALSE(broken && "Module failed to verify.");

  // Note that this
  bool isOK = h.expectValue(func->function(), exp);
  EXPECT_TRUE(isOK && "Function does not have expected contents");
}

TEST_P(BackendExprTests, TestLhsConditionalExpression) {
  auto cc = GetParam();
  FcnTestHarness h(cc);
  Llvm_backend *be = h.be();
  Btype *bi32t = be->integer_type(false, 32);
  BFunctionType *befty1 = mkFuncTyp(be,
                            L_PARM, be->pointer_type(bi32t),
                            L_PARM, be->pointer_type(bi32t),
                            L_END);
  Bfunction *func = h.mkFunction("foo", befty1);
  Location loc;

  // *(p0 == nil ? p1 : p0) = 7
  Bvariable *p0v = func->getNthParamVar(0);
  Bvariable *p1v = func->getNthParamVar(1);
  Bexpression *vex = be->var_expression(p0v, loc);
  Bexpression *npe = be->nil_pointer_expression();
  Bexpression *cmp = be->binary_expression(OPERATOR_EQEQ, vex, npe, loc);
  Bexpression *ver0 = be->var_expression(p0v, loc);
  Bexpression *ver1 = be->var_expression(p1v, loc);
  Bexpression *guard = be->conditional_expression(func, be->pointer_type(bi32t), cmp,
                                                  ver1, ver0, loc);
  Bexpression *dex = be->indirect_expression(bi32t, guard, false, loc);
  h.mkAssign(dex, mkInt32Const(be, 7));

  DECLARE_EXPECTED_OUTPUT(exp, R"RAW_RESULT(
    define void @foo(i8* nest %nest.0, i32* %p0, i32* %p1) #0 {
  entry:
    %p0.addr = alloca i32*, align 8
    %p1.addr = alloca i32*, align 8
    %tmpv.0 = alloca i32*, align 8
    store i32* %p0, i32** %p0.addr, align 8
    store i32* %p1, i32** %p1.addr, align 8
    %p0.ld.0 = load i32*, i32** %p0.addr, align 8
    %icmp.0 = icmp eq i32* %p0.ld.0, null
    %zext.0 = zext i1 %icmp.0 to i8
    %trunc.0 = trunc i8 %zext.0 to i1
    br i1 %trunc.0, label %then.0, label %else.0
  
  then.0:                                           ; preds = %entry
    %p1.ld.0 = load i32*, i32** %p1.addr, align 8
    store i32* %p1.ld.0, i32** %tmpv.0, align 8
    br label %fallthrough.0
  
  fallthrough.0:                                    ; preds = %else.0, %then.0
    %tmpv.0.ld.0 = load i32*, i32** %tmpv.0, align 8
    store i32 7, i32* %tmpv.0.ld.0, align 4
    ret void
  
  else.0:                                           ; preds = %entry
    %p0.ld.1 = load i32*, i32** %p0.addr, align 8
    store i32* %p0.ld.1, i32** %tmpv.0, align 8
    br label %fallthrough.0
  }
  )RAW_RESULT");

  bool broken = h.finish(StripDebugInfo);
  EXPECT_FALSE(broken && "Module failed to verify.");

  // Note that this
  bool isOK = h.expectValue(func->function(), exp);
  EXPECT_TRUE(isOK && "Function does not have expected contents");
}

TEST_P(BackendExprTests, TestUnaryExpression) {
  auto cc = GetParam();
  FcnTestHarness h(cc, "foo");
  Llvm_backend *be = h.be();
  Location loc;

  // var x bool
  // var y bool = !x
  Btype *bt = be->bool_type();
  Bvariable *xv = h.mkLocal("x", bt);
  Bexpression *vex = be->var_expression(xv, loc);
  h.mkLocal("y", bt, be->unary_expression(OPERATOR_NOT, vex, loc));

  // var a i32
  // var b i32 = -a
  Btype *bi32t = be->integer_type(false, 32);
  Bvariable *av = h.mkLocal("a", bi32t);
  Bexpression *vea = be->var_expression(av, loc);
  h.mkLocal("b", bi32t, be->unary_expression(OPERATOR_MINUS, vea, loc));

  // var z i64
  // var w i64 = ^z
  Btype *bi64t = be->integer_type(false, 64);
  Bvariable *zv = h.mkLocal("z", bi64t);
  Bexpression *vez = be->var_expression(zv, loc);
  h.mkLocal("w", bi64t, be->unary_expression(OPERATOR_XOR, vez, loc));

  // var q float64
  // var r float64 = -z
  Btype *bf64t = be->float_type(64);
  Bvariable *qv = h.mkLocal("q", bf64t);
  Bexpression *veq = be->var_expression(qv, loc);
  h.mkLocal("r", bf64t, be->unary_expression(OPERATOR_MINUS, veq, loc));

  DECLARE_EXPECTED_OUTPUT(exp, R"RAW_RESULT(
    store i8 0, i8* %x, align 1
    %x.ld.0 = load i8, i8* %x, align 1
    %icmp.0 = icmp ne i8 %x.ld.0, 0
    %xor.0 = xor i1 %icmp.0, true
    %zext.0 = zext i1 %xor.0 to i8
    store i8 %zext.0, i8* %y, align 1
    store i32 0, i32* %a, align 4
    %a.ld.0 = load i32, i32* %a, align 4
    %sub.0 = sub i32 0, %a.ld.0
    store i32 %sub.0, i32* %b, align 4
    store i64 0, i64* %z, align 8
    %z.ld.0 = load i64, i64* %z, align 8
    %xor.1 = xor i64 %z.ld.0, -1
    store i64 %xor.1, i64* %w, align 8
    store double 0.000000e+00, double* %q, align 8
    %q.ld.0 = load double, double* %q, align 8
    %fsub.0 = fsub double -0.000000e+00, %q.ld.0
    store double %fsub.0, double* %r, align 8
  )RAW_RESULT");

  bool isOK = h.expectBlock(exp);
  EXPECT_TRUE(isOK && "Block does not have expected contents");

  bool broken = h.finish(StripDebugInfo);
  EXPECT_FALSE(broken && "Module failed to verify.");
}

TEST_P(BackendExprTests, TestCallArgConversions) {
  auto cc = GetParam();
  FcnTestHarness h(cc);
  Llvm_backend *be = h.be();
  Btype *bi8t = be->integer_type(false, 8);
  Btype *bi32t = be->integer_type(false, 32);
  Btype *bi64t = be->integer_type(false, 64);
  BFunctionType *befty1 = mkFuncTyp(be,
                                    L_PARM, be->pointer_type(bi8t),
                                    L_PARM, be->pointer_type(bi32t),
                                    L_PARM, be->pointer_type(bi64t),
                                    L_END);
  Bfunction *func = h.mkFunction("foo", befty1);
  Location loc;

  Bexpression *nil = be->nil_pointer_expression();
  Bexpression *call1 = h.mkCallExpr(be, func, nil, nil, nil, nullptr);
  h.mkExprStmt(call1);

  DECLARE_EXPECTED_OUTPUT(exp, R"RAW_RESULT(
    call addrspace(0) void @foo(i8* nest undef, i8* null, i32* null, i64* null)
  )RAW_RESULT");

  // Note that this
  bool isOK = h.expectBlock(exp);
  EXPECT_TRUE(isOK && "Block does not have expected contents");

  bool broken = h.finish(StripDebugInfo);
  EXPECT_FALSE(broken && "Module failed to verify.");
}

TEST_P(BackendExprTests, TestStringDuplication) {
  auto cc = GetParam();
  FcnTestHarness h(cc, "foo");
  Llvm_backend *be = h.be();

  Bexpression *bst = be->string_constant_expression("abc");
  Bexpression *bst2 = be->string_constant_expression("def");
  Bexpression *bst3 = be->string_constant_expression("abc");
  EXPECT_NE(bst->value(), bst2->value());
  EXPECT_EQ(bst->value(), bst3->value());

  bool broken = h.finish(StripDebugInfo);
  EXPECT_FALSE(broken && "Module failed to verify.");
}

TEST_P(BackendExprTests, TestImmutableStructReferenceDuplication) {
  auto cc = GetParam();
  FcnTestHarness h(cc, "foo");
  Llvm_backend *be = h.be();
  Location loc = h.loc();

  Btype *bi32t = be->integer_type(false, 32);
  Btype *st = mkBackendStruct(be, bi32t, "f1", nullptr);
  Bvariable *v1 = be->immutable_struct_reference("foo", "", st, loc);
  Bvariable *v2 = be->immutable_struct_reference("bar", "", st, loc);
  Bvariable *v3 = be->immutable_struct_reference("", "foo", st, loc);
  EXPECT_EQ(v1, v3);
  EXPECT_NE(v1, v2);

  bool broken = h.finish(StripDebugInfo);
  EXPECT_FALSE(broken && "Module failed to verify.");
}

} // namespace
