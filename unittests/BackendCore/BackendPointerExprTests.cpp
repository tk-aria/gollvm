//=- llvm/tools/gollvm/unittests/BackendCore/BackendPointeerExprTests.cpp -=//
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

TEST(BackEndPointerExprTests, TestAddrAndIndirection) {
  FcnTestHarness h("foo");
  Llvm_backend *be = h.be();
  Bfunction *func = h.func();

  // var y int64 = 10
  Btype *bi64t = be->integer_type(false, 64);
  Location loc;
  Bvariable *y = h.mkLocal("y", bi64t, mkInt64Const(be, 10));

  // var x *int64 = nil
  Btype *bpi64t = be->pointer_type(bi64t);
  Bvariable *x = h.mkLocal("x", bpi64t);

  {
    // x = &y
    Bexpression *vex = be->var_expression(x, loc);
    Bexpression *vey = be->var_expression(y, loc);
    Bexpression *ady = be->address_expression(vey, loc);
    Bstatement *as = be->assignment_statement(func, vex, ady, loc);
    h.addStmt(as);
  }

  {
    // y = *x
    Bexpression *vey = be->var_expression(y, loc);
    Bexpression *vex = be->var_expression(x, loc);
    bool knValid = false;
    Bexpression *indx1 = be->indirect_expression(bi64t, vex, knValid, loc);
    Bstatement *as = be->assignment_statement(func, vey, indx1, loc);
    h.addStmt(as);
  }

  {
    // *x = 3
    Bexpression *vex = be->var_expression(x, loc);
    Bexpression *indx = be->indirect_expression(bi64t, vex, false, loc);
    Bexpression *beic3 = mkInt64Const(be, 3);
    Bstatement *as = be->assignment_statement(func, indx, beic3, loc);
    h.addStmt(as);
  }

  const char *exp = R"RAW_RESULT(
    store i64 10, i64* %y
    store i64* null, i64** %x
    store i64* %y, i64** %x
    %x.ld.0 = load i64*, i64** %x
    %.ld.0 = load i64, i64* %x.ld.0
    store i64 %.ld.0, i64* %y
    %x.ld.1 = load i64*, i64** %x
    store i64 3, i64* %x.ld.1
    )RAW_RESULT";

  bool isOK = h.expectBlock(exp);
  EXPECT_TRUE(isOK && "Block does not have expected contents");

  bool broken = h.finish(PreserveDebugInfo);
  EXPECT_FALSE(broken && "Module failed to verify.");
}

TEST(BackEndPointerExprTests, CreateFunctionCodeExpression) {

  FcnTestHarness h("foo");
  Llvm_backend *be = h.be();
  Bfunction *func = h.func();
  Location loc;

  // Local variables of pointer-to-function-descriptor type. A key
  // item to note here is that we want to verify that the backend methods
  // allow flexibility in terms of the concrete LLVM type for the
  // function descriptor. The FE sometimes creates a struct with a
  // uintptr field, and sometimes a struct with a function pointer field.

  // Function descriptor type with uintptr, e.g. { i64 }
  Btype *fdesct1 = mkFuncDescType(be);

  // Function descriptor type with function pointer, e.g. { i64 (i64, ...) }
  Btype *fdesct2 = mkBackendStruct(be, func->fcnType(), "f1", nullptr);

  // Function descriptor variable
  Bvariable *bfdv1 = h.mkLocal("fdloc1", fdesct1, mkFuncDescExpr(be, func));

  // Pointer-to-FD variables
  Btype *pfd1t = be->pointer_type(fdesct1);
  Btype *pfd2t = be->pointer_type(fdesct2);
  Bexpression *vex1 = be->var_expression(bfdv1, loc);
  Bexpression *adfd1 = be->address_expression(vex1, loc);
  Bvariable *bfpv1 = h.mkLocal("fploc1", pfd1t, adfd1);
  Bvariable *bfpv2 = h.mkLocal("fploc2", pfd2t);

  // Assignment of function descriptor pointer values. Note that the
  // types here are not going to agree strictly; this test verifies
  // that this flexibility is allowed.
  Bexpression *vex2 = be->var_expression(bfpv2, loc);
  Bexpression *rvex2 = be->var_expression(bfpv1, loc);
  h.mkAssign(vex2, rvex2);
  Bexpression *vex3 = be->var_expression(bfpv1, loc);
  Bexpression *rvex3 = be->var_expression(bfpv2, loc);
  h.mkAssign(vex3, rvex3);

  const char *exp = R"RAW_RESULT(
  %cast.0 = bitcast { i64 }* %fdloc1 to i8*
  %cast.1 = bitcast { i64 }* @const.0 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %cast.0, i8* align 8 %cast.1, i64 8, i1 false)
  store { i64 }* %fdloc1, { i64 }** %fploc1
  store { i64 (i8*, i32, i32, i64*)* }* null, { i64 (i8*, i32, i32, i64*)* }** %fploc2
  %fploc1.ld.0 = load { i64 }*, { i64 }** %fploc1
  %cast.2 = bitcast { i64 }* %fploc1.ld.0 to { i64 (i8*, i32, i32, i64*)* }*
  store { i64 (i8*, i32, i32, i64*)* }* %cast.2, { i64 (i8*, i32, i32, i64*)* }** %fploc2
  %fploc2.ld.0 = load { i64 (i8*, i32, i32, i64*)* }*, { i64 (i8*, i32, i32, i64*)* }** %fploc2
  %cast.3 = bitcast { i64 (i8*, i32, i32, i64*)* }* %fploc2.ld.0 to { i64 }*
  store { i64 }* %cast.3, { i64 }** %fploc1
  )RAW_RESULT";

  bool isOK = h.expectBlock(exp);
  EXPECT_TRUE(isOK && "Block does not have expected contents");

  bool broken = h.finish(PreserveDebugInfo);
  EXPECT_FALSE(broken && "Module failed to verify.");
}

TEST(BackEndPointerExprTests, CreateNilPointerExpression) {

  FcnTestHarness h("foo");
  Llvm_backend *be = h.be();

  // Manufacture a nil pointer expression
  Bexpression *npe = be->nil_pointer_expression();
  const char *exp1 = R"RAW_RESULT(
    i64* null
  )RAW_RESULT";
  bool isOK = h.expectValue(npe->value(), exp1);
  EXPECT_TRUE(isOK && "Value does not have expected contents");

  // Expressions involving nil pointers.
  Location loc;
  Btype *bt = be->bool_type();
  Btype *pbt = be->pointer_type(bt);
  Bvariable *b1 = h.mkLocal("b1", bt);
  Bvariable *pb1 = h.mkLocal("pb1", pbt);

  {
    // b1 = (pb1 == nil)
    Bexpression *vel = be->var_expression(b1, loc);
    Bexpression *ver = be->var_expression(pb1, loc);
    Bexpression *npe = be->nil_pointer_expression();
    Bexpression *cmp = be->binary_expression(OPERATOR_EQEQ, ver, npe, loc);
    h.mkAssign(vel, cmp);
  }

  {
    // b1 = (nil == pb1)
    Bexpression *vel = be->var_expression(b1, loc);
    Bexpression *ver = be->var_expression(pb1, loc);
    Bexpression *npe = be->nil_pointer_expression();
    Bexpression *cmp = be->binary_expression(OPERATOR_EQEQ, npe, ver, loc);
    h.mkAssign(vel, cmp);
  }

  const char *exp2 = R"RAW_RESULT(
      store i8 0, i8* %b1
      store i8* null, i8** %pb1
      %pb1.ld.0 = load i8*, i8** %pb1
      %icmp.0 = icmp eq i8* %pb1.ld.0, null
      %zext.0 = zext i1 %icmp.0 to i8
      store i8 %zext.0, i8* %b1
      %pb1.ld.1 = load i8*, i8** %pb1
      %icmp.1 = icmp eq i8* null, %pb1.ld.1
      %zext.1 = zext i1 %icmp.1 to i8
      store i8 %zext.1, i8* %b1
    )RAW_RESULT";

  bool isOK2 = h.expectBlock(exp2);
  EXPECT_TRUE(isOK2 && "Block does not have expected contents");

  bool broken = h.finish(PreserveDebugInfo);
  EXPECT_FALSE(broken && "Module failed to verify.");
}

TEST(BackEndPointerExprTests, TestDerefNilPointer) {
  FcnTestHarness h("foo");
  Llvm_backend *be = h.be();
  Location loc;

  Btype *bi32t = be->integer_type(false, 32);
  Bexpression *npe = be->nil_pointer_expression();
  Bexpression *deref = be->indirect_expression(bi32t, npe, false, loc);
  h.mkLocal("x", bi32t, deref);

  // Aggregate type
  Btype *bst = mkTwoFieldStruct(be, bi32t, bi32t);
  Bexpression *npe2 = be->nil_pointer_expression();
  Bexpression *deref2 = be->indirect_expression(bst, npe2, false, loc);
  h.mkLocal("y", bst, deref2);

  const char *exp = R"RAW_RESULT(
    %deref.ld.0 = load i32, i32* null
    store i32 %deref.ld.0, i32* %x
    %cast.2 = bitcast { i32, i32 }* %y to i8*
    %cast.3 = bitcast { i32, i32 }* null to i8*
    call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 4 %cast.2, i8* align 4 %cast.3, i64 8, i1 false)
  )RAW_RESULT";

  bool isOK = h.expectBlock(exp);
  EXPECT_TRUE(isOK && "Block does not have expected contents");

  bool broken = h.finish(PreserveDebugInfo);
  EXPECT_FALSE(broken && "Module failed to verify.");
}

TEST(BackEndPointerExprTests, CircularPointerExpressions1) {

  // This testpoint is intended to verify handling of expressions
  // involving circular pointer types. Go code:
  //
  //  type p *p
  //  func foo() {
  //     var cpv1, cpv2 p
  //     cpv1 = &cpv2
  //     cpv2 = &cpv1
  //     b1 := (cpv1 == *cpv2)
  //     b2 := (&cpv1 != cpv2)
  //     b3 := (&cpv1 == ***cpv2)

  FcnTestHarness h("foo");
  Llvm_backend *be = h.be();
  Location loc;

  // Create circular pointer type
  Btype *pht = be->placeholder_pointer_type("ph", loc, false);
  Btype *cpt = be->circular_pointer_type(pht, false);
  Btype *pcpt = be->pointer_type(cpt);
  be->set_placeholder_pointer_type(pht, pcpt);
  EXPECT_EQ(pht->type(), cpt->type());

  // Local vars
  Bvariable *cpv1 = h.mkLocal("cpv1", pht);
  Bvariable *cpv2 = h.mkLocal("cpv2", pht);

  {
    // cpv1 = &cpv2
    Bexpression *ve1 = be->var_expression(cpv1, loc);
    Bexpression *ve2 = be->var_expression(cpv2, loc);
    Bexpression *adx = be->address_expression(ve2, loc);
    h.mkAssign(ve1, adx);
  }

  {
    // cpv2 = &cpv1
    Bexpression *ve1 = be->var_expression(cpv2, loc);
    Bexpression *ve2 = be->var_expression(cpv1, loc);
    Bexpression *adx = be->address_expression(ve2, loc);
    h.mkAssign(ve1, adx);
  }

  Btype *bt = be->bool_type();
  Bvariable *b1 = h.mkLocal("b1", bt);
  Bvariable *b2 = h.mkLocal("b2", bt);
  Bvariable *b3 = h.mkLocal("b3", bt);

  {
    // b1 := (cpv1 == *cpv2)
    Bexpression *ve0 = be->var_expression(b1, loc);
    Bexpression *ve1 = be->var_expression(cpv1, loc);
    Bexpression *ve2 = be->var_expression(cpv2, loc);
    Bexpression *dex = be->indirect_expression(pht, ve2, false, loc);
    Bexpression *cmp = be->binary_expression(OPERATOR_EQEQ, ve1, dex, loc);
    h.mkAssign(ve0, cmp);
  }

  {
    // b2 := (&cpv1 != cpv2)
    Bexpression *ve0 = be->var_expression(b2, loc);
    Bexpression *ve1 = be->var_expression(cpv1, loc);
    Bexpression *adx = be->address_expression(ve1, loc);
    Bexpression *ve2 = be->var_expression(cpv2, loc);
    Bexpression *cmp = be->binary_expression(OPERATOR_EQEQ, adx, ve2, loc);
    h.mkAssign(ve0, cmp);
  }

  {
    // b3 := (cpv1 == ***cpv2)
    Bexpression *ve0 = be->var_expression(b3, loc);
    Bexpression *ve1 = be->var_expression(cpv1, loc);
    Bexpression *ve2 = be->var_expression(cpv2, loc);
    Bexpression *dex1 = be->indirect_expression(pht, ve2, false, loc);
    Bexpression *dex2 = be->indirect_expression(pht, dex1, false, loc);
    Bexpression *dex3 = be->indirect_expression(pht, dex2, false, loc);
    Bexpression *cmp = be->binary_expression(OPERATOR_EQEQ, ve1, dex3, loc);
    h.mkAssign(ve0, cmp);
  }

  const char *exp = R"RAW_RESULT(
  store %CPT.0* null, %CPT.0** %cpv1
  store %CPT.0* null, %CPT.0** %cpv2
  %cast.0 = bitcast %CPT.0** %cpv2 to %CPT.0*
  store %CPT.0* %cast.0, %CPT.0** %cpv1
  %cast.1 = bitcast %CPT.0** %cpv1 to %CPT.0*
  store %CPT.0* %cast.1, %CPT.0** %cpv2
  store i8 0, i8* %b1
  store i8 0, i8* %b2
  store i8 0, i8* %b3
  %cpv1.ld.0 = load %CPT.0*, %CPT.0** %cpv1
  %cast.2 = bitcast %CPT.0** %cpv2 to %CPT.0***
  %cpv2.ld.0 = load %CPT.0**, %CPT.0*** %cast.2
  %.ld.0 = load %CPT.0*, %CPT.0** %cpv2.ld.0
  %icmp.0 = icmp eq %CPT.0* %cpv1.ld.0, %.ld.0
  %zext.0 = zext i1 %icmp.0 to i8
  store i8 %zext.0, i8* %b1
  %cpv2.ld.1 = load %CPT.0*, %CPT.0** %cpv2
  %cast.3 = bitcast %CPT.0* %cpv2.ld.1 to %CPT.0**
  %icmp.1 = icmp eq %CPT.0** %cpv1, %cast.3
  %zext.1 = zext i1 %icmp.1 to i8
  store i8 %zext.1, i8* %b2
  %cpv1.ld.1 = load %CPT.0*, %CPT.0** %cpv1
  %cast.4 = bitcast %CPT.0** %cpv2 to %CPT.0***
  %cpv2.ld.2 = load %CPT.0**, %CPT.0*** %cast.4
  %cast.5 = bitcast %CPT.0** %cpv2.ld.2 to %CPT.0***
  %deref.ld.0 = load %CPT.0**, %CPT.0*** %cast.5
  %cast.6 = bitcast %CPT.0** %deref.ld.0 to %CPT.0***
  %deref.ld.1 = load %CPT.0**, %CPT.0*** %cast.6
  %.ld.1 = load %CPT.0*, %CPT.0** %deref.ld.1
  %icmp.2 = icmp eq %CPT.0* %cpv1.ld.1, %.ld.1
  %zext.2 = zext i1 %icmp.2 to i8
  store i8 %zext.2, i8* %b3
    )RAW_RESULT";

  bool isOK = h.expectBlock(exp);
  EXPECT_TRUE(isOK && "Block does not have expected contents");

  bool broken = h.finish(PreserveDebugInfo);
  EXPECT_FALSE(broken && "Module failed to verify.");
}

TEST(BackEndPointerExprTests, CircularPointerExpressions2) {

  // More tests for circular pointers, this time
  // with multiple levels. Go code:
  //
  //  type p *q
  //  type q *p
  //  func foo() {
  //     var x p
  //     var y q
  //     x = &y
  //     y = &x
  //     b1 := (x == *y)

  FcnTestHarness h("foo");
  Llvm_backend *be = h.be();
  Location loc;

  // Create circular pointer types
  Btype *pht1 = be->placeholder_pointer_type("ph1", loc, false);
  Btype *pht2 = be->placeholder_pointer_type("ph2", loc, false);
  Btype *cpt = be->circular_pointer_type(pht1, false);
  Btype *pcpt = be->pointer_type(cpt);
  be->set_placeholder_pointer_type(pht2, pcpt);
  be->set_placeholder_pointer_type(pht1, be->pointer_type(pcpt));

  // Local vars
  Bvariable *cpv1 = h.mkLocal("x", pht1);
  Bvariable *cpv2 = h.mkLocal("y", pht2);

  {
    // x = &y
    Bexpression *ve1 = be->var_expression(cpv1, loc);
    Bexpression *ve2 = be->var_expression(cpv2, loc);
    Bexpression *adx = be->address_expression(ve2, loc);
    h.mkAssign(ve1, adx);
  }

  {
    // y = &x
    Bexpression *ve1 = be->var_expression(cpv2, loc);
    Bexpression *ve2 = be->var_expression(cpv1, loc);
    Bexpression *adx = be->address_expression(ve2, loc);
    h.mkAssign(ve1, adx);
  }

  Btype *bt = be->bool_type();
  Bvariable *b1 = h.mkLocal("b1", bt);

  {
    // b1 := (x == *y)
    Bexpression *ve0 = be->var_expression(b1, loc);
    Bexpression *ve1 = be->var_expression(cpv1, loc);
    Bexpression *ve2 = be->var_expression(cpv2, loc);
    Bexpression *dex = be->indirect_expression(pht1, ve2, false, loc);
    Bexpression *cmp = be->binary_expression(OPERATOR_EQEQ, ve1, dex, loc);
    h.mkAssign(ve0, cmp);
  }

  const char *exp = R"RAW_RESULT(
  store %CPT.0* null, %CPT.0** %x
  store %CPT.0** null, %CPT.0*** %y
  %cast.0 = bitcast %CPT.0*** %y to %CPT.0*
  store %CPT.0* %cast.0, %CPT.0** %x
  store %CPT.0** %x, %CPT.0*** %y
  store i8 0, i8* %b1
  %x.ld.0 = load %CPT.0*, %CPT.0** %x
  %y.ld.0 = load %CPT.0**, %CPT.0*** %y
  %.ld.0 = load %CPT.0*, %CPT.0** %y.ld.0
  %icmp.0 = icmp eq %CPT.0* %x.ld.0, %.ld.0
  %zext.0 = zext i1 %icmp.0 to i8
  store i8 %zext.0, i8* %b1
    )RAW_RESULT";

  bool isOK = h.expectBlock(exp);
  EXPECT_TRUE(isOK && "Block does not have expected contents");

  bool broken = h.finish(PreserveDebugInfo);
  EXPECT_FALSE(broken && "Module failed to verify.");
}

TEST(BackEndPointerExprTests, CreatePointerOffsetExprs) {

  FcnTestHarness h("foo");
  Llvm_backend *be = h.be();
  Bfunction *func = h.func();
  Location loc;

  Bvariable *p0 = func->getNthParamVar(0);
  Bvariable *p3 = func->getNthParamVar(2);
  Btype *bi64t = be->integer_type(false, 64);

  {
    // deref(ptr_offset(p3, 5)) = 9
    Bexpression *ve = be->var_expression(p3, loc);
    Bexpression *cfive = mkInt32Const(be, 5);
    Bexpression *poe1 = be->pointer_offset_expression(ve, cfive, loc);
    Bexpression *der = be->indirect_expression(bi64t, poe1, false, loc);
    Bexpression *cnine = mkInt64Const(be, 9);
    h.mkAssign(der, cnine);
  }

  {
    // p0 = int32(deref(ptr_offset(p3, 7)))
    Btype *bi32t = be->integer_type(false, 32);
    Bexpression *ve = be->var_expression(p0, loc);
    Bexpression *ver = be->var_expression(p3, loc);
    Bexpression *cseven = mkInt32Const(be, 7);
    Bexpression *poe2 = be->pointer_offset_expression(ver, cseven, loc);
    Bexpression *der = be->indirect_expression(bi64t, poe2, false, loc);
    Bexpression *con32 = be->convert_expression(bi32t, der, loc);
    h.mkAssign(ve, con32);
  }

  const char *exp = R"RAW_RESULT(
  %param3.ld.0 = load i64*, i64** %param3.addr
  %ptroff.0 = getelementptr i64, i64* %param3.ld.0, i32 5
  store i64 9, i64* %ptroff.0
  %param3.ld.1 = load i64*, i64** %param3.addr
  %ptroff.1 = getelementptr i64, i64* %param3.ld.1, i32 7
  %.ptroff.ld.0 = load i64, i64* %ptroff.1
  %trunc.0 = trunc i64 %.ptroff.ld.0 to i32
  store i32 %trunc.0, i32* %param1.addr
  )RAW_RESULT";

  bool isOK = h.expectBlock(exp);
  EXPECT_TRUE(isOK && "Block does not have expected contents");

  bool broken = h.finish(PreserveDebugInfo);
  EXPECT_FALSE(broken && "Module failed to verify.");
}

TEST(BackEndPointerExprTests, TestAddrDerefFold) {

  FcnTestHarness h("foo");
  Llvm_backend *be = h.be();
  Bfunction *func = h.func();
  Location loc;

  Bvariable *p3 = func->getNthParamVar(2);
  Btype *bi64t = be->integer_type(false, 64);
  Bvariable *xv = h.mkLocal("x", bi64t);

  // p3 = addr(deref(addr(deref(addr(x))))))
  Bexpression *vexr = be->var_expression(xv, loc);
  Bexpression *ad1 = be->address_expression(vexr, loc);
  Bexpression *der1 = be->indirect_expression(bi64t, ad1, false, loc);
  Bexpression *ad2 = be->address_expression(der1, loc);
  Bexpression *der2 = be->indirect_expression(bi64t, ad2, false, loc);
  Bexpression *ad3 = be->address_expression(der2, loc);
  Bexpression *vexl = be->var_expression(p3, loc);
  h.mkAssign(vexl, ad3);

  const char *exp = R"RAW_RESULT(
  store i64 0, i64* %x
  store i64* %x, i64** %param3.addr
  )RAW_RESULT";

  bool isOK = h.expectBlock(exp);
  EXPECT_TRUE(isOK && "Block does not have expected contents");

  bool broken = h.finish(PreserveDebugInfo);
  EXPECT_FALSE(broken && "Module failed to verify.");
}

TEST(BackEndPointerExprTests, TestDerefPointerConstantLHS)
{
  FcnTestHarness h("foo");
  Llvm_backend *be = h.be();
  Location loc;

  // *(*int32)(unsafe.Pointer(uintptr(0x10101))) = 0
  {
    Bexpression *val10101 = mkUint64Const(be, uint64_t(0x10101));
    Btype *buipt = be->uintPtrType();
    Bexpression *con1 = be->convert_expression(buipt, val10101, loc);
    Btype *bpvt = be->pointer_type(be->void_type());
    Bexpression *con2 = be->convert_expression(bpvt, con1, loc);
    Btype *bi32t = be->integer_type(false, 32);
    Btype *bpi32t = be->pointer_type(bi32t);
    Bexpression *con3 = be->convert_expression(bpi32t, con2, loc);
    bool knValid = false;
    Bexpression *der = be->indirect_expression(bi32t, con3, knValid, loc);
    Bexpression *val0 = mkInt32Const(be, int32_t(0));
    h.mkAssign(der, val0);
  }

  // type xyz struct { x, y int64 }
  // (*xyz)(unsafe.Pointer(uintptr(0x8765))).y = 2
  {
    Btype *bi64t = be->integer_type(false, 64);
    Btype *xyz = mkBackendStruct(be, bi64t, "x", bi64t, "y", nullptr);
    Btype *pxyz =  be->pointer_type(xyz);
    Bexpression *val8765 = mkUint64Const(be, uint64_t(0x8765));
    Btype *buipt = be->uintPtrType();
    Bexpression *con1 = be->convert_expression(buipt, val8765, loc);
    Btype *bpvt = be->pointer_type(be->void_type());
    Bexpression *con2 = be->convert_expression(bpvt, con1, loc);
    Bexpression *con3 = be->convert_expression(pxyz, con2, loc);
    bool knValid = false;
    Bexpression *der = be->indirect_expression(xyz, con3, knValid, loc);
    Bexpression *fex = be->struct_field_expression(der, 1, loc);
    Bexpression *val2 = mkInt64Const(be, int64_t(2));
    h.mkAssign(fex, val2);
  }

  const char *exp = R"RAW_RESULT(
    store i32 0, i32* inttoptr (i64 65793 to i32*)
    store i64 2, i64* getelementptr inbounds ({ i64, i64 }, { i64, i64 }*
      inttoptr (i64 34661 to { i64, i64 }*), i32 0, i32 1)
  )RAW_RESULT";

  bool isOK = h.expectBlock(exp);
  EXPECT_TRUE(isOK && "Block does not have expected contents");

  bool broken = h.finish(PreserveDebugInfo);
  EXPECT_FALSE(broken && "Module failed to verify.");
}

TEST(BackEndPointerExprTests, TestCircularFunctionTypes)
{
  // Make sure we can handle circular function types, especially
  // those in which the cycle extends across multiple types. Example:
  //
  // type gf1 func(int, int, gf1, gf2) int
  // type gf2 func(int, int, gf2, gf1) int
  //

  FcnTestHarness h("foo");
  Llvm_backend *be = h.be();
  Location loc;

  // Create circular pointer types
  Btype *bi64t = be->integer_type(false, 64);
  Btype *pht1 = be->placeholder_pointer_type("ph1", loc, false);
  Btype *pht2 = be->placeholder_pointer_type("ph2", loc, false);
  Btype *cft1 = be->circular_pointer_type(pht1, true);
  Btype *cft2 = be->circular_pointer_type(pht2, true);
  BFunctionType *befty2 = mkFuncTyp(be,
                                    L_PARM, bi64t,
                                    L_PARM, bi64t,
                                    L_PARM, cft2,
                                    L_PARM, cft1,
                                    L_RES, bi64t,
                                    L_END);
  Btype *pbefty2 = be->pointer_type(befty2);
  be->set_placeholder_pointer_type(pht2, pbefty2);
  BFunctionType *befty1 = mkFuncTyp(be,
                                    L_PARM, bi64t,
                                    L_PARM, bi64t,
                                    L_PARM, cft1,
                                    L_PARM, cft2,
                                    L_RES, bi64t,
                                    L_END);
  Btype *pbefty1 = be->pointer_type(befty1);
  be->set_placeholder_pointer_type(pht1, pbefty1);

  // Local vars
  h.mkLocal("x", pbefty1);
  h.mkLocal("y", pbefty2);

  const char *exp = R"RAW_RESULT(
   store i64 (i8*, i64, i64, %CFT.0*, %CFT.1*)* null, i64 (i8*, i64, i64, %CFT.0*, %CFT.1*)** %x
   store i64 (i8*, i64, i64, %CFT.1*, %CFT.0*)* null, i64 (i8*, i64, i64, %CFT.1*, %CFT.0*)** %y

    )RAW_RESULT";

  bool isOK = h.expectBlock(exp);
  EXPECT_TRUE(isOK && "Block does not have expected contents");

  bool broken = h.finish(PreserveDebugInfo);
  EXPECT_FALSE(broken && "Module failed to verify.");
}

}
