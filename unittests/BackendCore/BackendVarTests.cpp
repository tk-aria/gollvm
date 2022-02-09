//===- llvm/tools/gollvm/unittests/BackendCore/BackendVarTests.cpp ------===//
//
// Copyright 2018 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
//
//===----------------------------------------------------------------------===//

#include "TestUtils.h"
#include "go-llvm-backend.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/GlobalVariable.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Value.h"
#include "gtest/gtest.h"

using namespace llvm;
using namespace goBackendUnitTests;

namespace {

class BackendVarTests : public testing::TestWithParam<llvm::CallingConv::ID> {};

INSTANTIATE_TEST_SUITE_P(
    UnitTest, BackendVarTests,
    goBackendUnitTests::CConvs,
    [](const testing::TestParamInfo<BackendVarTests::ParamType> &info) {
      std::string name = goBackendUnitTests::ccName(info.param);
      return name;
    });

TEST_P(BackendVarTests, MakeLocalVar) {
  auto cc = GetParam();
  FcnTestHarness h(cc, "foo");
  Llvm_backend *be = h.be();
  Bfunction *func1 = h.func();
  Bfunction *func2 = mkFunci32o64(be, "bar");

  // Manufacture some locals
  Location loc;
  Btype *bi64t = be->integer_type(false, 64);
  Btype *bst = mkBackendThreeFieldStruct(be);
  Bvariable *loc1 = h.mkLocal("loc1", bi64t);
  ASSERT_TRUE(loc1 != nullptr);
  EXPECT_TRUE(loc1 != be->error_variable());
  Bvariable *loc2 = h.mkLocal("loc2", bst);
  ASSERT_TRUE(loc2 != nullptr);
  EXPECT_TRUE(loc2 != be->error_variable());
  Bvariable *loc3 = be->local_variable(func2, "loc3", bst, nullptr, false, loc);
  ASSERT_TRUE(loc3 != nullptr);
  EXPECT_TRUE(loc3 != be->error_variable());

  // Examine resulting alloca instructions
  EXPECT_TRUE(isa<AllocaInst>(loc1->value()));
  EXPECT_TRUE(isa<AllocaInst>(loc2->value()));
  EXPECT_TRUE(loc1 != loc2 && loc1->value() != loc2->value());

  // Test var_expression created from local variable
  Bexpression *ve1 = be->var_expression(loc1, Location());
  ASSERT_TRUE(ve1 != nullptr);
  EXPECT_EQ(ve1->value(), loc1->value());

  // Test var_expression created from local variable
  Bexpression *ve2 = be->var_expression(loc1, Location());
  ASSERT_TRUE(ve2 != nullptr);
  Bstatement *es = h.mkExprStmt(ve2);
  EXPECT_EQ(repr(ve2->value()), "%loc1 = alloca i64, align 8");
  EXPECT_EQ(repr(es), "%loc1.ld.0 = load i64, i64* %loc1, align 8");

  // Make sure error detection is working
  Bvariable *loce = be->local_variable(func1, "", be->error_type(), nullptr,
                                       true, loc);
  EXPECT_TRUE(loce == be->error_variable());

  bool broken = h.finish(PreserveDebugInfo);
  EXPECT_FALSE(broken && "Module failed to verify.");
}

TEST_P(BackendVarTests, MakeParamVar) {
  LLVMContext C;
  auto cc = GetParam();
  std::unique_ptr<Backend> be(go_get_backend(C, cc));
  bool dontMakeParams = false;
  Bfunction *func = mkFunci32o64(be.get(), "foo", dontMakeParams);

  // Add params for the function
  Btype *bi32t = be->integer_type(false, 32);
  Bvariable *p1 = be->parameter_variable(func, "p1", bi32t, false, Location());
  Bvariable *p2 = be->parameter_variable(func, "p2", bi32t, false, Location());
  ASSERT_TRUE(p1 != nullptr);
  ASSERT_TRUE(p2 != nullptr);
  EXPECT_TRUE(p1 != p2);
  EXPECT_TRUE(p1 != be->error_variable());

  // Values for param variables will be the alloca instructions
  // created to capture their values
  EXPECT_TRUE(isa<AllocaInst>(p1->value()));
  EXPECT_TRUE(isa<AllocaInst>(p2->value()));

  // Test var_expression created from param variable
  Bexpression *ve1 = be->var_expression(p1, Location());
  ASSERT_TRUE(ve1 != nullptr);
  EXPECT_EQ(ve1->value(), p1->value());

  // Error handling
  Bfunction *func2 = mkFunci32o64(be.get(), "bar");
  Bvariable *p3 =
      be->parameter_variable(func2, "p3", be->error_type(), false, Location());
  EXPECT_TRUE(p3 == be->error_variable());
}

TEST_P(BackendVarTests, MakeGlobalVar) {
  LLVMContext C;
  auto cc = GetParam();
  std::unique_ptr<Backend> be(go_get_backend(C, cc));

  Btype *bi32t = be->integer_type(false, 32);
  unsigned int g1flags = 0;
  Bvariable *g1 =
      be->global_variable("varname", "asmname", bi32t, g1flags, Location());
  ASSERT_TRUE(g1 != nullptr);
  Value *g1val = g1->value();
  ASSERT_TRUE(g1val != nullptr);
  EXPECT_TRUE(isa<GlobalVariable>(g1val));
  EXPECT_EQ(g1val->getName(), "asmname");

  // Set initializer
  be->global_variable_set_init(g1, mkInt32Const(be.get(), 101));

  // Test var_expression created from global variable
  Bexpression *ve1 = be->var_expression(g1, Location());
  ASSERT_TRUE(ve1 != nullptr);
  EXPECT_EQ(ve1->value(), g1->value());

  // error case
  unsigned int anonflags = 0;
  Bvariable *gerr =
      be->global_variable("", "", be->error_type(), anonflags, Location());
  EXPECT_TRUE(gerr == be->error_variable());
}

TEST_P(BackendVarTests, MakeTemporaryVar) {
  auto cc = GetParam();
  FcnTestHarness h(cc, "foo");
  Llvm_backend *be = h.be();
  Bfunction *func = h.func();

  // var b bool = true
  Location loc;
  Btype *boolt = be->bool_type();
  h.mkLocal("b", boolt, be->boolean_constant_expression(true));

  // temporary var [uint64] = 99
  Btype *bu64t = be->integer_type(true, 64);
  Bstatement *inits = nullptr;
  Bexpression *con64 = mkUint64Const(be, 64);
  Bvariable *tvar = be->temporary_variable(func, h.block(), bu64t, con64,
                                           false, loc, &inits);
  ASSERT_TRUE(tvar != nullptr);
  ASSERT_TRUE(inits != nullptr);
  EXPECT_EQ(repr(inits), "store i64 64, i64* %tmpv.0, align 8");

  h.addStmt(inits);

  bool broken = h.finish(PreserveDebugInfo);
  EXPECT_FALSE(broken && "Module failed to verify.");
}

TEST_P(BackendVarTests, MakeImmutableStruct) {
  auto cc = GetParam();
  FcnTestHarness h(cc, "foo");
  Llvm_backend *be = h.be();

  Btype *bi32t = be->integer_type(false, 32);
  Btype *bst = mkTwoFieldStruct(be, bi32t, bi32t);

  const bool is_common[2] = {true, false};
  const bool is_hidden[2] = {true, false};
  Location loc;
  GlobalVariable *gvar = nullptr;
  bool first = true;
  for (auto hidden : is_hidden) {
    for (auto common : is_common) {
      if (hidden && common)
        continue;
      unsigned int flags = (hidden ? Backend::variable_is_hidden : 0) |
                           (common ? Backend::variable_is_common : 0);
      Bvariable *ims =
          be->immutable_struct("name", "asmname", flags, bst, loc);
      ASSERT_TRUE(ims != nullptr);
      Value *ival = ims->value();
      ASSERT_TRUE(ival != nullptr);
      EXPECT_TRUE(isa<GlobalVariable>(ival));
      if (first) {
        gvar = cast<GlobalVariable>(ival);
        EXPECT_EQ(gvar->getName(), "asmname");
        first = false;
      }
    }
  }

  // error case
  unsigned int emptyflags = 0;
  Bvariable *gerr =
      be->immutable_struct("", "", emptyflags, be->error_type(), Location());
  EXPECT_TRUE(gerr == be->error_variable());

  bool broken = h.finish(PreserveDebugInfo);
  EXPECT_FALSE(broken && "Module failed to verify.");
}

TEST_P(BackendVarTests, MakeImplicitVariable) {
  LLVMContext C;
  auto cc = GetParam();
  std::unique_ptr<Backend> be(go_get_backend(C, cc));

  Btype *bi32t = be->integer_type(false, 32);
  Btype *bst = mkTwoFieldStruct(be.get(), bi32t, bi32t);

  const bool is_common[2] = {true, false};
  const bool is_constant[2] = {true, false};
  const bool is_hidden[2] = {true, false};
  GlobalVariable *gvar = nullptr;
  bool first = true;
  for (auto hidden : is_hidden) {
    for (auto common : is_common) {
      for (auto iscon : is_constant) {
        if (hidden && common)
          continue;
      unsigned int flags = (hidden ? Backend::variable_is_hidden : 0) |
                           (common ? Backend::variable_is_common : 0) |
                           (iscon ? Backend::variable_is_constant : 0);
        Bvariable *ims =
            be->implicit_variable("name", "asmname", bst, flags, 8);
        ASSERT_TRUE(ims != nullptr);
        Value *ival = ims->value();
        ASSERT_TRUE(ival != nullptr);
        EXPECT_TRUE(isa<GlobalVariable>(ival));
        if (first) {
          gvar = cast<GlobalVariable>(ival);
          EXPECT_EQ(gvar->getName(), "asmname");
          first = false;
        }
      }
    }
  }

  // error case
  unsigned emptyflags = 0;
  Bvariable *gerr =
      be->implicit_variable("", "", be->error_type(), emptyflags, 0);
  EXPECT_TRUE(gerr == be->error_variable());
}

TEST_P(BackendVarTests, MakeImmutableStructReference) {
  auto cc = GetParam();
  FcnTestHarness h(cc, "foo");
  Llvm_backend *be = h.be();

  Location loc;
  Btype *bi32t = be->integer_type(false, 32);
  Btype *bst = mkTwoFieldStruct(be, bi32t, bi32t);
  Bvariable *ims =
      be->immutable_struct_reference("name", "asmname", bst, loc);
  ASSERT_TRUE(ims != nullptr);
  Value *ival = ims->value();
  ASSERT_TRUE(ival != nullptr);
  EXPECT_TRUE(isa<GlobalVariable>(ival));
  EXPECT_EQ(repr(ival), "@asmname = external constant { i32, i32 }");

  // error case
  Bvariable *ierr =
      be->immutable_struct_reference("name", "asmname", be->error_type(), loc);
  EXPECT_TRUE(ierr == be->error_variable());

  bool broken = h.finish(PreserveDebugInfo);
  EXPECT_FALSE(broken && "Module failed to verify.");
}

TEST_P(BackendVarTests, ImmutableStructSetInit) {
  auto cc = GetParam();
  FcnTestHarness h(cc, "foo");
  Llvm_backend *be = h.be();
  Bfunction *func = h.func();

  Location loc;
  Btype *bt = be->bool_type();
  Btype *pbt = be->pointer_type(bt);
  Btype *uintptrt = be->integer_type(true, be->type_size(pbt)*8);
  Btype *desct = mkBackendStruct(be, uintptrt, "x", nullptr);
  unsigned int descflags = Backend::variable_is_hidden;
  Bvariable *ims = be->immutable_struct("desc", "desc", descflags, desct, loc);
  Bexpression *fp = be->function_code_expression(func, loc);
  Bexpression *confp = be->convert_expression(uintptrt, fp, loc);

  std::vector<Bexpression *> vals;
  vals.push_back(confp);
  Bexpression *scon = be->constructor_expression(desct, vals, loc);
  unsigned int emptyflags = 0;
  be->immutable_struct_set_init(ims, "", emptyflags, desct, loc, scon);

  {
    DECLARE_EXPECTED_OUTPUT(exp, R"RAW_RESULT(
      @desc = internal constant { i64 } { i64 ptrtoint
      (i64 (i8*, i32, i32, i64*)* @foo to i64) }
    )RAW_RESULT");

    bool isOK = h.expectValue(ims->value(), exp);
    EXPECT_TRUE(isOK && "Value does not have expected contents");
  }

  unsigned int xyzflags = Backend::variable_is_common;
  Bvariable *ims2 = be->immutable_struct("xyz", "abc", xyzflags, desct, loc);
  be->immutable_struct_set_init(ims2, "", xyzflags,
                                desct, loc, be->zero_expression(desct));

  {
    DECLARE_EXPECTED_OUTPUT(exp, R"RAW_RESULT(
      @abc = weak constant { i64 } zeroinitializer, comdat
    )RAW_RESULT");

    bool isOK = h.expectValue(ims2->value(), exp);
    EXPECT_TRUE(isOK && "Value does not have expected contents");
  }

  // check that these don't crash
  be->immutable_struct_set_init(be->error_variable(), "", emptyflags,
                                desct, loc, scon);
  be->immutable_struct_set_init(ims, "", emptyflags,
                                desct, loc, be->error_expression());

  bool broken = h.finish(PreserveDebugInfo);
  EXPECT_FALSE(broken && "Module failed to verify.");
}

TEST_P(BackendVarTests, MakeImmutableStructReferenceWithSameName) {
  auto cc = GetParam();
  FcnTestHarness h(cc, "foo");
  Llvm_backend *be = h.be();

  Location loc;
  Btype *bi32t = be->integer_type(false, 32);
  Btype *bst = mkTwoFieldStruct(be, bi32t, bi32t);

  // Create an immutable_struct and an immutable_struct_reference
  // with same name. They should refer to the same global variable.
  unsigned int emptyflags = 0;
  Bvariable *ims =
      be->immutable_struct("name", "asmname", emptyflags, bst, loc);
  ASSERT_TRUE(ims != nullptr);

  Bvariable *imsr = be->immutable_struct_reference("name", "asmname", bst, loc);
  ASSERT_TRUE(imsr != nullptr);
  EXPECT_TRUE(isa<GlobalVariable>(imsr->value()));
  EXPECT_TRUE(imsr->value() == ims->value());

  bool broken = h.finish(PreserveDebugInfo);
  EXPECT_FALSE(broken && "Module failed to verify.");
}

TEST_P(BackendVarTests, ImplicitVariableSetInit) {
  auto cc = GetParam();
  FcnTestHarness h(cc, "foo");
  Llvm_backend *be = h.be();

  Location loc;

  Btype *bi32t = be->integer_type(false, 32);
  Btype *bst = mkTwoFieldStruct(be, bi32t, bi32t);

  // Case 1: non-common, concrete init value.
  unsigned int firstflags = 0;
  Bvariable *ims1 =
      be->implicit_variable("first", "v1", bst, firstflags, 8);
  std::vector<Bexpression *> vals1;
  vals1.push_back(mkInt32Const(be, 101));
  vals1.push_back(mkInt32Const(be, 202));
  Bexpression *con1 = be->constructor_expression(bst, vals1, loc);
  be->implicit_variable_set_init(ims1, "x", bst, firstflags, con1);

  DECLARE_EXPECTED_OUTPUT(exp1, R"RAW_RESULT(
    @v1 = global { i32, i32 } { i32 101, i32 202 }, align 8
  )RAW_RESULT");

  bool isOK1 = h.expectValue(ims1->value(), exp1);
  EXPECT_TRUE(isOK1 && "Value does not have expected contents");

  // Case 2: const, common, no init value.
  unsigned int secondflags = (Backend::variable_is_constant |
                              Backend::variable_is_common);
  Bvariable *ims2 = be->implicit_variable("second", "v2", bst, secondflags, 8);
  be->implicit_variable_set_init(ims2, "x", bst, secondflags, nullptr);

  DECLARE_EXPECTED_OUTPUT(exp2, R"RAW_RESULT(
    @v2 = weak constant { i32, i32 } zeroinitializer, comdat, align 8
  )RAW_RESULT");

  bool isOK2 = h.expectValue(ims2->value(), exp2);
  EXPECT_TRUE(isOK2 && "Value does not have expected contents");

  // check that these don't crash
  unsigned int iniflags = secondflags;
  be->implicit_variable_set_init(be->error_variable(), "x", bst,
                                 iniflags, nullptr);
  be->implicit_variable_set_init(be->error_variable(), "x", bst,
                                 iniflags, be->error_expression());

  bool broken = h.finish(PreserveDebugInfo);
  EXPECT_FALSE(broken && "Module failed to verify.");
}

TEST_P(BackendVarTests, GlobalVarSetInitToComposite) {
  LLVMContext C;
  auto cc = GetParam();
  std::unique_ptr<Backend> be(go_get_backend(C, cc));
  Location loc;

  Btype *bt = be->bool_type();
  Btype *pbt = be->pointer_type(bt);
  Btype *bi32t = be->integer_type(false, 32);
  Btype *s2t = mkBackendStruct(be.get(), pbt, "f1", bi32t, "f2", nullptr);
  unsigned int g1flags = 0;
  Bvariable *g1 =
      be->global_variable("gv", "gv", s2t, g1flags, Location());

  std::vector<Bexpression *> vals;
  vals.push_back(be->zero_expression(pbt));
  vals.push_back(mkInt32Const(be.get(), int32_t(101)));
  Bexpression *scon =
      be->constructor_expression(s2t, vals, loc);

  // Set initializer
  be->global_variable_set_init(g1, scon);
}

TEST_P(BackendVarTests, GlobalVarsWithSameName) {
  auto cc = GetParam();
  FcnTestHarness h(cc, "foo");
  Llvm_backend *be = h.be();

  Location loc;
  Btype *bi32t = be->integer_type(false, 32);
  Btype *bst = mkBackendStruct(be, bi32t, "f", nullptr);

  // Create two global variables with same name, one is
  // a declaration (external), the other is a definition.
  // They should refer to the same underlying global var.

  // declare x as external int32
  unsigned int xflags = Backend::variable_is_external;
  Bvariable *gvdecl =
      be->global_variable("x", "x", bi32t, xflags, loc);
  ASSERT_TRUE(gvdecl != nullptr);

  // define x as non-external struct
  xflags = 0; // no longer external
  Bvariable *gv =
      be->global_variable("x", "x", bst, xflags, loc);
  ASSERT_TRUE(gv != nullptr);
  EXPECT_TRUE(isa<GlobalVariable>(gv->value()));
  EXPECT_EQ(repr(gv->value()), "@x = global { i32 } zeroinitializer");
  EXPECT_EQ(repr(gvdecl->value()),
            "i32* getelementptr inbounds ({ i32 }, { i32 }* @x, i32 0, i32 0)");

  // Create them in the other order: definition first,
  // then external declaration.
  // define y as non-external struct
  unsigned int yflags = 0;
  Bvariable *gv2 =
      be->global_variable("y", "y", bst, yflags, loc);
  ASSERT_TRUE(gv2 != nullptr);

  // declare y as external int32
  yflags = Backend::variable_is_external;
  Bvariable *gvdecl2 =
      be->global_variable("y", "y", bi32t, yflags, loc);
  ASSERT_TRUE(gvdecl2 != nullptr);
  EXPECT_TRUE(isa<GlobalVariable>(gv2->value()));
  EXPECT_EQ(repr(gv2->value()), "@y = global { i32 } zeroinitializer");
  EXPECT_EQ(repr(gvdecl2->value()),
      "i32* getelementptr inbounds ({ i32 }, { i32 }* @y, i32 0, i32 0)");

  bool broken = h.finish(PreserveDebugInfo);
  EXPECT_FALSE(broken && "Module failed to verify.");
}

TEST_P(BackendVarTests, TestVarLifetimeInsertion) {
  auto cc = GetParam();
  FcnTestHarness h(cc);
  Llvm_backend *be = h.be();
  BFunctionType *befty1 = mkFuncTyp(be, L_END);
  Bfunction *func = h.mkFunction("foo", befty1);

  Location loc;
  Btype *bi32t = be->integer_type(false, 32);
  Btype *bst = mkTwoFieldStruct(be, bi32t, bi32t);

  // Block with two locals
  Bvariable *x = be->local_variable(func, "x", bi32t, nullptr, false, loc);
  Bvariable *y = be->local_variable(func, "y", bst, nullptr, false, loc);
  const std::vector<Bvariable *> vars = { x, y };
  Bblock *b1 = be->block(func, nullptr, vars, loc, loc);
  Bstatement *is1 = be->init_statement(func, x, be->zero_expression(bi32t));
  Bstatement *is2 = be->init_statement(func, y, be->zero_expression(bst));

  // x := y.f1
  Bexpression *ve1 = be->var_expression(x, loc);
  Bexpression *ve2 = be->var_expression(y, loc);
  Bexpression *fex = be->struct_field_expression(ve2, 1, loc);
  Bstatement *as =
      be->assignment_statement(func, ve1, fex, loc);
  const std::vector<Bstatement *> slist = { is1, is2, as };
  be->block_add_statements(b1, slist);
  Bstatement *bs = be->block_statement(b1);
  h.addStmt(bs);

  bool broken = h.finish(StripDebugInfo);
  EXPECT_FALSE(broken && "Module failed to verify.");

  DECLARE_EXPECTED_OUTPUT(exp, R"RAW_RESULT(
    define void @foo(i8* nest %nest.0) #0 {
  entry:
    %x = alloca i32, align 4
    %y = alloca { i32, i32 }, align 4
    %0 = bitcast i32* %x to i8*
    call void @llvm.lifetime.start.p0i8(i64 4, i8* %0)
    %1 = bitcast { i32, i32 }* %y to i8*
    call void @llvm.lifetime.start.p0i8(i64 8, i8* %1)
    store i32 0, i32* %x, align 4
    %cast.0 = bitcast { i32, i32 }* %y to i8*
    call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 4 %cast.0, i8* align 4 bitcast ({ i32, i32 }* @const.0 to i8*), i64 8, i1 false)
    %field.0 = getelementptr inbounds { i32, i32 }, { i32, i32 }* %y, i32 0, i32 1
    %y.field.ld.0 = load i32, i32* %field.0, align 4
    store i32 %y.field.ld.0, i32* %x, align 4
    %2 = bitcast i32* %x to i8*
    call void @llvm.lifetime.end.p0i8(i64 4, i8* %2)
    %3 = bitcast { i32, i32 }* %y to i8*
    call void @llvm.lifetime.end.p0i8(i64 8, i8* %3)
    ret void
  }
  )RAW_RESULT");

  bool isOK = h.expectValue(func->function(), exp);
  EXPECT_TRUE(isOK && "Value does not have expected contents");
}

TEST_P(BackendVarTests, ZeroSizedGlobals) {
  auto cc = GetParam();
  FcnTestHarness h(cc);
  Llvm_backend *be = h.be();
  BFunctionType *befty1 = mkFuncTyp(be, L_END);
  Bfunction *func = h.mkFunction("foo", befty1);
  Location loc;

  // This test case is designed to exercise code in the bridge that
  // handles zero-sized globals.  It turns out that these critters
  // have to be handled carefully due to problems in the way that
  // linkers handle them.

  // empty struct type
  Btype *best = mkBackendStruct(be, nullptr);

  // a struct with two empty fields
  Btype *bef2 = mkBackendStruct(be, best, "f1", best, "f2", nullptr);

  // a zero-length array of the struct above
  Bexpression *val0 = mkInt64Const(be, int64_t(0));
  Btype *ats1 = be->array_type(bef2, val0);

  // a zero-length array of some non-zero sized type
  Btype *bi32t = be->integer_type(false, 32);
  Btype *atint0 = be->array_type(bi32t, val0);

  // define globals with the types above
  unsigned int emptyflags = 0;
  Bvariable *gs1 =
      be->global_variable("emptystruct", "", best, emptyflags, loc);
  Bvariable *gs2 =
      be->global_variable("emptys2f", "", bef2, emptyflags, loc);
  Bvariable *ga1 =
      be->global_variable("emptyar", "", ats1, emptyflags, loc);
  Bvariable *ga2 =
      be->global_variable("emptyintar", "", atint0, emptyflags, loc);

  // Create initializers for these odd beasties.

  // this first one doesn't look so bad...
  be->global_variable_set_init(gs1, be->zero_expression(best));

  // now we're getting weird, however:
  std::vector<Bexpression *> vals;
  vals.push_back(be->zero_expression(best));
  vals.push_back(be->zero_expression(best));
  Bexpression *emptycon = be->constructor_expression(bef2, vals, loc);
  be->global_variable_set_init(gs2, emptycon);

  // ... and some weird empty array initializers as well
  const std::vector<unsigned long> ivals;
  const std::vector<Bexpression *> exprs;
  Bexpression *ear1 =
      be->array_constructor_expression(ats1, ivals, exprs, loc);
  be->global_variable_set_init(ga1, ear1);
  Bexpression *ear2 =
      be->array_constructor_expression(atint0, ivals, exprs, loc);
  be->global_variable_set_init(ga2, ear2);

  {
    DECLARE_EXPECTED_OUTPUT(exp, R"RAW_RESULT(
      @emptystruct = global { i8 } zeroinitializer
    )RAW_RESULT");
    bool isOK = h.expectValue(gs1->value(), exp);
    EXPECT_TRUE(isOK && "Value does not have expected contents");
  }
  {
    DECLARE_EXPECTED_OUTPUT(exp, R"RAW_RESULT(
      @emptys2f = global { { i8 }, {} } zeroinitializer
    )RAW_RESULT");
    bool isOK = h.expectValue(gs2->value(), exp);
    EXPECT_TRUE(isOK && "Value does not have expected contents");
  }
  {
    DECLARE_EXPECTED_OUTPUT(exp, R"RAW_RESULT(
      @emptyar = global [1 x { { i8 }, {} }] zeroinitializer
    )RAW_RESULT");
    bool isOK = h.expectValue(ga1->value(), exp);
    EXPECT_TRUE(isOK && "Value does not have expected contents");
  }
  {
    DECLARE_EXPECTED_OUTPUT(exp, R"RAW_RESULT(
      @emptyintar = global [1 x i32] zeroinitializer
    )RAW_RESULT");
    bool isOK = h.expectValue(ga2->value(), exp);
    EXPECT_TRUE(isOK && "Value does not have expected contents");
  }

  // OK, now that we have out empty globals set up, manufacture
  // some accesses to them.

  // emptys2f.f1 = emptystruct
  Bexpression *vex1 = be->var_expression(gs2, loc);
  Bexpression *fex1 = be->struct_field_expression(vex1, 0, loc);
  Bexpression *vex2 = be->var_expression(gs1, loc);
  h.mkAssign(fex1, vex2);

  // localemptys2f = emptys2f
  Bvariable *loc1 = h.mkLocal("localemptys2f", bef2);
  Bexpression *vex3 = be->var_expression(loc1, loc);
  Bexpression *vex4 = be->var_expression(gs2, loc);
  h.mkAssign(vex3, vex4);

  // localemptyintar = emptyintar
  Bvariable *loc2 = h.mkLocal("localemptyintar", atint0);
  Bexpression *vex5 = be->var_expression(loc2, loc);
  Bexpression *vex6 = be->var_expression(ga2, loc);
  h.mkAssign(vex5, vex6);

  bool broken = h.finish(StripDebugInfo);
  EXPECT_FALSE(broken && "Module failed to verify.");

  DECLARE_EXPECTED_OUTPUT(exp, R"RAW_RESULT(
    define void @foo(i8* nest %nest.0) #0 {
  entry:
    %localemptys2f = alloca { {}, {} }, align 1
    %localemptyintar = alloca [0 x i32], align 4
    ret void
  }
  )RAW_RESULT");
  bool isOK = h.expectValue(func->function(), exp);
  EXPECT_TRUE(isOK && "Value does not have expected contents");
}

TEST_P(BackendVarTests, MakeLocalWithDeclVar) {
  auto cc = GetParam();
  FcnTestHarness h(cc, "foo");
  Llvm_backend *be = h.be();
  Bfunction *func1 = h.func();

  // Create an initial local variable at the top level.
  Location loc;
  Btype *bi64t = be->integer_type(false, 64);
  Bvariable *loc1 = h.mkLocal("loc1", bi64t);

  // Now manufacture a second local that refers to the first via the
  // "declVar" mechanism.
  Bvariable *loc2 = be->local_variable(func1, "loc2", bi64t, loc1, false, loc);
  Bstatement *is2 = be->init_statement(func1, loc2, mkInt64Const(be, 9));

  // Third regular local.
  Bvariable *loc3 = be->local_variable(func1, "loc3", bi64t, nullptr,
                                       false, loc);
  Bstatement *is3 = be->init_statement(func1, loc3, mkInt64Const(be, 11));

  // Place the second and third variables in a block.
  std::vector<Bvariable *> vars;
  vars.push_back(loc2);
  vars.push_back(loc3);
  h.newBlock(&vars);

  // Add inits to block
  std::vector<Bstatement *> bstmts;
  bstmts.push_back(is2);
  bstmts.push_back(is3);
  be->block_add_statements(h.block(), bstmts);

  // The two vars should share the same alloca instruction.
  EXPECT_TRUE(loc1->value() == loc2->value());

  bool broken = h.finish(PreserveDebugInfo);
  EXPECT_FALSE(broken && "Module failed to verify.");

  // Expect to see only one lifetime start, since A) loc1 is at the top
  // level, and B) loc2 uses loc1 as declVar.
  const char *expected = "call void @llvm.lifetime.start.p0i8(i64";
  EXPECT_EQ(h.countInstancesInModuleDump(expected), 1u);
}

} // namespace
