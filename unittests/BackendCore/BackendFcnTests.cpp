//===- llvm/tools/gollvm/unittests/BackendCore/BackendFcnTests.cpp ------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "TestUtils.h"
#include "go-llvm-backend.h"
#include "llvm/IR/Function.h"
#include "gtest/gtest.h"

using namespace llvm;
using namespace goBackendUnitTests;

namespace {

TEST(BackendFcnTests, MakeEmptyFunction) {

  // Create empty function
  FcnTestHarness h;
  Llvm_backend *be = h.be();
  BFunctionType *befty1 = mkFuncTyp(be, L_END);
  h.mkFunction("foo", befty1);

  const char *exp = R"RAW_RESULT(
    )RAW_RESULT";

  bool isOK = h.expectBlock(exp);
  EXPECT_TRUE(isOK && "Block does not have expected contents");

  bool broken = h.finish(PreserveDebugInfo);
  EXPECT_FALSE(broken && "Module failed to verify.");
}

TEST(BackendFcnTests, MakeFuncWithLotsOfArgs) {

  // Create empty function
  FcnTestHarness h;
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

  const char *exp = R"RAW_RESULT(
    )RAW_RESULT";

  bool isOK = h.expectBlock(exp);
  EXPECT_TRUE(isOK && "Block does not have expected contents");

  bool broken = h.finish(PreserveDebugInfo);
  EXPECT_FALSE(broken && "Module failed to verify.");
}

TEST(BackendFcnTests, MakeFunction) {
  LLVMContext C;

  std::unique_ptr<Backend> be(go_get_backend(C));

  Btype *bi64t = be->integer_type(false, 64);
  Btype *bi32t = be->integer_type(false, 32);

  // func foo(i1, i2 int32) int64 { }
  BFunctionType *befty =
      mkFuncTyp(be.get(), L_PARM, bi32t, L_PARM, bi32t, L_RES, bi64t, L_END);

  // FIXME: this is not supported yet.
  bool in_unique_section = false;

  const bool is_declaration = true;
  const bool is_visible[2] = {true, false};
  const bool is_inlinable[2] = {true, false};
  bool split_stack[2] = {true, false};
  bool is_noret[2] = {true, false};
  Location loc;
  unsigned count = 0;
  for (auto vis : is_visible) {
    for (auto inl : is_inlinable) {
      for (auto split : split_stack) {
        for (auto noret : is_noret) {
          std::stringstream ss;
          ss << "fcn" << count++;
          Bfunction *befcn =
              be->function(befty, "_foo", ss.str(), vis, is_declaration,
                           inl, split, noret, in_unique_section, loc);
          llvm::Function *llfunc = befcn->function();
          ASSERT_TRUE(llfunc != NULL);
          EXPECT_EQ(llfunc->getName(), ss.str());
          EXPECT_FALSE(llfunc->isVarArg());
          EXPECT_EQ(llfunc->hasFnAttribute(Attribute::NoInline), !inl);
          EXPECT_EQ(llfunc->hasFnAttribute(Attribute::NoReturn), noret);
          EXPECT_EQ(llfunc->hasExternalLinkage(), vis);
          EXPECT_EQ(llfunc->hasInternalLinkage(), !vis);
          EXPECT_EQ(befcn->splitStack() == Bfunction::YesSplit, !split);
        }
      }
    }
  }

  // Error function
  Bfunction *be_error_fcn = be->error_function();
  ASSERT_TRUE(be_error_fcn != NULL);

  // Try to create a function with an error type -- we should
  // get back error_function
  Bfunction *mistake = be->function(be->error_type(), "bad", "bad", true, true,
                                    false, false, false, false, loc);
  EXPECT_EQ(mistake, be_error_fcn);
}

TEST(BackendFcnTests, BuiltinFunctionsMisc) {
  LLVMContext C;

  std::unique_ptr<Backend> be(go_get_backend(C));

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

TEST(BackendFcnTests, BuiltinFunctionsTrig) {
  LLVMContext C;

  std::unique_ptr<Backend> be(go_get_backend(C));

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

TEST(BackendFcnTests, MakeBlocks) {
  LLVMContext C;

  std::unique_ptr<Backend> be(go_get_backend(C));
  Bfunction *bfcn = mkFunci32o64(be.get(), "foo");
  const std::vector<Bvariable *> vars;
  Bblock *bb = be->block(bfcn, nullptr, vars, Location(), Location());
  ASSERT_TRUE(bb != nullptr);
}

TEST(BackendFcnTests, MakeFuncWithRecursiveTypeParam) {

  // Create empty function
  FcnTestHarness h;
  Llvm_backend *be = h.be();
  Location loc;

  // type P *P
  Btype *cpht = be->placeholder_pointer_type("ph", loc, false);
  Btype *cpt = be->circular_pointer_type(cpht, false);
  be->set_placeholder_pointer_type(cpht, cpt);

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

  const char *exp = R"RAW_RESULT(
    )RAW_RESULT";

  bool isOK = h.expectBlock(exp);
  EXPECT_TRUE(isOK && "Block does not have expected contents");

  bool broken = h.finish(PreserveDebugInfo);
  EXPECT_FALSE(broken && "Module failed to verify.");
}

TEST(BackendFcnTests, MakeMultipleDeclarations) {

  FcnTestHarness h("foo");
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
  bool is_visible = true;
  bool is_declaration = true;
  bool is_inl = true;
  bool is_splitstack = true;
  bool in_unique_section = false;
  bool is_noret = false;
  Bfunction *bf1 =
      be->function(befty1, "_foo", "bar", is_visible, is_declaration,
                   is_inl, is_splitstack, is_noret, in_unique_section, loc);
  Bfunction *bf2 =
      be->function(befty1, "_foo", "bar", is_visible, is_declaration,
                   is_inl, is_splitstack, is_noret, in_unique_section, loc);
  Bfunction *bf3 =
      be->function(befty1, "_foo", "bar", is_visible, !is_declaration,
                   is_inl, is_splitstack, is_noret, in_unique_section, loc);
  Bfunction *bf4 =
      be->function(befty2, "_foo", "bar", is_visible, is_declaration,
                   is_inl, is_splitstack, is_noret, in_unique_section, loc);
  EXPECT_EQ(bf1, bf2);
  EXPECT_NE(bf1, bf3);
  EXPECT_NE(bf2, bf4);

  bool broken = h.finish(PreserveDebugInfo);
  EXPECT_FALSE(broken && "Module failed to verify.");
}

TEST(BackendFcnTests, TestIntrinsicCall) {
  FcnTestHarness h("foo");
  Llvm_backend *be = h.be();
  Location loc;

  // var x uint64
  Btype *bu64t = be->integer_type(true, 64);
  Bvariable *x = h.mkLocal("x", bu64t);

  // __builtin_ctzll(x);
  Bfunction *bfcn = be->lookup_builtin("__builtin_ctzll");
  //Bexpression *fnexpr = be->function_code_expression(bfcn, loc);
  Bexpression *ve = be->var_expression(x, loc);
  Bexpression *call = h.mkCallExpr(be, bfcn, ve, nullptr);
  h.mkExprStmt(call);

  const char *exp = R"RAW_RESULT(
    store i64 0, i64* %x
    %x.ld.0 = load i64, i64* %x
    %call.0 = call i64 @llvm.cttz.i64(i64 %x.ld.0, i1 true)
  )RAW_RESULT";

  bool isOK = h.expectBlock(exp);
  EXPECT_TRUE(isOK && "Block does not have expected contents");

  bool broken = h.finish(StripDebugInfo);
  EXPECT_FALSE(broken && "Module failed to verify.");
}

TEST(BackendFcnTests, TestCallMemBuiltins) {
  FcnTestHarness h("foo");
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
      h.mkCallExpr(be, bmemcpy,
                   be->address_expression(vey, loc),
                   be->address_expression(vex, loc),
                   mkUint64Const(be, be->type_size(bu64t)),
                   nullptr);
  h.mkExprStmt(call);
  }

  const char *exp = R"RAW_RESULT(
  store i64 0, i64* %x
  store i64 10101, i64* %y
  %cast.0 = bitcast i64* %x to i8*
  %cast.1 = bitcast i64* %y to i8*
  %call.0 = call i32 @memcmp(i8* %cast.0, i8* %cast.1, i64 8)
  %cast.2 = bitcast i64* %x to i8*
  %cast.3 = bitcast i64* %y to i8*
  call void @llvm.memmove.p0i8.p0i8.i64(i8* %cast.2, i8* %cast.3, i64 8, i1 false)
  %cast.4 = bitcast i64* %y to i8*
  %cast.5 = bitcast i64* %x to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %cast.4, i8* %cast.5, i64 8, i1 false)
  )RAW_RESULT";

  bool isOK = h.expectBlock(exp);
  EXPECT_TRUE(isOK && "Block does not have expected contents");

  bool broken = h.finish(StripDebugInfo);
  EXPECT_FALSE(broken && "Module failed to verify.");
}

TEST(BackendFcnTests, TestMultipleExternalFcnsWithSameName) {
  FcnTestHarness h("foo");
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
  bool visible = true;
  bool is_declaration = true;
  bool is_inl = true;
  bool split_stack = false;
  bool unique_sec = false;
  bool no_ret = false;
  Bfunction *bf1 = be->function(btf1, "syscall", "syscall", visible,
                                is_declaration, is_inl,
                                split_stack, no_ret, unique_sec, loc);
  Bfunction *bf2 = be->function(btf2, "syscall", "syscall", visible,
                                is_declaration, is_inl,
                                split_stack, no_ret, unique_sec, loc);

  // Create calls to the functions

  // x := syscall64(64)
  Bexpression *call64 = h.mkCallExpr(be, bf1, mkInt64Const(be, 64), nullptr);
  h.mkLocal("x", bi64t, call64);

  // y := syscall32(32)
  Bexpression *call32 = h.mkCallExpr(be, bf2, mkInt32Const(be, 32), nullptr);
  h.mkLocal("y", bi32t, call32);

  const char *exp = R"RAW_RESULT(
     %call.0 = call i64 @syscall(i8* nest undef, i64 64)
     store i64 %call.0, i64* %x
     %call.1 = call i32 bitcast (i64 (i8*, i64)*
           @syscall to i32 (i8*, i32)*)(i8* nest undef, i32 32)
     store i32 %call.1, i32* %y
  )RAW_RESULT";

  bool isOK = h.expectBlock(exp);
  EXPECT_TRUE(isOK && "Block does not have expected contents");

  bool broken = h.finish(StripDebugInfo);
  EXPECT_FALSE(broken && "Module failed to verify.");
}

TEST(BackendFcnTests, TestDeclAndDefWithSameName) {
  FcnTestHarness h("foo");
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
  bool visible = true;
  bool is_inl = true;
  bool split_stack = false;
  bool unique_sec = false;
  bool no_ret = false;

  // bar() declaration and definition
  Bfunction *bf1 = be->function(btf1, "bar", "bar", visible,
                                true, // is_declaration
                                is_inl, split_stack, no_ret, unique_sec, loc);
  Bfunction *bf2 = be->function(btf1, "bar", "bar", visible,
                                false, // is_declaration
                                is_inl, split_stack, no_ret, unique_sec, loc);

  // baz() declaration and definition
  Bfunction *bf3 = be->function(btf2, "baz", "baz", visible,
                                true, // is_declaration
                                is_inl, split_stack, no_ret, unique_sec, loc);
  Bfunction *bf4 = be->function(btf3, "baz", "baz", visible,
                                false, // is_declaration
                                is_inl, split_stack, no_ret, unique_sec, loc);

  // Create calls to the functions
  Bexpression *call1 = h.mkCallExpr(be, bf1, nullptr);
  h.mkLocal("a", bi32t, call1);

  Bexpression *call2 = h.mkCallExpr(be, bf2, nullptr);
  h.mkLocal("b", bi32t, call2);

  Bexpression *call3 = h.mkCallExpr(be, bf3, nullptr);
  h.mkLocal("x", bps0t, call3);

  Bexpression *call4 = h.mkCallExpr(be, bf4, nullptr);
  h.mkLocal("y", bps1t, call4);

  const char *exp = R"RAW_RESULT(
    %call.0 = call i32 @bar(i8* nest undef)
    store i32 %call.0, i32* %a
    %call.1 = call i32 @bar(i8* nest undef)
    store i32 %call.1, i32* %b
    %call.2 = call {}* bitcast ({ i32 }* (i8*)* @baz to {}* (i8*)*)(i8* nest undef)
    store {}* %call.2, {}** %x
    %call.3 = call { i32 }* @baz(i8* nest undef)
    store { i32 }* %call.3, { i32 }** %y
  )RAW_RESULT";

  bool isOK = h.expectBlock(exp);
  EXPECT_TRUE(isOK && "Block does not have expected contents");

  bool broken = h.finish(StripDebugInfo);
  EXPECT_FALSE(broken && "Module failed to verify.");
}

}
