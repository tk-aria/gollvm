//===- llvm/tools/gollvm/unittests/BackendCore/BackendTreeIntegrity.cpp -===//
//
// Copyright 2018 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
//
//===----------------------------------------------------------------------===//

#include "TestUtils.h"
#include "go-llvm-backend.h"
#include "gtest/gtest.h"

using namespace llvm;
using namespace goBackendUnitTests;

namespace {

TEST(BackendTreeIntegrity, CheckTreeIntegrity1) {
  FcnTestHarness h("foo");
  Llvm_backend *be = h.be();
  Bfunction *func = h.func();
  Location loc;

  // So that we can test the checker itself
  be->disableIntegrityChecks();

  // Create "2 + x"
  Btype *bi64t = be->integer_type(false, 64);
  Bvariable *xv = h.mkLocal("x", bi64t);
  Bexpression *vex = be->var_expression(xv, loc);
  Bexpression *bl1 = mkInt64Const(be, 2);
  Bexpression *badd1 = be->binary_expression(OPERATOR_PLUS, bl1, vex, loc);
  Bstatement *es = be->expression_statement(func, badd1);
  h.addStmt(es);

  // Grab pointer to the add expr above, post-materialization.
  badd1 = es->children()[0]->castToBexpression();

  // Create "4"
  Bexpression *b4 = mkInt64Const(be, 4);
  Bstatement *es2 = be->expression_statement(func, b4);
  h.addStmt(es2);

  // Mangle the IR so that we have a some instructions
  // parented by more than one Bexpression. Warning to our viewers at
  // home -- don't do this.
  for (auto inst : badd1->instructions())
    b4->appendInstruction(inst);

  TreeIntegCtl control(NoDumpPointers, ReportRepairableSharing, BatchMode);
  std::pair<bool, std::string> result =
      be->checkTreeIntegrity(h.block(), control);
  EXPECT_FALSE(result.first);
  EXPECT_TRUE(containstokens(result.second,
                             "instruction has multiple parents"));

  // Undo the mangling to avoid asserts later on
  b4->clear();

  h.finish(PreserveDebugInfo);
}

TEST(BackendTreeIntegrity, CheckTreeIntegrity2) {

  // Add the same Expression to more than one statement
  LLVMContext C;
  std::unique_ptr<Llvm_backend> be(new Llvm_backend(C, nullptr, nullptr));
  be->disableIntegrityChecks();

  Location loc;
  Bfunction *func = mkFunci32o64(be.get(), "foo");
  Btype *bi64t = be->integer_type(false, 64);
  Bvariable *loc1 = be->local_variable(func, "loc1", bi64t, nullptr, true, loc);
  Bvariable *loc2 = be->local_variable(func, "loc2", bi64t, nullptr, true, loc);

  // Create "loc1" varexpr, then supply to more than one statement
  Bexpression *ve = be->var_expression(loc1, loc);
  Bstatement *es1 = be->expression_statement(func, ve);
  Bblock *block = mkBlockFromStmt(be.get(), func, es1);
  Bstatement *es2 = be->expression_statement(func, ve);
  addStmtToBlock(be.get(), block, es2);

  TreeIntegCtl control(NoDumpPointers, ReportRepairableSharing, BatchMode);
  std::pair<bool, std::string> result =
      be->checkTreeIntegrity(block, control);
  EXPECT_FALSE(result.first);
  EXPECT_TRUE(containstokens(result.second, "expr has multiple parents"));

  be->nodeBuilder().destroy(block, DelBoth);

  Bexpression *ve3 = be->var_expression(loc2, loc);
  Bstatement *es3 = be->expression_statement(func, ve3);
  Bblock *block2 = mkBlockFromStmt(be.get(), func, es3);

  be->disableDebugMetaDataGeneration();

  be->function_set_body(func, block2);
}

TEST(BackendTreeIntegrity, CheckTreeIntegrity3) {

  // Same statement with more than one parent.
  LLVMContext C;
  std::unique_ptr<Llvm_backend> be(new Llvm_backend(C, nullptr, nullptr));
  be->disableIntegrityChecks();
  Location loc;
  Bfunction *func = mkFunci32o64(be.get(), "foo");

  // Create expr stmt, add to block more than once
  Bexpression *b2 = mkInt64Const(be.get(), 2);
  Bstatement *es = be->expression_statement(func, b2);
  Bblock *block = mkBlockFromStmt(be.get(), func, es);
  addStmtToBlock(be.get(), block, es);

  TreeIntegCtl control(NoDumpPointers, ReportRepairableSharing, BatchMode);
  std::pair<bool, std::string> result =
      be->checkTreeIntegrity(block, control);
  EXPECT_FALSE(result.first);
  EXPECT_TRUE(containstokens(result.second, "stmt has multiple parents"));

  Bexpression *b3 = mkInt64Const(be.get(), 3);
  Bstatement *es2 = be->expression_statement(func, b3);
  Bblock *block2 = mkBlockFromStmt(be.get(), func, es2);

  be->disableDebugMetaDataGeneration();

  be->function_set_body(func, block2);
}

TEST(BackendTreeIntegrity, CheckTreeIntegrityRepairableSubtree) {
  FcnTestHarness h;
  Llvm_backend *be = h.be();
  Btype *bi32t = be->integer_type(false, 32);
  Btype *bpi32t = be->pointer_type(bi32t);
  BFunctionType *befty = mkFuncTyp(be, L_PARM, bpi32t, L_END);
  Bfunction *func = h.mkFunction("x", befty);
  Location loc;

  TreeIntegCtl ctl(NoDumpPointers, ReportRepairableSharing, BatchMode);
  IntegrityVisitor ivis(be, ctl);

  // *p0 + 2
  Bvariable *p0v = func->getNthParamVar(0);
  Bexpression *vex0 = be->var_expression(p0v, loc);
  Bexpression *deref = be->indirect_expression(bi32t, vex0, false, loc);
  Bexpression *add =
      be->binary_expression(OPERATOR_PLUS, mkInt32Const(be, 2), deref, loc);
  EXPECT_TRUE(ivis.repairableSubTree(vex0));
  EXPECT_TRUE(ivis.repairableSubTree(deref));
  EXPECT_TRUE(ivis.repairableSubTree(add));

  // p0 == nil ? 1 : *p0
  Bexpression *vex2 = be->var_expression(p0v, loc);
  Bexpression *npe = be->nil_pointer_expression();
  Bexpression *cmp =
      be->binary_expression(OPERATOR_EQEQ, npe, vex2, loc);
  Bexpression *der2 = be->indirect_expression(bi32t, vex2, false, loc);
  Bexpression *const1 = mkInt32Const(be, 1);
  Bexpression *condex = be->conditional_expression(func, bi32t, cmp, const1,
                                                   der2, loc);
  EXPECT_TRUE(ivis.repairableSubTree(cmp));
  EXPECT_TRUE(ivis.repairableSubTree(condex));

  // Not legal to share calls (in general)
  Bexpression *vex3 = be->var_expression(p0v, loc);
  Bexpression *call2 = h.mkCallExpr(be, func, vex3, nullptr);
  EXPECT_FALSE(ivis.repairableSubTree(call2));

  // Create runtime error function.
  const char *rtename = "__go_runtime_error";
  BFunctionType *bfterr = mkFuncTyp(be,
                                    L_PARM, bi32t,
                                    L_END);
  unsigned fflags = (Backend::function_is_visible |
                     Backend::function_is_inlinable |
                     Backend::function_does_not_return |
                     Backend::function_is_declaration);
  Bfunction *rtefcn = be->function(bfterr, rtename, rtename, fflags, loc);

  // p0 != nil ? *p0 + 3 : runtime_error(6)
  Bexpression *cmp2 =
      be->binary_expression(OPERATOR_NOTEQ, vex3, npe, loc);
  Bexpression *der3 = be->indirect_expression(bi32t, vex3, false, loc);
  Bexpression *add2 =
      be->binary_expression(OPERATOR_PLUS, mkInt32Const(be, 3), der3, loc);
  Bexpression *const6 = mkInt32Const(be, 6);
  Bexpression *call3 = h.mkCallExpr(be, rtefcn, const6, nullptr);
  Bexpression *condex2 = be->conditional_expression(func, bi32t, cmp2, add2,
                                                   call3, loc);

  // Runtime error calls are ok to unshare/duplicate.
  EXPECT_TRUE(ivis.repairableSubTree(call3));
  EXPECT_TRUE(ivis.repairableSubTree(condex2));

  h.mkExprStmt(call2);
  h.mkExprStmt(condex);
  h.mkExprStmt(condex2);
  h.mkExprStmt(add);

  bool broken = h.finish(StripDebugInfo);
  EXPECT_FALSE(broken && "Module failed to verify.");
}

}
