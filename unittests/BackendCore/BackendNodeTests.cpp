//===- llvm/tools/gollvm/unittests/BackendCore/BackendNodeTests.cpp -----===//
//
// Copyright 2018 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
//
//===----------------------------------------------------------------------===//

#include "go-llvm-bnode.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Function.h"
#include "operator.h"
#include "gtest/gtest.h"
#include "TestUtils.h"
#include "DiffUtils.h"

using namespace goBackendUnitTests;

namespace {

class BackendNodeTests : public testing::TestWithParam<llvm::CallingConv::ID> {
};

INSTANTIATE_TEST_CASE_P(
    UnitTest, BackendNodeTests,
    goBackendUnitTests::CConvs,
    [](const testing::TestParamInfo<BackendNodeTests::ParamType> &info) {
      std::string name = goBackendUnitTests::ccName(info.param);
      return name;
    });

class SimpleVisitor {
 public:
  SimpleVisitor(unsigned stopAt = 0xffffffff) : stopAt_(stopAt) {}

  std::pair<VisitDisp, Bnode *> visitNodePre(Bnode *node) {
    ss_ << "node " << node->flavstr() << " pre " << id(node) << "\n";
    VisitDisp disp = (stopAt_ == id(node) ? StopWalk : ContinueWalk);
    return std::make_pair(disp, node);
  }
  std::pair<VisitDisp, Bnode *> visitNodePost(Bnode *node) {
    ss_ << "node " << node->flavstr() << " post " << id(node) << "\n";
    VisitDisp disp = (stopAt_ == id(node) ? StopWalk : ContinueWalk);
    return std::make_pair(disp, node);
  }

  std::pair< std::pair<VisitDisp, VisitChildDisp>, Bnode *>
  visitChildPre(Bnode *parent, Bnode *child) {
    ss_ << "pre child " << child->flavstr() << " " << id(parent) << " " << id(child) << "\n";
    VisitDisp disp = (stopAt_ == id(child) ? StopWalk : ContinueWalk);
    return std::make_pair(std::make_pair(disp, VisitChild), child);
  }
  std::pair<VisitDisp, Bnode *> visitChildPost(Bnode *parent, Bnode *child) {
    ss_ << "post child " << child->flavstr() << " " << id(parent) << " " << id(child) << "\n";
    VisitDisp disp = (stopAt_ == id(child) ? StopWalk : ContinueWalk);
    return std::make_pair(disp, child);
  }

  std::string str() const { return ss_.str(); }

  unsigned id(Bnode *node) {
    return node->id();
  }

 private:
  std::stringstream ss_;
  unsigned stopAt_;
};

TEST_P(BackendNodeTests, VerifyVisitorBehavior) {
  auto cc = GetParam();
  FcnTestHarness h(cc, "foo");
  Llvm_backend *be = h.be();
  Location loc;

  SimpleVisitor vis;

  Btype *bi32t = be->integer_type(false, 32);
  Btype *bpi32t = be->pointer_type(bi32t);
  Bexpression *c22 = mkInt32Const(be, 22);
  Bvariable *xv = h.mkLocal("x", bi32t);
  Bvariable *yv = h.mkLocal("y", bpi32t);
  Bexpression *ve = be->var_expression(xv, loc);
  Bexpression *add = be->binary_expression(OPERATOR_PLUS, c22, ve, loc);
  Bexpression *ve2 = be->var_expression(yv, loc);
  Bexpression *der = be->indirect_expression(bi32t, ve2, false, loc);
  Bexpression *sub = be->binary_expression(OPERATOR_MINUS, add, der, loc);
  Bexpression *matsub = be->materialize(sub);

  Bnode *res1 = update_walk_nodes(matsub, vis);

  EXPECT_EQ(res1, matsub);

  DECLARE_EXPECTED_OUTPUT(exp, R"RAW_RESULT(
    node - pre 18
    pre child + 18 15
    node + pre 15
    pre child const 15 1
    node const pre 1
    node const post 1
    post child const 15 1
    pre child deref 15 14
    node deref pre 14
    pre child var 14 8
    node var pre 8
    node var post 8
    post child var 14 8
    node deref post 14
    post child deref 15 14
    node + post 15
    post child + 18 15
    pre child deref 18 17
    node deref pre 17
    pre child deref 17 16
    node deref pre 16
    pre child var 16 10
    node var pre 10
    node var post 10
    post child var 16 10
    node deref post 16
    post child deref 17 16
    node deref post 17
    post child deref 18 17
    node - post 18
  )RAW_RESULT");

  std::string reason;
  bool equal = difftokens(exp.content, vis.str(), reason);
  EXPECT_EQ("pass", equal ? "pass" : reason);
  if (!equal) {
    std::cerr << "expected dump:\n" << exp.content << "\n";
    std::cerr << "result dump:\n" << vis.str() << "\n";
  }

  h.mkExprStmt(matsub);

  bool broken = h.finish(PreserveDebugInfo);
  EXPECT_FALSE(broken && "Module failed to verify.");
}

TEST_P(BackendNodeTests, CloneSubtree) {
  auto cc = GetParam();
  FcnTestHarness h(cc, "foo");
  Llvm_backend *be = h.be();
  Location loc = h.loc();

  // (x - int32(z)) + *y
  Btype *bi16t = be->integer_type(false, 16);
  Btype *bi32t = be->integer_type(false, 32);
  Btype *bpi32t = be->pointer_type(bi32t);
  Bvariable *xv = h.mkLocal("x", bi32t);
  Bvariable *yv = h.mkLocal("y", bpi32t);
  Bvariable *zv = h.mkLocal("z", bi16t);
  Bexpression *vex = be->var_expression(xv, loc);
  Bexpression *vez = be->var_expression(zv, loc);
  Bexpression *conv = be->convert_expression(bi32t, vez, loc);
  Bexpression *sub = be->binary_expression(OPERATOR_PLUS, vex, conv, loc);
  Bexpression *vey = be->var_expression(yv, loc);
  Bexpression *der = be->indirect_expression(bi32t, vey, false, loc);
  Bexpression *add = be->binary_expression(OPERATOR_PLUS, sub, der, loc);
  Bexpression *matadd = be->materialize(add);
  Bexpression *matclone = be->nodeBuilder().cloneSubtree(matadd);
  EXPECT_NE(add, matclone);

  DECLARE_EXPECTED_OUTPUT(exp, R"RAW_RESULT(
    %x.ld.0 = load i32, i32* %x, align 4
    %z.ld.0 = load i16, i16* %z, align 2
    %sext.0 = sext i16 %z.ld.0 to i32
    %add.0 = add i32 %x.ld.0, %sext.0
    %y.ld.0 = load i32*, i32** %y, align 8
    %.ld.0 = load i32, i32* %y.ld.0, align 4
    %add.1 = add i32 %add.0, %.ld.0
  )RAW_RESULT");

  bool isOK = h.expectRepr(matadd, exp);
  EXPECT_TRUE(isOK && "expr does not have expected contents");
  isOK = h.expectRepr(matclone, exp);
  EXPECT_TRUE(isOK && "cloned expr does not have expected contents");

  h.mkExprStmt(matadd);
  h.mkExprStmt(matclone);

  bool broken = h.finish(PreserveDebugInfo);
  EXPECT_FALSE(broken && "Module failed to verify.");
}

TEST_P(BackendNodeTests, FixSharing) {
  auto cc = GetParam();
  FcnTestHarness h(cc, "foo");
  Llvm_backend *be = h.be();
  Location loc = h.loc();

  Btype *bi32t = be->integer_type(false, 32);
  Btype *bpi32t = be->pointer_type(bi32t);
  Btype *s2t = mkBackendStruct(be, bpi32t, "f1", bi32t, "f2", nullptr);
  Btype *s4t = mkBackendStruct(be, s2t, "f1", s2t, "f2", nullptr);
  Bvariable *xv = h.mkLocal("x", s4t);

  // y = x.f1.f1 + x.f1.f1 [with sharing]
  Bexpression *vex = be->var_expression(xv, loc);
  Bexpression *f1ex = be->struct_field_expression(vex, 0, loc);
  Bexpression *f2ex = be->struct_field_expression(f1ex, 0, loc);
  Bexpression *der = be->indirect_expression(bi32t, f2ex, false, loc);
  Bexpression *add = be->binary_expression(OPERATOR_PLUS, der, der, loc);
  Bexpression *matadd = be->materialize(add);

  DECLARE_EXPECTED_OUTPUT(exp2, R"RAW_RESULT(
    %field.0 = getelementptr inbounds { { i32*, i32 }, { i32*, i32 } }, { { i32*, i32 }, { i32*, i32 } }* %x, i32 0, i32 0
    %field.1 = getelementptr inbounds { i32*, i32 }, { i32*, i32 }* %field.0, i32 0, i32 0
    %x.field.field.ld.0 = load i32*, i32** %field.1, align 8
    %.ld.0 = load i32, i32* %x.field.field.ld.0, align 4
    %field.2 = getelementptr inbounds { { i32*, i32 }, { i32*, i32 } }, { { i32*, i32 }, { i32*, i32 } }* %x, i32 0, i32 0
    %field.3 = getelementptr inbounds { i32*, i32 }, { i32*, i32 }* %field.2, i32 0, i32 0
    %.field.field.ld.0 = load i32*, i32** %field.3, align 8
    %.ld.1 = load i32, i32* %.field.field.ld.0, align 4
    %add.0 = add i32 %.ld.0, %.ld.1
  )RAW_RESULT");

  bool isOK = h.expectRepr(matadd, exp2);
  EXPECT_TRUE(isOK && "expr does not have expected contents");

  h.mkExprStmt(matadd);

  bool broken = h.finish(PreserveDebugInfo);
  EXPECT_FALSE(broken && "Module failed to verify.");
}

} // namespace
