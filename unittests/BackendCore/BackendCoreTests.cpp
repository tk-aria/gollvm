//===- llvm/tools/gollvm/unittests/BackendCore/BackendCoreTests.cpp -----===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "TestUtils.h"
#include "go-llvm-backend.h"
#include "gtest/gtest.h"

using namespace llvm;
using namespace goBackendUnitTests;

namespace {

TEST(BackendCoreTests, MakeBackend) {
  LLVMContext C;

  std::unique_ptr<Backend> makeit(go_get_backend(C));
}

TEST(BackendCoreTests, ScalarTypes) {
  LLVMContext C;

  std::unique_ptr<Backend> be(go_get_backend(C));

  Btype *et = be->error_type();
  EXPECT_TRUE(et != nullptr);
  Btype *vt = be->void_type();
  EXPECT_TRUE(vt != nullptr);
  EXPECT_TRUE(vt != et);
  Btype *bt = be->bool_type();
  EXPECT_TRUE(bt != nullptr);
  Btype *pbt = be->pointer_type(bt);
  ASSERT_TRUE(pbt != nullptr);
  EXPECT_TRUE(pbt->type()->isPointerTy());

  std::vector<bool> isuns = {false, true};
  std::vector<int> ibits = {8, 16, 32, 64, 128};
  for (auto uns : isuns) {
    for (auto nbits : ibits) {
      Btype *it = be->integer_type(uns, nbits);
      ASSERT_TRUE(it != nullptr);
      EXPECT_TRUE(it->type()->isIntegerTy());
    }
  }

  std::vector<int> fbits = {32, 64, 128};
  for (auto nbits : fbits) {
    Btype *ft = be->float_type(nbits);
    ASSERT_TRUE(ft != nullptr);
    EXPECT_TRUE(ft->type()->isFloatingPointTy());
  }
}

TEST(BackendCoreTests, StructTypes) {
  LLVMContext C;

  std::unique_ptr<Backend> be(go_get_backend(C));

  // Empty struct
  std::vector<Backend::Btyped_identifier> nofields;
  Btype *emptyst = be->struct_type(nofields);
  SmallVector<Type *, 3> smv_empty(0);
  Type *llvm_emptyst = StructType::get(C, smv_empty);
  ASSERT_TRUE(llvm_emptyst != nullptr);
  ASSERT_TRUE(emptyst != nullptr);
  EXPECT_EQ(llvm_emptyst, emptyst->type());

  // Three-field struct
  Btype *best = mkBackendThreeFieldStruct(be.get());
  Type *llst = mkLlvmThreeFieldStruct(C);
  ASSERT_TRUE(best != nullptr);
  ASSERT_TRUE(llst != nullptr);
  EXPECT_EQ(llst, best->type());
  EXPECT_EQ(repr(best->type()), "{ i8, float*, i64 }");

  // If a field has error type, entire struct has error type
  std::vector<Backend::Btyped_identifier> fields = {
      Backend::Btyped_identifier("f1", be->bool_type(), Location()),
      Backend::Btyped_identifier("fe", be->error_type(), Location())};
  Btype *badst = be->struct_type(fields);
  EXPECT_TRUE(badst != nullptr);
  EXPECT_EQ(badst, be->error_type());

  // Llvm_backend should be caching and reusing anonymous types
  Btype *st1 = mkBackendThreeFieldStruct(be.get());
  Btype *st2 = mkBackendThreeFieldStruct(be.get());
  EXPECT_EQ(st1, st2);
}

TEST(BackendCoreTests, TypeHashAndCompare) {
  LLVMContext C;

  std::unique_ptr<Backend> be(go_get_backend(C));

  Btype *st1 = mkBackendThreeFieldStruct(be.get());
  Btype *st2 = mkBackendThreeFieldStruct(be.get());
  EXPECT_EQ(st1->hash(), st2->hash());
  EXPECT_TRUE(st1->equal(*st1));
}

TEST(BackendCoreTests, ComplexTypes) {
  LLVMContext C;

  Type *ft = Type::getFloatTy(C);
  Type *dt = Type::getDoubleTy(C);

  std::unique_ptr<Backend> be(go_get_backend(C));
  Btype *c32 = be->complex_type(64);
  ASSERT_TRUE(c32 != nullptr);
  EXPECT_EQ(c32->type(), mkTwoFieldLLvmStruct(C, ft, ft));
  Btype *c64 = be->complex_type(128);
  ASSERT_TRUE(c64 != nullptr);
  EXPECT_EQ(c64->type(), mkTwoFieldLLvmStruct(C, dt, dt));
}

TEST(BackendCoreTests, FunctionTypes) {
  LLVMContext C;

  Type *i64t = IntegerType::get(C, 64);

  std::unique_ptr<Backend> be(go_get_backend(C));

  // func foo() {}
  Btype *emptyf = mkFuncTyp(be.get(), L_END);
  Type *llemptyf = mkLLFuncTyp(&C, L_END);
  ASSERT_TRUE(llemptyf != nullptr && emptyf != nullptr);
  EXPECT_TRUE(llemptyf == emptyf->type());

  {
    // func (Blah) foo() {}
    Btype *pt = be->pointer_type(mkBackendThreeFieldStruct(be.get()));
    Btype *befn =
        mkFuncTyp(be.get(), L_RCV, pt, L_END);
    llvm::PointerType *llpt =
        llvm::PointerType::get(mkLlvmThreeFieldStruct(C), 0);
    Type *llfn = mkLLFuncTyp(&C, L_RCV, llpt, L_END);
    ASSERT_TRUE(befn != nullptr && llfn != nullptr);
    EXPECT_TRUE(befn->type() == llfn);
  }

  {
    // func foo(x int64) {}
    Btype *befn =
        mkFuncTyp(be.get(), L_PARM, be->integer_type(false, 64), L_END);
    Type *llfn = mkLLFuncTyp(&C, L_PARM, i64t, L_END);
    ASSERT_TRUE(befn != nullptr && llfn != nullptr);
    EXPECT_TRUE(befn->type() == llfn);
  }

  {
    // func foo() int64 {}
    Btype *befn =
        mkFuncTyp(be.get(), L_RES, be->integer_type(false, 64), L_END);
    Type *llfn = mkLLFuncTyp(&C, L_RES, i64t, L_END);
    ASSERT_TRUE(befn != nullptr && llfn != nullptr);
    EXPECT_TRUE(befn->type() == llfn);
  }
}

TEST(BackendCoreTests, PlaceholderTypes) {
  LLVMContext C;

  std::unique_ptr<Backend> be(go_get_backend(C));

  // Create a placeholder pointer type
  Location loc;
  Btype *phpt1 = be->placeholder_pointer_type("ph1", loc, false);
  ASSERT_TRUE(phpt1 != nullptr);
  EXPECT_TRUE(phpt1->type()->isPointerTy());

  // Placeholder pointer types should not be cached
  Btype *phpt2 = be->placeholder_pointer_type("ph", loc, false);
  Btype *phpt3 = be->placeholder_pointer_type("ph", loc, false);
  ASSERT_TRUE(phpt2 != phpt3);
  EXPECT_TRUE(phpt2->type() != phpt3->type());

  // Replace placeholder pointer type
  Btype *pst = be->pointer_type(mkBackendThreeFieldStruct(be.get()));
  be->set_placeholder_pointer_type(phpt1, pst);
  ASSERT_TRUE(phpt1->type()->isPointerTy());
  PointerType *llpt = cast<PointerType>(phpt1->type());
  EXPECT_TRUE(llpt->getElementType()->isStructTy());

  // Placeholder struct type
  Btype *phst1 = be->placeholder_struct_type("ph", loc);
  ASSERT_TRUE(phpt1 != nullptr);

  // Replace placeholder struct type
  std::vector<Backend::Btyped_identifier> fields = {
      Backend::Btyped_identifier("f1", be->integer_type(false, 64), Location()),
      Backend::Btyped_identifier("f2", be->integer_type(false, 64),
                                 Location())};
  be->set_placeholder_struct_type(phst1, fields);
  Type *i64t = IntegerType::get(C, 64);
  EXPECT_TRUE(llvmTypesEquiv(phst1->type(),
                             mkTwoFieldLLvmStruct(C, i64t, i64t)));

  // Placeholder array type
  Btype *phat1 = be->placeholder_array_type("pha", loc);
  ASSERT_TRUE(phat1 != nullptr);

  // Replace placeholder array type
  Btype *bi64t = be->integer_type(false, 64);
  Bexpression *val10 = mkInt64Const(be.get(), int64_t(10));
  Btype *at10 = be->array_type(bi64t, val10);
  ASSERT_TRUE(at10 != nullptr);
  be->set_placeholder_array_type(phat1, bi64t, val10);
  EXPECT_TRUE(phat1->type() == at10->type());

  // Circular pointer support
  Btype *php4 = be->placeholder_pointer_type("ph", loc, false);
  Btype *cpt = be->circular_pointer_type(php4, false);
  Btype *cpt2 = be->circular_pointer_type(php4, false);
  EXPECT_EQ(cpt, cpt2);
  be->set_placeholder_pointer_type(php4, cpt);
  EXPECT_EQ(php4->type(), cpt->type());
}

TEST(BackendCoreTests, ArrayTypes) {
  LLVMContext C;

  std::unique_ptr<Backend> be(go_get_backend(C));
  Location loc;

  // Very basic tests of array type creation: array of integers
  Btype *bi32t = be->integer_type(false, 32);
  Btype *bi64t = be->integer_type(false, 64);
  Bexpression *val10 = mkInt64Const(be.get(), int64_t(10));
  Btype *at10 = be->array_type(bi64t, val10);
  ASSERT_TRUE(at10 != nullptr);
  EXPECT_EQ(at10->type(), llvm::ArrayType::get(bi64t->type(), 10));

  // Array of structs
  Bexpression *val1023 = mkUint64Const(be.get(), uint64_t(1023));
  Btype *st = mkTwoFieldStruct(be.get(), bi32t, bi64t);
  Btype *at1023 = be->array_type(st, val1023);
  ASSERT_TRUE(at1023 != nullptr);
  llvm::Type *lst = mkTwoFieldLLvmStruct(C, bi32t->type(), bi64t->type());
  EXPECT_EQ(at1023->type(), llvm::ArrayType::get(lst, 1023));

  // Zero sized array
  Bexpression *val0 = mkInt64Const(be.get(), 0);
  Btype *at0 = be->array_type(bi64t, val0);
  ASSERT_TRUE(at0 != nullptr);

  // Error cases
  Bexpression *badval = be->error_expression();
  Btype *badt1 = be->array_type(bi64t, badval);
  EXPECT_EQ(badt1, be->error_type());
  Btype *badt2 = be->array_type(be->error_type(), val10);
  EXPECT_EQ(badt2, be->error_type());
}

TEST(BackendCoreTests, NamedTypes) {
  LLVMContext C;

  std::unique_ptr<Backend> be(go_get_backend(C));
  Location loc;
  Btype *nt = be->named_type("named_int32", be->integer_type(false, 32), loc);
  ASSERT_TRUE(nt != nullptr);
  Btype *nt2 =
      be->named_type("another_int32", be->integer_type(false, 32), loc);
  ASSERT_TRUE(nt2 != nullptr);
  EXPECT_TRUE(nt != nt2);
  EXPECT_TRUE(nt->equivalent(*nt2));
}

TEST(BackendCoreTests, TypeUtils) {
  LLVMContext C;
  Location loc;

  // Type size and alignment. Size and align are in bytes.
  std::unique_ptr<Backend> be(go_get_backend(C));
  Btype *i8t = be->integer_type(false, 8);
  EXPECT_EQ(be->type_size(i8t), int64_t(1));
  EXPECT_EQ(be->type_alignment(i8t), 1);

  // Slightly more complicated example
  Btype *u64 = be->integer_type(true, 64);
  Btype *st = mkTwoFieldStruct(be.get(), u64, u64);
  EXPECT_EQ(be->type_size(st), int64_t(16));
  EXPECT_EQ(be->type_alignment(st), 8);

  // Strictly speaking, one should be asking for the size
  // of a pointer-to-function type, not a function type, however
  // it does appear that this needs to be supported.
  Btype *emptyf = mkFuncTyp(be.get(), L_END);
  EXPECT_EQ(be->type_size(emptyf), be->type_size(be->pointer_type(u64)));

  // We need to support type size queries on types that are not fully
  // resolved, specifically struct types that incorporate unresolved
  // placeholder pointer fields.
  Btype *pvt = be->pointer_type(be->void_type());
  Btype *phpt = be->placeholder_pointer_type("ph1", loc, false);
  Btype *ptphpt = be->pointer_type(phpt);
  Btype *phst = be->placeholder_struct_type("ph2", loc);
  std::vector<Backend::Btyped_identifier> fields = {
    Backend::Btyped_identifier("f1", phpt, loc),
    Backend::Btyped_identifier("f2", ptphpt, loc)};
  be->set_placeholder_struct_type(phst, fields);
  Btype *rst = mkTwoFieldStruct(be.get(), pvt, pvt);
  EXPECT_EQ(be->type_size(rst), be->type_size(phst));

  // type field alignment
  Btype *u32 = be->integer_type(true, 32);
  EXPECT_EQ(be->type_field_alignment(u32), 4);
}

TEST(BackendCoreTests, TestTypeEquivalence) {
  LLVMContext C;

  std::unique_ptr<Backend> be(go_get_backend(C));

  // Two structs with same field type but different field names
  Btype *bi32t = be->integer_type(false, 32);
  Btype *pbi32t = be->pointer_type(bi32t);
  Btype *s1t = mkBackendStruct(be.get(), pbi32t, "f1", bi32t, "f2", nullptr);
  Btype *s2t = mkBackendStruct(be.get(), pbi32t, "f2", bi32t, "f1", nullptr);

  // Neither ::equal nor ::equivalent
  EXPECT_FALSE(s1t->equal(*s2t));
  EXPECT_FALSE(s1t->equivalent(*s2t));

  // Structs created via placholders
  Location loc;
  Btype *phst1 = be->placeholder_struct_type("ph1", loc);
  std::vector<Backend::Btyped_identifier> fields1 = {
    Backend::Btyped_identifier("f1", s1t, loc),
    Backend::Btyped_identifier("f2", s2t, loc)};
  be->set_placeholder_struct_type(phst1, fields1);

  Btype *phst2 = be->placeholder_struct_type("ph2", loc);
  std::vector<Backend::Btyped_identifier> fields2 = {
    Backend::Btyped_identifier("f1", s1t, loc),
    Backend::Btyped_identifier("f2", s2t, loc)};
  be->set_placeholder_struct_type(phst2, fields2);

  EXPECT_FALSE(phst1->equal(*phst2));
  EXPECT_TRUE(phst1->equivalent(*phst2));
}

TEST(BackendCoreTests, TestFcnPointerCompatible) {

  FcnTestHarness h("foo");
  Llvm_backend *be = h.be();
  TypeManager *tm = be->typeManager();
  Location loc;

  // Create a struct that looks like
  //
  //  struct S { int8, void(int8)*, *S }
  //
  Btype *pht = be->placeholder_pointer_type("ph", loc, false);
  Btype *bi8t = be->integer_type(false, 8);
  BFunctionType *befty1 = mkFuncTyp(be, L_PARM, bi8t, L_END);
  Btype *pft = be->pointer_type(befty1);
  Btype *s1t = mkBackendStruct(be, bi8t, "f1", pft, "f2", pht, "n", nullptr);
  Btype *ps1t = be->pointer_type(s1t);
  be->set_placeholder_pointer_type(pht, ps1t);
  h.mkLocal("y", s1t);

  // Create a struct that looks like
  //
  //  struct S { int8, void*, *S }
  //
  Btype *pht2 = be->placeholder_pointer_type("ph", loc, false);
  Btype *pvt = be->pointer_type(be->void_type());
  Btype *s2t = mkBackendStruct(be, bi8t, "f1", pvt, "f2", pht2, "n", nullptr);
  Btype *ps2t = be->pointer_type(s2t);
  be->set_placeholder_pointer_type(pht2, ps2t);
  h.mkLocal("x", s2t);

  // These two types are not structurally equivalent
  EXPECT_FALSE(pht2->equal(*pht));

  // However the underlying LLVM types should be considered
  // assignment-compatible.
  std::set<llvm::Type *> visited;
  bool equiv = tm->fcnPointerCompatible(pht->type(), pht2->type(), visited);
  EXPECT_TRUE(equiv);

  bool broken = h.finish(PreserveDebugInfo);
  EXPECT_FALSE(broken && "Module failed to verify.");
}

}
