//===- llvm/tools/gollvm/unittests/BackendCore/TestUtils.h --------------===//
//
// Copyright 2018 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
//
//===----------------------------------------------------------------------===//

#ifndef GOLLVM_UNITTESTS_BACKENDCORE_TESTUTILS_H
#define GOLLVM_UNITTESTS_BACKENDCORE_TESTUTILS_H

#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Instruction.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Value.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Support/raw_ostream.h"

// Currently these need to be included before backend.h
#include "go-linemap.h"
#include "go-location.h"

#include "backend.h"
#include "go-llvm.h"

#include "DiffUtils.h"

#include <stdarg.h>
#include "gtest/gtest.h"

namespace goBackendUnitTests {

// All supported calling conventions
auto CConvs = testing::Values(llvm::CallingConv::X86_64_SysV,
                              llvm::CallingConv::ARM_AAPCS);

// Convert llvm::CallingConv::ID to its coresponding string name.
std::string ccName(llvm::CallingConv::ID);

// Return string representation of LLVM value (handling null ptr)
std::string repr(llvm::Value *val);

// Return string representation of LLVM type (handling null ptr)
std::string repr(llvm::Type *t);

// Return string representation of Bstatement or Bexpression (handling
// null ptr). Currently only a subset of statement types are supported.
std::string repr(Bnode *node);

// Varargs helper for struct creation. Pass in pairs of
// type/name fields, ending with null ptr.
Btype *mkBackendStruct(Backend *be, ...);

// Similar to the above but with LLVM types (no field names)
llvm::StructType *mkLlvmStruct(llvm::LLVMContext *context, ...);

// Create this struct using backend interfaces:
//
//    struct { bool f1; float *f2; uint64_t f3; }
//
Btype *mkBackendThreeFieldStruct(Backend *be);

// Create this struct using LLVM interfaces:
//
//    struct { bool f1; float *f2; uint64_t f3; }
//
llvm::StructType *mkLlvmThreeFieldStruct(llvm::LLVMContext &context);

// Create a struct Btype with two fields, specified by t1 and t2
Btype *mkTwoFieldStruct(Backend *be, Btype *t1, Btype *t2);

// Create an LLVM struct type with two fields, specified by t1 and t2
llvm::Type *mkTwoFieldLLvmStruct(llvm::LLVMContext &context,
                                 llvm::Type *t1, llvm::Type *t2);

// Check two LLVM types for structural equality. Hacky, but it helps
// to have this for unit testing of placeholder struct types. Note
// that this ignores type names and type attributes (ex: whether a
// function type is varargs).
bool llvmTypesEquiv(llvm::Type *t1, llvm::Type *t2);

// Create Btyped_identifier from type (uses static counter
// to insure unique name each time).
Backend::Btyped_identifier mkid(Btype *t);

typedef enum {
  L_END = 0, // end of list
  L_RCV,     // receiver type follows
  L_PARM,    // arg type follows
  L_RES,     // res type follows
  L_RES_ST,  // res struct type follows
} MkfToken;

// Varargs helper to create Btype function type. The variable arguments
// are token-value pairs using for form above in MkfToken, with pairlist
// terminated by L_END. Example usage:
//
//      Btype *bi32t = be->integer_type(false, 32);
//      Btype *befn = mkFuncTyp(be.get(),
//                            L_PARM, bi32t,
//                            L_PARM, bi32t,
//                            L_RES, bi32t,
//                            L_END);
//
BFunctionType *mkFuncTyp(Backend *be, ...);

// Varargs helper to create llvm function type, similar to "mkFuncTyp"
// above (same token-value pair list).
llvm::Type *mkLLFuncTyp(llvm::LLVMContext *context, ...);

// Returns func:  fname(i1, i2 int32) int64 { }
Bfunction *mkFunci32o64(Backend *be, const char *fname, bool mkParams = true);

// Returns function created from type
Bfunction *mkFuncFromType(Backend *be, const char *fname,
                          BFunctionType *befty, Location loc = Location());

// Manufacture an unsigned 64-bit integer constant
Bexpression *mkUint64Const(Backend *be, uint64_t val);

// Manufacture a signed 64-bit integer constant
Bexpression *mkInt64Const(Backend *be, int64_t val);

// Manufacture a 64-bit float constant
Bexpression *mkFloat64Const(Backend *be, double val);

// Manufacture a signed 32-bit integer constant
Bexpression *mkInt32Const(Backend *be, int32_t val);

// Manufacture N-bit constants
Bexpression *mkIntConst(Backend *be, int64_t val, unsigned bits);
Bexpression *mkUIntConst(Backend *be, uint64_t val, unsigned bits);

// Return func desc type
Btype *mkFuncDescType(Backend *be);

// Create a function descriptor value for specified func
Bexpression *mkFuncDescExpr(Backend *be, Bfunction *fcn);

// Create a basic block from a single statement
Bblock *mkBlockFromStmt(Backend *be, Bfunction *func, Bstatement *st);

// Append stmt to block
Bstatement *addStmtToBlock(Backend *be, Bblock *block, Bstatement *st);

// Adds expression to block as expr statement.
Bstatement *addExprToBlock(Backend *be, Bfunction *f,
                           Bblock *bl, Bexpression *e);

// What to do with debug meta-data when verifying the module
enum DebugDisposition {
  StripDebugInfo,
  PreserveDebugInfo
};

class FcnTestHarness {
 public:
  // Create test harness. If name specified, then create a
  // default function "fcnName(i1, i2 int32) int64".
  FcnTestHarness(llvm::CallingConv::ID cconv, const char *fcnName = nullptr);
  ~FcnTestHarness();

  // Create function to work on
  Bfunction *mkFunction(const char *fcnName, BFunctionType *befty);

  // Return pointer to backend
  Llvm_backend *be() { return be_.get(); }

  // Harness creates a single dummy location to assign to all
  // expressions it creates -- this helper returns that location.
  Location loc() const { return loc_; }

  // Update the dummy location to something new, then return it.
  Location newloc();

  // New location with specified file and line.
  Location newFileLineLoc(const char *file, unsigned line);

  // Return current function
  Bfunction *func() const { return func_; }

  // Return current block
  Bblock *block() const { return curBlock_; }

  // Create a local variable in the function.
  Bvariable *mkLocal(const char *name, Btype *typ, Bexpression *init = nullptr);

  // Whether to append created stmt to current block
  enum AppendDisp { YesAppend, NoAppend };

  // Produce a call expression targeting the specified function. Variable
  // args are parameter values, terminated by nullptr.
  Bexpression *mkCallExpr(Backend *be, Bfunction *fun, ...);

  // Create an assignment LHS = RHS and append to block
  void mkAssign(Bexpression *lhs, Bexpression *rhs, AppendDisp d = YesAppend);

  // Create and append an expression stmt
  Bstatement *mkExprStmt(Bexpression *expr, AppendDisp disp = YesAppend);

  // Append a return stmt to block
  Bstatement *mkReturn(Bexpression *expr, AppendDisp disp = YesAppend);

  // Append a multi-value return stmt to block
  Bstatement *mkReturn(const std::vector<Bexpression *> &vals,
                       AppendDisp disp = YesAppend);

  // Create and append an "if" statement.
  Bstatement *mkIf(Bexpression *cond, Bstatement *trueStmt,
                   Bstatement *falseStmt, AppendDisp disp = YesAppend);

  // Create and append a switch statement
  Bstatement *mkSwitch(Bexpression *swval,
                       const std::vector<std::vector<Bexpression*>> &cases,
                       const std::vector<Bstatement*> &statements,
                       AppendDisp disp = YesAppend);

  // Add a previously created statement to the current block
  void addStmt(Bstatement *stmt);

  // Create a new block (prev block will jump to new one)
  void newBlock(std::vector<Bvariable *> *varlist = nullptr);

  // Verify that block contains specified contents. Return false
  // and emit diagnostics if not.
  bool expectBlock(const ExpectedDump &expected);

  // Verify that stmt contains specified contents. Return false
  // and emit diagnostics if not.
  bool expectStmt(Bstatement *st, const ExpectedDump &expected);

  // Verify that value contains specified contents. Return false
  // and emit diagnostics if not.
  bool expectValue(llvm::Value *val, const ExpectedDump &expected);

  // Verify that repr() output contains specified contents. Return false
  // and emit diagnostics if not.
  bool expectRepr(Bnode *node, const ExpectedDump &expected);

  // Verify that a dump of the module contains the specified token sequence
  // somewhere within it. To be used only if the various methods above
  // are inadequate, since obviously there is more potential for
  // trouble here.
  bool expectModuleDumpContains(const std::string &expected);

  // Return a count of the number of times we see the specified
  // token sequence within the module dump.
  unsigned countInstancesInModuleDump(const std::string &token);

  // Skip the checking at finish time for orphan CFG blocks.
  void allowOrphans() { findOrphanBBs_ = false; }

  //
  // Finish function:
  // - attach current block to function
  // - verify module, returning TRUE if module fails to verify
  //
  // In some cases having debug meta-data around can create useless
  // noise (for example, if the goal of a test is to verify the control
  // flow generated for an "if" statement, we don't really care about
  // checking that things have proper meta-data). In such cases clients
  // can pass in StripDebugInfo when invoking this routine.
  //
  bool finish(DebugDisposition disp);

 private:
  llvm::LLVMContext context_;
  std::unique_ptr<Llvm_backend> be_;
  Bfunction *func_;
  std::vector<Bvariable *> emptyVarList_;
  Location loc_;
  Bblock *entryBlock_;
  Bblock *curBlock_;
  Blabel *nextLabel_;
  unsigned lineNum_;
  bool finished_;
  bool returnAdded_;
  bool emitDumpFilesOnDiff_;
  bool emitRemasterScript_;
  bool findOrphanBBs_;
};

} // end namespace goBackendUnitTests

#endif // !defined(#define GOLLVM_UNITTESTS_BACKENDCORE_TESTUTILS_H)
