//===-- go-llvm-bnode.h - decls for 'Bnode' class -------------------------===//
//
// Copyright 2018 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
//
//===----------------------------------------------------------------------===//
//
// Defines Bnode class.
//
//===----------------------------------------------------------------------===//

#ifndef LLVMGOFRONTEND_BNODE_H
#define LLVMGOFRONTEND_BNODE_H

// Currently these need to be included before backend.h
#include "go-llvm-linemap.h"
#include "go-location.h"
#include "go-llvm-btype.h"

#include "backend.h"

namespace llvm {
class AllocaInst;
class Instruction;
class Value;
class raw_ostream;
}

// Opaque labelID handle for goto/label nodes.
typedef unsigned LabelId;

class Bexpression;
class Binstructions;
class SwitchDescriptor;
class IntegrityVisitor;
class Llvm_backend;

// Use when deleting a Bnode subtree. Controls whether to delete just
// the Bnode objects, just the LLVM instructions they contain, or both.
//
enum WhichDel {
  DelInstructions, // delete only instructions
  DelWrappers,     // delete only wrappers
  DelBoth          // delete wrappers and instructions
};

// Varieties of nodes. Order is important here, since enum
// values are used to index into tables/arrays.

enum NodeFlavor {
  N_Error=0,

  N_FirstExpr,
  N_Const=N_FirstExpr,
  N_Var,
  N_FcnAddress,
  N_LabelAddress,
  N_Conversion,
  N_Deref,
  N_Address,
  N_UnaryOp,
  N_StructField,
  N_BinaryOp,
  N_Compound,
  N_ArrayIndex,
  N_PointerOffset,
  N_Composite,
  N_Call,
  N_Conditional,
  N_LastExpr = N_Conditional,

  N_FirstStmt,
  N_EmptyStmt=N_FirstStmt,
  N_LabelStmt,
  N_GotoStmt,
  N_ExprStmt,
  N_ReturnStmt,
  N_DeferStmt,
  N_IfStmt,
  N_ExcepStmt,
  N_BlockStmt,
  N_SwitchStmt,
  N_LastStmt=N_SwitchStmt
};

// For use by callbacks invoked by UpdatingNodeWalker.  A return value
// of StopWalk will terminate the walk immediately after the callback
// in question; a ContineWalk return means that the visitor will continue
// on normally.

enum VisitDisp {
  ContinueWalk, StopWalk
};

// Used by the visitChildPre callback invoked by UpdatingNodeWalker,
// to allow specific subtrees to be skipped. A return value of
// VisitChild tells the walker to go ahead and visit all of the nodes
// in the child subtree; a value of SkipChild means that none of the
// nodes in the child subtree will be visited. Example (from go code:
// x.f[v]+10):
//
//                      +
//                     / \
//          array_index  10
//          /        \
//         field     var v
//         /
//       var x
//
// Suppose the visitChildPre(array_index, field) returns SkipChild --
// in this case no other callbacks will be invoked for the "field" or
// the "var x" nodes, but all other nodes will be walked (ex: "var v").

enum VisitChildDisp {
  VisitChild, SkipChild
};

enum VisitOrder {
  PreOrder, PostOrder
};

enum VisitWhich {
  VisitExprs, VisitStmts, VisitAll
};

// Class Bnode -- this class is not directly used or referred to in
// gofrontend, but it acts as an abstract base for the Bexpression and
// Bstatement classes. A given Bnode has zero or more Bnode children;
// the number and type (expr or stmt) children are determined by
// the Bnode flavor.

class Bnode {
 public:
  virtual ~Bnode() { }
  NodeFlavor flavor() const { return flavor_; }
  Location location() const { return location_; }
  unsigned id() const { return id_; }
  const char *flavstr() const;
  Blabel *label() const;

  // debugging
  void dump();

  // dump with source line info
  void srcDump(Llvm_linemap *);

  // dump to raw_ostream
  void osdump(llvm::raw_ostream &os, unsigned ilevel = 0,
              Llvm_linemap *linemap = nullptr, bool terse = false);

  // Cast to Bexpression. Returns NULL if not correct flavor.
  Bexpression *castToBexpression() const;

  // Cast to Bstatement. Returns NULL if not correct flavor.
  Bstatement *castToBstatement() const;

  // Cast to Bblock. Returns NULL if not correct flavor.
  Bblock *castToBblock() const;

  // For unary and binary op expression nodes, this returns the opcode
  Operator op() const;

  // For var exprs, this returns the underlying bvariable
  Bvariable *var() const;

  // Return struct field index for a field expr
  unsigned fieldIndex() const;

  // Return function associated with this node. Value only for
  // function constants, calls, and conditionals. Note that for calls and
  // conditionals this will be the function containing the construct.
  Bfunction *getFunction() const;

  // Return target function for call (will be nil if indirect call).
  Bfunction *getCallTarget() const;

  // Return index to BnodeBuilder's vector of indices for composite,
  // or -1 if no indexing.
  int getIndices() const;

  template<class Visitor> friend class SimpleNodeWalker;
  template<class Visitor> friend class UpdatingNodeWalker;
  friend class BnodeBuilder;
  friend class IntegrityVisitor;

  // TODO: hide this once GenBlocksVisitor is working
  const std::vector<Bnode *> &children() const { return kids_; }

  bool isStmt() const {
    return (flavor() >= N_FirstStmt && flavor() <= N_LastStmt);
  }

 protected:
  Bnode(NodeFlavor flavor, const std::vector<Bnode *> &kids, Location loc);
  Bnode(const Bnode &src);
  SwitchDescriptor *getSwitchCases();

 private:
  void replaceChild(unsigned idx, Bnode *newchild);

 private:
  std::vector<Bnode *> kids_;
  union {
    Bvariable *var;  // filled in only for Var and ExcepStmt nodes
    Bfunction *func; // filled in only for fcn constants, calls, conditionals
    SwitchDescriptor *swcases;
    int indices; // for composite expressions, index to BnodeBuilder's indexvecs_
    Blabel *label;
    Operator op;
    unsigned fieldIndex;
  } u;
  Location location_;
  NodeFlavor flavor_;
  unsigned id_;
  unsigned flags_;
};

// This helper class handles construction for all Bnode objects.
// Notes on storage allocation: ideally once an LLVM function has been
// constructed and sent off to the back end for a given Go function,
// we would want to delete all of the Bexpression's used by that
// function before moving on to the next function. Putting this into
// practice is tricky, however, since some Bexpressions (for example,
// var exprs and function addresses) wind up being held over and
// reused else where (for example, in emitted GC descriptors). For the
// moment we don't try to free Bexpressions at intermediate points
// in the compilation, but we do free Bstatements.

class BnodeBuilder {
 public:
  BnodeBuilder(Llvm_backend *be);
  ~BnodeBuilder();

  // Deletes all allocated Bstatements (also switch descriptors)
  void freeStmts();

  // expressions
  Bexpression *mkError(Btype *errortype);
  Bexpression *mkConst(Btype *btype, llvm::Value *val);
  Bexpression *mkVoidValue(Btype *voidType);
  Bexpression *mkVar(Bvariable *var, llvm::Value *val, Location loc);
  Bexpression *mkConversion(Btype *btype, llvm::Value *val,
                            Bexpression *src, Location loc);
  Bexpression *mkDeref(Btype *typ, llvm::Value *val,
                       Bexpression *src, Location loc);
  Bexpression *mkAddress(Btype *typ, llvm::Value *val,
                         Bexpression *src, Location loc);
  Bexpression *mkFcnAddress(Btype *typ, llvm::Value *val,
                            Bfunction *func, Location loc);
  Bexpression *mkLabelAddress(Btype *typ, llvm::Value *val,
                              Blabel *label, Location loc);
  Bexpression *mkUnaryOp(Operator op, Btype *typ, llvm::Value *val,
                         Bexpression *src, Location loc);
  Bexpression *mkBinaryOp(Operator op, Btype *typ, llvm::Value *val,
                          Bexpression *left, Bexpression *right, Location loc);
  Bexpression *mkBinaryOp(Operator op, Btype *typ, llvm::Value *val,
                          Bexpression *left, Bexpression *right,
                          Binstructions &instructions, Location loc);
  Bexpression *mkCompound(Bstatement *st, Bexpression *expr, llvm::Value *val,
                          Location loc);
  Bexpression *mkStructField(Btype *type, llvm::Value *value,
                             Bexpression *structval, unsigned fieldIndex,
                             Location loc);
  Bexpression *mkArrayIndex(Btype *typ,
                            llvm::Value *val,
                            Bexpression *arval,
                            Bexpression *index,
                            Location loc);
  Bexpression *mkPointerOffset(Btype *typ,
                               llvm::Value *val,
                               Bexpression *ptr,
                               Bexpression *offset,
                               Location loc);
  Bexpression *mkIndexedComposite(Btype *btype, llvm::Value *value,
                                  const std::vector<Bexpression *> &vals,
                                  const std::vector<unsigned long> &indices,
                                  Binstructions &instructions,
                                  Location loc);
  Bexpression *mkComposite(Btype *btype, llvm::Value *value,
                           const std::vector<Bexpression *> &vals,
                           Binstructions &instructions,
                           Location loc);
  Bexpression *mkCall(Btype *btype,
                      llvm::Value *value,
                      Bfunction *caller,
                      Bexpression *fnExpr,
                      Bexpression *chainExpr,
                      const std::vector<Bexpression *> &vals,
                      Binstructions &instructions,
                      Location loc);
  Bexpression *mkConditional(Bfunction *function,
                             Btype *btype,
                             Bexpression *condition,
                             Bexpression *then_expr,
                             Bexpression *else_expr,
                             Location loc);

  // statements
  Bstatement *mkErrorStmt();
  Bstatement *mkLabelDefStmt(Bfunction *func, Blabel *label, Location loc);
  Bstatement *mkGotoStmt(Bfunction *func, Blabel *label, Location loc);
  Bstatement *mkExprStmt(Bfunction *func, Bexpression *expr, Location loc);
  Bstatement *mkReturn(Bfunction *func, Bexpression *returnVal, Location loc);
  Bstatement *mkIfStmt(Bfunction *func,
                       Bexpression *cond, Bblock *trueBlock,
                       Bblock *thenBlock, Location loc);
  Bstatement *mkDeferStmt(Bfunction *func,
                          Bexpression *undefer,
                          Bexpression *defer,
                          Location loc);
  Bstatement *mkExcepStmt(Bfunction *func,
                          Bstatement *body,
                          Bstatement *onexception,
                          Bstatement *finally,
                          Bvariable *finTempVar,
                          Location loc);
  Bstatement *mkSwitchStmt(Bfunction *func,
                           Bexpression *swvalue,
                           const std::vector<std::vector<Bexpression *> > &vals,
                           const std::vector<Bstatement *> &stmts,
                           Location loc);

  // block
  Bblock *mkBlock(Bfunction *func,
                  const std::vector<Bvariable *> &vars,
                  Location loc);
  void addStatementToBlock(Bblock *block, Bstatement *st);

  // Create a temporary variable of the specified type, to be
  // incorporated into some expression we're building. Although this
  // strictly speaking doesn't relate to Bnode building, it helps to
  // have this interface here, since IR construction methods will
  // always have access to a BnodeBuilder but may not have the current
  // function we're processing.
  Bvariable *mkTempVar(Btype *varType, Location loc, const std::string &name);

  // This helper looks up the specified variable (as identified by its
  // alloca) to see if it is an unparented temp created during IR
  // construction (via the mkTempVar method above). If so, the
  // variable is extracted from the builder's set and returned. If the
  // var is not an unadopted temp, then NULL is returned.
  Bvariable *adoptTemporaryVariable(llvm::AllocaInst *alloca);

  // Free up this expr or stmt (it is garbage). Does not free up children.
  void freeNode(Bnode *node);

  // Delete some or all or this Bnode and its children/contents.
  // Here 'which' can be set to DelInstructions (deletes only the
  // LLVM instructions found in the subtree), DelWrappers (deletes
  // Bnodes but not instructions), or DelBoth (gets rid of nodes and
  // instructions).
  // If recursive is true, also delete its children recursively.
  void destroy(Bnode *node, WhichDel which = DelWrappers,
               bool recursive = true);

  // Clone an expression subtree.
  Bexpression *cloneSubtree(Bexpression *expr);

  // Get the indices of a composite expression.
  const std::vector<unsigned long> *getIndices(Bexpression *expr) const;

  // Inform the builder that we're about to extract all of the
  // children of the specified node and incorporate them into a new
  // node (after which the old node will be thrown away). Returns vector
  // containing expression children.
  std::vector<Bexpression *> extractChildenAndDestroy(Bexpression *expr);

  // Similar to the routine above, but works at the Bnode level (so as
  // to support dealing with Bnodes that have a mix of statement/expr
  // children).
  std::vector<Bnode *> extractChildNodesAndDestroy(Bnode *node);

  // Update the instructions of an expression node after we've finished
  // walking its instruction list. During the walk it's possible that
  // A) one or more existing instructions were converted into new
  // instructions, or B) some instructions were thrown away because
  // they appeared after a no-return call in the list. This helper
  // fixes up the instruction list and updates ownership info
  // in the integrity checker.
  void updateInstructions(Bexpression *expr,
                          std::vector<llvm::Instruction*> newinsts);

  // Get/set whether the tree integrity checker is enabled. It makes sense
  // to turn off the integrity checker during tree cloning operations
  // (part of sharing repair), and also for unit testing.
  bool integrityChecksEnabled() const { return checkIntegrity_; }
  void setIntegrityChecks(bool val) { checkIntegrity_ = val; }

 private:
  void appendInstIfNeeded(Bexpression *rval, llvm::Value *val);
  Bexpression *archive(Bexpression *expr);
  Bstatement *archive(Bstatement *stmt);
  Bblock *archive(Bblock *bb);
  Bexpression *cloneSub(Bexpression *expr,
                        std::map<llvm::Value *, llvm::Value *> &vm);
  void checkTreeInteg(Bnode *node);
  void destroyRec(Bnode *node, WhichDel which, bool recursive,
                  std::set<Bnode *> &visited);
  void recordDeadInstruction(llvm::Instruction *inst);

 private:
  std::unique_ptr<Bstatement> errorStatement_;
  std::vector<Bexpression *> earchive_;
  std::vector<Bstatement *> sarchive_;
  std::vector<SwitchDescriptor*> swcases_;
  std::vector< std::vector<unsigned long> > indexvecs_;
  std::unordered_map<llvm::AllocaInst*, Bvariable*> tempvars_;
  std::unique_ptr<IntegrityVisitor> integrityVisitor_;
  bool checkIntegrity_;
  std::vector<llvm::Instruction*> deadInstructions_;
};

// This class helps automate walking of a Bnode subtree; it invokes
// callbacks in the supplied visitor object at useful points during
// the walk that is instigated by 'simple_walk_nodes' below.

template<class Visitor>
class SimpleNodeWalker {
public:
  SimpleNodeWalker(Visitor &vis) : visitor_(vis) { }

  void walk(Bnode *node) {
    assert(node);

    // pre-node hook
    visitor_.visitNodePre(node);

    // walk children
    for (unsigned idx = 0; idx < node->children().size(); ++idx) {
      Bnode *child = node->children()[idx];
      walk(child);
    }

    // post-node hook
    visitor_.visitNodePost(node);
  }

 private:
  Visitor &visitor_;
};

template<class Visitor>
inline void simple_walk_nodes(Bnode *root, Visitor &vis) {
  SimpleNodeWalker<Visitor> walker(vis);
  walker.walk(root);
}

// A more complicated node walker that allows for replacement of child
// nodes, plus stopping the walk if the visitor so decides.

template<class Visitor>
class UpdatingNodeWalker {
public:
  UpdatingNodeWalker(Visitor &vis) : visitor_(vis) { }

  std::pair<VisitDisp, Bnode *> walk(Bnode *node) {
    assert(node);

    // pre-node hook
    auto pairPre = visitor_.visitNodePre(node);
    if (pairPre.second != node)
      node = pairPre.second;
    if (pairPre.first == StopWalk)
      return std::make_pair(StopWalk, node);

    std::vector<Bnode *> children = node->children();
    for (unsigned idx = 0; idx < children.size(); ++idx) {
      Bnode *child = children[idx];

      // pre-child hook. Return value here is
      // std::pair< std::pair<VisitDisp, VisitChildDisp>, Bnode *>

      auto res = visitor_.visitChildPre(node, child);
      Bnode *newChild = res.second;
      std::pair<VisitDisp, VisitChildDisp> disp = res.first;
      enum VisitDisp walkDisp = disp.first;
      enum VisitChildDisp childDisp = disp.second;
      if (newChild != child) {
        node->replaceChild(idx, newChild);
        child = newChild;
      }
      if (walkDisp == StopWalk)
        return std::make_pair(StopWalk, node);

      // walk child (unless we've been told to skip it)
      if (childDisp != SkipChild) {
        auto pairChild = walk(child);
        if (pairChild.second != child) {
          node->replaceChild(idx, pairChild.second);
          child = pairChild.second;
        }
        if (pairChild.first == StopWalk)
          return std::make_pair(StopWalk, node);
      }

      // post-child hook
      auto pairPost = visitor_.visitChildPost(node, child);
      if (pairPost.second != child)
        node->replaceChild(idx, pairPost.second);
      if (pairPost.first == StopWalk)
        return std::make_pair(StopWalk, node);
    }

    // post-node hook
    auto pairPost = visitor_.visitNodePost(node);
    if (pairPost.second != node)
      node = pairPost.second;
    if (pairPost.first == StopWalk)
      return std::make_pair(StopWalk, node);

    return std::make_pair(ContinueWalk, node);
  }

 private:
  Visitor &visitor_;
};

template<class Visitor>
inline Bnode *update_walk_nodes(Bnode *root, Visitor &vis) {
  UpdatingNodeWalker<Visitor> walker(vis);
  auto p = walker.walk(root);
  return p.second;
}

// Helper contained class for recording the child indices of switch
// cases and switch statements within the containing Bnode.  Here 'st'
// is the child index of the first case value, 'len' is the number of
// matching values for the case, and 'stmt' is the child index of the
// corresponding statement. Example:
//
// switch q {             Switch Bnode children:
//   case 2, 3, 4:          0: expr q
//     return x + y         1: expr 2
//   default:               2: expr 3
//     return x / y         3: expr 4
//   case 5:                4: expr 5
//     return 9             5: stmt return x + y
//  }                       6: stmt return x / y
//                          7: stmt return 9
//  Case descriptors:
//     1, 3, 5
//     4, 0, 6
//     5, 1, 7

struct SwitchCaseDesc {
  unsigned st, len, stmt;
  SwitchCaseDesc(unsigned s, unsigned l, unsigned st)
      : st(s), len(l), stmt(st) { }
};

// Stores child index info about a switch.

class SwitchDescriptor {
 public:
  SwitchDescriptor(const std::vector<std::vector<Bexpression *> > &vals);
  const std::vector<SwitchCaseDesc> &cases() const { return cases_; }

 private:
  std::vector<SwitchCaseDesc> cases_;
};

#endif // LLVMGOFRONTEND_GO_BNODE_H
