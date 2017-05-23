//===-- go-llvm-tree-integrity.h - decls for tree integrity utils ---------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// Defines IntegrityVisitor class.
//
//===----------------------------------------------------------------------===//

#ifndef LLVMGOFRONTEND_GO_LLVM_TREE_INTEGRITY_H
#define LLVMGOFRONTEND_GO_LLVM_TREE_INTEGRITY_H

#include "llvm/Support/raw_ostream.h"

namespace llvm {
class Instruction;
}

class Bstatement;
class Bexpression;
class Llvm_backend;
class BnodeBuilder;

enum CkTreePtrDisp { DumpPointers, NoDumpPointers };
enum CkTreeVarDisp { CheckVarExprs, IgnoreVarExprs };
enum CkTreeRepairDisp { DontReportRepairableSharing, ReportRepairableSharing };
enum CkTreeVisitDisp { BatchMode, IncrementalMode };

// Options/controls for the tree integrity checker.

struct TreeIntegCtl {
  CkTreePtrDisp ptrDisp;
  CkTreeVarDisp varDisp;
  CkTreeRepairDisp repairDisp;
  CkTreeVisitDisp visitDisp;
  TreeIntegCtl()
      : ptrDisp(DumpPointers),
        varDisp(CheckVarExprs),
        repairDisp(ReportRepairableSharing),
        visitDisp(BatchMode) { }
  TreeIntegCtl(CkTreePtrDisp ptrs, CkTreeVarDisp vars,
               CkTreeRepairDisp repair, CkTreeVisitDisp visit)
      : ptrDisp(ptrs), varDisp(vars),
        repairDisp(repair), visitDisp(visit) { }
};

// This visitor class detects malformed IR trees, specifically cases
// where the same Bexpression or Bstatement is pointed to by more than
// containing expr/stmt. For example suppose we have a couple of assignment
// statements
//
//      x = y
//      z = y
//
// where the Bexpression corresponding to "y" is pointed to both by
// the first assignment stmt and by the second assignment stmt.
//
// It is worth noting that some sharing is allowed in Bexpression trees,
// specifically sharing of Bexpressions corresponding to module-scope
// constants.
//
// In addition, we provide a mode of the checker ("IgnoreVarExprs"
// above) in which we don't try to enforce sharing for var exprs,
// since gofrontend tends to reuse them in a number of places.  The
// one place where this can cause issues is with nested functions and
// closures. Example:
//
//     func simple(x, q int) int {
//      pf := func(xx int) int {
//              qm2 := q - 2
//              qm1 := q - 1
//              return xx + qm2 + qm1
//      }
//      return pf(x)
//     }
//
// Note the references to "q" within the nested function -- in a
// non-nested function these woule be simple var expressions, however
// in the case above they will appear to the backend as loads of
// fields from the closure pointer passed via the static chain. To
// support this case there is yet another option/control that tells
// the checker to "unshare" the nodes in question (clone them to
// restore tree integrity). This unsharing/repairing is applied only
// to a whitelisted set of expression nodes (for example, cloning of
// call expressions is not allowed).

class IntegrityVisitor {
 public:
  IntegrityVisitor(Llvm_backend *be,
                   TreeIntegCtl control)
      : be_(be), ss_(str_), control_(control),
        instShareCount_(0), stmtShareCount_(0), exprShareCount_(0) { }

  // Visits the node tree "n", looking for any shared nodes or
  // instructions. Returns TRUE if there is no sharing (or if all
  // sharing instances are repairable and repair is on), or FALSE if
  // there is unrepairable sharing. Note that if the visit mode (in
  // 'control' options above) is set to BatchMode, then the visitor
  // will walk the entire subtree rooted at "n" and perform repairs
  // after the way. Otherwise (checkers is in incremental mode), only
  // the chidren of "n" are visited, and not repairs are performed.
  bool examine(Bnode *n);

  // Returns a message describing the nature of the node sharing (intended
  // for a developer, not a compiler user).
  std::string msg() { auto rv = ss_.str(); str_ = ""; return rv; }

  // Tell the IntegrityVisitor to forget about the specified parent/child
  // relationship (used when node child is deleted or repurposed).
  void unsetParent(Bnode *child, Bnode *parent, unsigned slot);

 private:
  Llvm_backend *be_;
  typedef std::pair<Bnode *, unsigned> parslot; // parent and child index
  std::unordered_map<llvm::Instruction *, parslot> iparent_;
  std::unordered_map<Bnode *, parslot> nparent_;
  std::vector<std::pair<Bnode *, parslot> > sharing_; // child -> parent+slot
  std::string str_;
  llvm::raw_string_ostream ss_;
  TreeIntegCtl control_;
  unsigned instShareCount_;
  unsigned stmtShareCount_;
  unsigned exprShareCount_;

 private:
  bool repair(Bnode *n);
  void visit(Bnode *n);
  bool repairableSubTree(Bexpression *root);
  bool shouldBeTracked(Bnode *child);
  void setParent(Bnode *child, Bnode *parent, unsigned slot);
  void setParent(llvm::Instruction *inst, Bexpression *par, unsigned slot);
  void dumpTag(const char *tag, void *ptr);
  void dump(llvm::Instruction *inst);
  void dump(Bnode *node);
  CkTreePtrDisp includePointers() const { return control_.ptrDisp; }
  CkTreeVarDisp includeVarExprs() const { return control_.varDisp; }
  CkTreeRepairDisp doRepairs() const { return control_.repairDisp; }
  CkTreeVisitDisp visitMode() const { return control_.visitDisp; }

  friend BnodeBuilder;
};

#endif // LLVMGOFRONTEND_GO_LLVM_TREE_INTEGRITY_H
