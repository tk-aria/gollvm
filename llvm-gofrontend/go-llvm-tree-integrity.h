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

#include "go-llvm-containertypes.h"

namespace llvm {
class Instruction;
}

class Bstatement;
class Bexpression;
class Llvm_backend;
class BnodeBuilder;

enum CkTreePtrDisp { DumpPointers, NoDumpPointers };
enum CkTreeRepairDisp { DontReportRepairableSharing, ReportRepairableSharing };
enum CkTreeVisitDisp { BatchMode, IncrementalMode };

// Options/controls for the tree integrity checker.

struct TreeIntegCtl {
  CkTreePtrDisp ptrDisp;
  CkTreeRepairDisp repairDisp;
  CkTreeVisitDisp visitDisp;
  TreeIntegCtl()
      : ptrDisp(DumpPointers),
        repairDisp(ReportRepairableSharing),
        visitDisp(BatchMode) { }
  TreeIntegCtl(CkTreePtrDisp ptrs, CkTreeRepairDisp repair,
               CkTreeVisitDisp visit)
      : ptrDisp(ptrs), repairDisp(repair), visitDisp(visit) { }
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
// In addition, we provide a mode of the checker in which we allow the
// front end to manufature IR that includes sharing of certain nodes
// but runs an "unsharing" or repair phase to replicate any node that
// is specified as a child in more than one tree location.  This
// unsharing/repairing is applied only to a whitelisted set of
// expression nodes (for example, cloning of call expressions is not
// allowed).

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
  pairhashset<Bnode *, unsigned> sharing_;
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
  CkTreeRepairDisp doRepairs() const { return control_.repairDisp; }
  CkTreeVisitDisp visitMode() const { return control_.visitDisp; }

  friend BnodeBuilder;
};

#endif // LLVMGOFRONTEND_GO_LLVM_TREE_INTEGRITY_H
