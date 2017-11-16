//===-- go-llvm-tree-integrity.cpp - tree integrity utils impl ------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// Methods for class IntegrityVisitor.
//
//===----------------------------------------------------------------------===//

#include "go-llvm-bexpression.h"
#include "go-llvm-bstatement.h"
#include "go-llvm-tree-integrity.h"
#include "go-llvm.h"

#include "llvm/IR/Instruction.h"

IntegrityVisitor::IntegrityVisitor(Llvm_backend *be,
                                   TreeIntegCtl control)
    : be_(be), ss_(str_), control_(control),
      instShareCount_(0), stmtShareCount_(0), exprShareCount_(0)
{
  acceptableNodes_.insert(N_Const);
  acceptableNodes_.insert(N_Call);
  acceptableNodes_.insert(N_FcnAddress);
  acceptableNodes_.insert(N_Conditional);
  acceptableNodes_.insert(N_Var);
  acceptableNodes_.insert(N_Conversion);
  acceptableNodes_.insert(N_Deref);
  acceptableNodes_.insert(N_StructField);
  acceptableNodes_.insert(N_BinaryOp); // subject to acceptableOpcodes
  acceptableOpcodes_.insert(OPERATOR_PLUS);
  acceptableOpcodes_.insert(OPERATOR_MINUS);
  acceptableOpcodes_.insert(OPERATOR_EQEQ);
  acceptableOpcodes_.insert(OPERATOR_NOTEQ);
}

void IntegrityVisitor::dumpTag(const char *tag, void *ptr) {
  ss_ << tag << ": ";
  if (includePointers() == DumpPointers)
    ss_ << ptr;
  ss_ << "\n";
}

void IntegrityVisitor::dump(Bnode *node) {
  dumpTag(node->isStmt() ? "stmt" : "expr", (void*) node);
  node->osdump(ss_, 0, be_->linemap(), false);
}

void IntegrityVisitor::dump(llvm::Instruction *inst) {
  dumpTag("inst", (void*) inst);
  inst->print(ss_);
  ss_ << "\n";
}

bool IntegrityVisitor::shouldBeTracked(Bnode *child)
{
  Bexpression *expr = child->castToBexpression();
  if (expr && expr->value() &&
      be_->moduleScopeValue(expr->value(), expr->btype()))
    return false;
  return true;
}

void IntegrityVisitor::deletePending(Bnode *node)
{
  assert(shouldBeTracked(node));
  auto it = nparent_.find(node);
  if (it == nparent_.end())
    return;
  nparent_.erase(it);
}

// This helper routine is invoked in situations where the node
// builder wants to "reparent" the children of a node, e.g. it has
// a "foo" node with children X and Y, and it wants to convert it
// into a "bar" node with the same children. Ex:
//
//     foo               bar
//    .  .       =>     .   .
//   .    .            .     .
//  X      Y          X       Y
//
// In this situation we need to update "nparent_" to reflect the fact
// that X and Y are no longer parented by foo. A complication can crop
// up if sharing has already been established at the point where this
// routine is called. For example, suppose that there is some other
// node "baz" that has also installed "X" as a child:
//
//        baz    foo
//       .  .   .   .
//      .    . .     .
//     W      X       Y
//
// If this situation arises, we need to preserve the existing nparent_
// mapping, so as to be able to repair the sharing later on.

void IntegrityVisitor::unsetParent(Bnode *child, Bnode *parent, unsigned slot)
{
  if (! shouldBeTracked(child))
    return;
  auto it = nparent_.find(child);
  if (it == nparent_.end())
    return;
  parslot pps = it->second;
  Bnode *prevParent = pps.first;
  unsigned prevSlot = pps.second;
  if (prevParent == parent || prevSlot == slot)
    nparent_.erase(it);
}

void IntegrityVisitor::unsetParent(llvm::Instruction *inst,
                                   Bexpression *exprParent,
                                   unsigned slot)
{
  auto it = iparent_.find(inst);
  if (it == iparent_.end())
    return;
  parslot pps = it->second;
  Bnode *prevParent = pps.first;
  unsigned prevSlot = pps.second;
  if (prevParent == exprParent || prevSlot == slot)
    iparent_.erase(it);
}

void IntegrityVisitor::setParent(Bnode *child, Bnode *parent, unsigned slot)
{
  if (! shouldBeTracked(child))
    return;
  auto it = nparent_.find(child);
  if (it != nparent_.end()) {
    parslot pps = it->second;
    Bnode *prevParent = pps.first;
    unsigned prevSlot = pps.second;
    if (prevParent == parent && prevSlot == slot)
      return;
    if (child->flavor() == N_Error)
      return;

    // Was this instance of sharing recorded previously?
    parslot ps = std::make_pair(parent, slot);
    auto it = sharing_.find(ps);
    if (it != sharing_.end())
      return;

    // Keep track of this location for future use in repair operations.
    sharing_.insert(ps);

    // If the sharing at this subtree is repairable, don't
    // log an error, since the sharing will be undone later on.
    Bexpression *expr = child->castToBexpression();
    if (expr != nullptr &&
        doRepairs() == DontReportRepairableSharing &&
        repairableSubTree(expr))
      return;

    const char *wh = nullptr;
    if (child->isStmt()) {
      stmtShareCount_ += 1;
      wh = "stmt";
    } else {
      exprShareCount_ += 1;
      wh = "expr";
    }

    // capture a description of the error
    ss_ << "error: " << wh << " has multiple parents"
        << " [ID=" << child->id() << "]\n";
    ss_ << "child " << wh << ":\n";
    dump(child);
    ss_ << "parent 1:\n";
    dump(prevParent);
    ss_ << "parent 2:\n";
    dump(parent);
    return;
  }
  nparent_[child] = std::make_pair(parent, slot);
}

void IntegrityVisitor::setParent(llvm::Instruction *inst,
                                 Bexpression *exprParent,
                                 unsigned slot)
{
  auto it = iparent_.find(inst);
  if (it != iparent_.end()) {
    parslot ps = it->second;
    Bnode *prevParent = ps.first;
    unsigned prevSlot = ps.second;
    if (prevParent == exprParent && prevSlot == slot)
      return;
    instShareCount_ += 1;
    ss_ << "error: instruction has multiple parents\n";
    dump(inst);
    ss_ << "parent 1:\n";
    dump(prevParent);
    ss_ << "parent 2:\n";
    dump(exprParent);
  } else {
    iparent_[inst] = std::make_pair(exprParent, slot);
  }
}

bool IntegrityVisitor::repairableSubTree(Bexpression *root)
{
  std::set<Bexpression *> visited;
  visited.insert(root);
  std::vector<Bexpression *> workList;
  workList.push_back(root);

  while (! workList.empty()) {
    Bexpression *e = workList.back();
    workList.pop_back();
    if (acceptableNodes_.find(e->flavor()) == acceptableNodes_.end())
      return false;
    if (e->flavor() == N_BinaryOp &&
        acceptableOpcodes_.find(e->op()) == acceptableOpcodes_.end())
      return false;
    for (auto &c : e->children()) {
      Bexpression *ce = c->castToBexpression();
      assert(ce);
      if (visited.find(ce) == visited.end()) {
        visited.insert(ce);
        workList.push_back(ce);
      }
    }
  }
  return true;
}

class ScopedIntegrityCheckDisabler {
 public:
  ScopedIntegrityCheckDisabler(Llvm_backend *be)
      : be_(be), val_(be->nodeBuilder().integrityChecksEnabled()) {
    be_->nodeBuilder().setIntegrityChecks(false);
  }
  ~ScopedIntegrityCheckDisabler() {
    be_->nodeBuilder().setIntegrityChecks(val_);
  }
 private:
  Llvm_backend *be_;
  bool val_;
};

bool IntegrityVisitor::repair(Bnode *node)
{
  ScopedIntegrityCheckDisabler disabler(be_);
  std::set<Bexpression *> visited;
  for (auto &ps : sharing_) {
    Bnode *parent = ps.first;
    unsigned slot = ps.second;
    const std::vector<Bnode *> &pkids = parent->children();
    Bexpression *child = pkids[slot]->castToBexpression();
    assert(child);

    if (visited.find(child) == visited.end()) {
      // Repairable?
      if (!repairableSubTree(child))
        return false;
      visited.insert(child);
    }
    Bexpression *childClone = be_->nodeBuilder().cloneSubtree(child);
    parent->replaceChild(slot, childClone);
  }
  sharing_.clear();
  exprShareCount_ = 0;
  return true;
}

void IntegrityVisitor::visit(Bnode *node)
{
  unsigned idx = 0;
  for (auto &child : node->children()) {
    if (visitMode() == BatchMode)
      visit(child);
    setParent(child, node, idx++);
  }
  Bexpression *expr = node->castToBexpression();
  if (expr) {
    idx = 0;
    for (auto inst : expr->instructions())
      setParent(inst, expr, idx++);
  }
}

bool IntegrityVisitor::examine(Bnode *node)
{
  // Visit node (and possibly entire subtree at node, mode depending)
  visit(node);

  // Inst sharing and statement sharing are not repairable.
  if (instShareCount_ != 0 || stmtShareCount_ != 0)
    return false;

  if (exprShareCount_ != 0)
    return false;

  // If the checker is in incremental mode, don't attempt repairs
  // (those will be done later on).
  if (visitMode() == IncrementalMode) {
    sharing_.clear();
    return true;
  }

  // Batch mode: return now if no sharing.
  if (sharing_.empty())
    return true;

  // Batch mode: perform repairs.
  if (repair(node))
    return true;

  // Repair failed -- return failure
  return false;
}
