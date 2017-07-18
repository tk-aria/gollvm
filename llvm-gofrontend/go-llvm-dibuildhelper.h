//===-- go-llvm-dibuild.h - DIBuildHelper class interfaces  ---------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// Defines DIBuildHelper class.
//
//===----------------------------------------------------------------------===//

#ifndef GO_LLVM_DIBUILDHELPER_H
#define GO_LLVM_DIBUILDHELPER_H

#include <vector>
#include <unordered_map>

#include "go-location.h"

namespace llvm {
class BasicBlock;
class DIBuilder;
class DIFile;
class DILocalVariable;
class DILocation;
class DIScope;
class DIType;
class DebugLoc;
class Instruction;
class Module;
class Type;
}

class Bnode;
class Bblock;
class Bexpression;
class Bfunction;
class Btype;
class Bvariable;
class Llvm_linemap;
class TypeManager;

// This class helps with managing the process of generating debug meta-data.
// It carries around pointers to objects that are needed (ex: linemap,
// typemanager, DIBuilder, etc) and keeps track of stack of DIScopes.

class DIBuildHelper {
 public:
  DIBuildHelper(llvm::Module *module,
                TypeManager *typemanager,
                Llvm_linemap *linemap);

  void processGlobal(Bvariable *gvar, bool isExported);

  void beginFunction(Bfunction *function, Bnode *topnode, llvm::BasicBlock *entryBlock);
  void endFunction(Bfunction *function);

  void beginLexicalBlock(Bblock *block);
  void endLexicalBlock(Bblock *block);

  void finalize();

  llvm::DIFile *diFileFromLocation(Location location);

  void processExprInst(Bexpression *expr, llvm::Instruction *inst);

  // Return module scope
  llvm::DIScope *moduleScope() const { return moduleScope_; }

  // Return top of DI scope stack
  llvm::DIScope *currentDIScope();

  // Push / pop scope
  llvm::DIScope *popDIScope();
  void pushDIScope(llvm::DIScope *);

  // Various getters
  llvm::DIBuilder &dibuilder() { return *dibuilder_.get(); }
  Llvm_linemap *linemap() { return linemap_; }
  TypeManager *typemanager() { return typemanager_; }

  // Type cache, to deal with cycles.
  std::unordered_map<Btype *, llvm::DIType*> &typeCache() {
    return typeCache_;
  }

 private:
  llvm::Module *module_;
  TypeManager *typemanager_;
  Llvm_linemap *linemap_;
  llvm::DIScope *moduleScope_;
  std::unique_ptr<llvm::DIBuilder> dibuilder_;
  std::vector<llvm::DIScope*> diScopeStack_;
  std::unordered_map<Btype *, llvm::DIType*> typeCache_;
  std::vector<std::pair<Bvariable *, bool> > globalsToProcess_;

  // The following items are specific to the current function we're visiting.
  std::unordered_set<Bvariable *> declared_;
  Bblock *topblock_;
  llvm::BasicBlock *entryBlock_;
  unsigned known_locations_;

 private:
  void createCompileUnitIfNeeded();
  llvm::DebugLoc debugLocFromLocation(Location location);
  void insertVarDecl(Bvariable *var, llvm::DILocalVariable *dilv);
  bool interestingBlock(Bblock *block);
  void processVarsInBLock(const std::vector<Bvariable*> &vars,
                          llvm::DIScope *scope);
  void markBlocks(Bnode *node);
};

#endif // !defined(GO_LLVM_DIBUILDHELPER_H)
