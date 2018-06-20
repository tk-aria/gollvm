//===-- go-llvm-dibuildhelper.cpp - implementation of DIBuildHelper -------===//
//
// Copyright 2018 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
//
//===----------------------------------------------------------------------===//
//
// Methods for DIBuildHelper class.
//
//===----------------------------------------------------------------------===//

#include "go-llvm-dibuildhelper.h"
#include "go-llvm-typemanager.h"
#include "go-llvm-bexpression.h"
#include "go-llvm-bstatement.h"
#include "go-llvm-bfunction.h"
#include "go-llvm-bvariable.h"

#include "llvm/IR/DIBuilder.h"
#include "llvm/IR/DebugInfo.h"
#include "llvm/IR/DebugInfoMetadata.h"
#include "llvm/IR/DebugLoc.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Instructions.h"
#include "llvm/Support/FileSystem.h"

DIBuildHelper::DIBuildHelper(llvm::Module *module,
                             TypeManager *typemanager,
                             Llvm_linemap *linemap)
    : module_(module), typemanager_(typemanager), linemap_(linemap), moduleScope_(nullptr),
      dibuilder_(new llvm::DIBuilder(*module)), topblock_(nullptr),
      entryBlock_(nullptr), known_locations_(0)
{
}

void DIBuildHelper::createCompileUnitIfNeeded()
{
  if (moduleScope_)
    return;

  // Create compile unit
  llvm::SmallString<256> currentDir;
  llvm::sys::fs::current_path(currentDir);
  std::string filestr = applyDebugPrefix(linemap_->get_initial_file());
  auto primaryFile = dibuilder_->createFile(filestr, currentDir);
  bool isOptimized = true;
  std::string compileFlags; // FIXME
  unsigned runtimeVersion = 0; // not sure what would be for Go
  moduleScope_ =
      dibuilder_->createCompileUnit(llvm::dwarf::DW_LANG_Go, primaryFile,
                                    "llvm-goc", isOptimized,
                                    compileFlags, runtimeVersion);
  pushDIScope(moduleScope_);
}

void DIBuildHelper::finalize()
{
  createCompileUnitIfNeeded();

  // Emit any globals we've collected along the way.
  for (auto it : globalsToProcess_) {
    Bvariable *v = it.first;
    bool isExported = it.second;
    llvm::DIFile *vfile = diFileFromLocation(v->location());
    unsigned vline = linemap()->location_line(v->location());
    llvm::DIType *vdit = typemanager()->buildDIType(v->btype(), *this);
    bool isLocalToUnit = !isExported;
    dibuilder().createGlobalVariableExpression(moduleScope_,
                                               v->name(), v->name(),
                                               vfile, vline, vdit,
                                               isLocalToUnit);
  }

  dibuilder_->finalize();
}



void DIBuildHelper::processGlobal(Bvariable *v, bool isExported)
{
  globalsToProcess_.push_back(std::make_pair(v, isExported));
}

void DIBuildHelper::beginFunction(Bfunction *function,
                                  Bnode *topnode,
                                  llvm::BasicBlock *entryBlock)
{
  createCompileUnitIfNeeded();

  assert(entryBlock_ == nullptr);
  assert(entryBlock != nullptr);
  entryBlock_ = entryBlock;

  assert(topblock_ == nullptr);
  assert(topnode->castToBblock());
  topblock_ = topnode->castToBblock();

  known_locations_ = 0;

  // Create proper DIType for function
  llvm::DIType *dit =
      typemanager()->buildDIType(function->fcnType(), *this);
  llvm::DISubroutineType *dst =
      llvm::cast<llvm::DISubroutineType>(dit);

  // Now the function entry itself
  unsigned fcnLine = linemap()->location_line(function->location());
  bool isLocalToUnit = !function->function()->hasExternalLinkage();
  bool isDefinition = true;
  unsigned scopeLine = fcnLine; // FIXME -- determine correct value here
  llvm::DIFile *difile = diFileFromLocation(function->location());
  auto difunc =
      dibuilder().createFunction(moduleScope(), function->name(),
                                 isLocalToUnit ? "" : function->asmName(),
                                 difile, fcnLine, dst, isLocalToUnit,
                                 isDefinition, scopeLine);
  pushDIScope(difunc);
}

void DIBuildHelper::processVarsInBLock(const std::vector<Bvariable*> &vars,
                                       llvm::DIScope *scope)
{
  for (auto &v : vars) {
    if (v->isTemporary())
      continue;
    if (declared_.find(v) != declared_.end())
      continue;
    declared_.insert(v);

    llvm::DIFile *vfile = diFileFromLocation(v->location());
    llvm::DIType *vdit =
        typemanager()->buildDIType(v->btype(), *this);
    unsigned vline = linemap()->location_line(v->location());
    llvm::DILocalVariable *dilv =
        dibuilder().createAutoVariable(scope, v->name(), vfile, vline, vdit);
    insertVarDecl(v, dilv);
  }
}

void DIBuildHelper::insertVarDecl(Bvariable *var,
                                  llvm::DILocalVariable *dilv)
{
  llvm::DIExpression *expr = dibuilder().createExpression();
  llvm::DILocation *vloc = debugLocFromLocation(var->location());

  // Don't emit declaration for dead variable.
  if(var->initializer() && !var->initializerInstruction()->getParent())
    return;

  // Create the declare instruction, giving it an initial position at
  // the end of the entry block (the insertDeclare call below doesn't
  // allow a NULL insert location, so we pick end-of-block arbitrarily).
  assert(entryBlock_);
  llvm::Instruction *decl =
      dibuilder().insertDeclare(var->value(), dilv, expr, vloc, entryBlock_);
  decl->removeFromParent();

  // Extract the decl from the end of the entry block and reposition
  // it according to the var properties.
  llvm::Instruction *insertionPoint = nullptr;
  if (var->initializer()) {
    assert(var->initializerInstruction()->getParent());
    insertionPoint = var->initializerInstruction();
  } else if (var->flavor() == ParamVar) {
    // parameters passing by reference may have no initializers.
    // declare them at function entry.
    entryBlock_->getInstList().push_front(decl);
    return;
  } else {
    // locals with no initializer should only be zero-sized vars.
    // make them available immediately after their alloca.
    assert(typemanager()->typeSize(var->btype()) == 0);
    llvm::Instruction *alloca = llvm::cast<llvm::Instruction>(var->value());
    insertionPoint = alloca;
  }

  assert(! llvm::isa<llvm::BranchInst>(insertionPoint));
  assert(insertionPoint != insertionPoint->getParent()->getTerminator());
  decl->insertAfter(insertionPoint);
}

void DIBuildHelper::endFunction(Bfunction *function)
{
  cleanFileScope();
  llvm::DISubprogram *fscope = llvm::cast<llvm::DISubprogram>(currentDIScope());

  // Create debug meta-data for parameter variables.
  unsigned argIdx = 0;
  for (auto &v : function->getParameterVars()) {
    llvm::DIFile *vfile = diFileFromLocation(v->location());
    llvm::DIType *vdit =
        typemanager()->buildDIType(v->btype(), *this);
    unsigned vline = linemap()->location_line(v->location());
    llvm::DILocalVariable *dilv =
        dibuilder().createParameterVariable(fscope, v->name(), ++argIdx,
                                            vfile, vline, vdit);
    insertVarDecl(v, dilv);
  }

  // Create debug meta-data for local variables. We wait to do this
  // here so as to insure that the initializer instructions for the
  // variables have been assigned to a basic block. Note that
  // block-scoped locals (as opposed to function-scoped locals) will
  // already have been processed at this point.
  processVarsInBLock(function->getFunctionLocalVars(), fscope);

  // If a given function has no debug locations at all, then don't
  // try to mark it as having debug info (without doing this we can
  // wind up having functions flagged as problematic by the verifier).
  if (known_locations_)
    function->function()->setSubprogram(fscope);

  // Done with this scope
  cleanFileScope();
  popDIScope();
  assert(diScopeStack_.size() == 1);

  // Clean up
  entryBlock_ = nullptr;
  topblock_ = nullptr;
  known_locations_ = 0;
  declared_.clear();
}

// Front end tends to declare more blocks than strictly required for
// debug generation purposes, so here we examine each block to see
// whether it makes sense to generate a DWARF lexical scope record for
// it. In particular, we look for blocks containing no user-visible
// variables (these can be omitted).
//
// Return TRUE if this is an interesting block (needs DWARF scope) or
// FALSE otherwise.

bool DIBuildHelper::interestingBlock(Bblock *block)
{
  assert(block);
  bool foundInteresting = false;
  for (auto &v : block->vars()) {
    if (! v->isTemporary()) {
      foundInteresting = true;
      break;
    }
  }
  return foundInteresting;
}

void DIBuildHelper::beginLexicalBlock(Bblock *block)
{
  if (! interestingBlock(block) || block == topblock_)
    return;

  // Register block with DIBuilder
  Location bloc = block->location();
  llvm::DIFile *file = diFileFromLocation(block->location());
  llvm::DILexicalBlock *dilb =
      dibuilder().createLexicalBlock(currentDIScope(), file,
                                     linemap()->location_line(bloc),
                                     linemap()->location_column(bloc));
  pushDIScope(dilb);
}

void DIBuildHelper::endLexicalBlock(Bblock *block)
{
  if (! interestingBlock(block))
    return;

  cleanFileScope();

  // In the case of the top block, we still want to process any local
  // variables it contains, but we'll want to insure that they are
  // parented by the function itself.
  //
  llvm::DIScope *scope =
      (block == topblock_ ? currentDIScope() : popDIScope());

  processVarsInBLock(block->vars(), scope);
}

void DIBuildHelper::processExprInst(Bstatement *stmt,
                                    Bexpression *expr,
                                    llvm::Instruction *inst)
{
  Location loc = expr->location();
  if (linemap()->is_unknown(loc)) {
    Location sloc = stmt->location();
    if (!linemap()->is_unknown(sloc))
      loc = sloc;
  }
  if (! linemap()->is_unknown(loc)) {
    known_locations_ += 1;
    inst->setDebugLoc(debugLocFromLocation(loc));
  }
}

void DIBuildHelper::addDebugPrefix(std::pair<llvm::StringRef, llvm::StringRef> pref)
{
  std::string from(pref.first);
  std::string to(pref.second);
  debugPrefixMap_[from] = to;
}

std::string DIBuildHelper::applyDebugPrefix(llvm::StringRef path) {
  for (const auto &remap : debugPrefixMap_)
    if (path.startswith(remap.first))
      return (llvm::Twine(remap.second) +
              path.substr(remap.first.size())).str();
  return path.str();
}

llvm::DIFile *DIBuildHelper::diFileFromLocation(Location location)
{
  std::string locfile = applyDebugPrefix(linemap()->location_file(location));
  llvm::StringRef locdir = llvm::sys::path::parent_path(locfile);
  llvm::StringRef locfilename = llvm::sys::path::filename(locfile);
  if (linemap()->is_predeclared(location))
    locdir = "";
  return dibuilder().createFile(locfilename, locdir);
}

llvm::DebugLoc DIBuildHelper::debugLocFromLocation(Location loc)
{
  llvm::LLVMContext &context = typemanager()->context();

  // In the (somewhat unusual) case of a file/line directive, create a new
  // pseudo-scope to capture the fact that the file has changed. Pop off
  // any previously created file scope prior to doing this.
  llvm::DIFile *curFile = currentDIScope()->getFile();
  llvm::DIFile *locFile = diFileFromLocation(loc);
  if (curFile != locFile) {
    cleanFileScope();
    llvm::DILexicalBlockFile *dilbf =
        dibuilder().createLexicalBlockFile(currentDIScope(), locFile);
    pushDIScope(dilbf);
  }

  return llvm::DILocation::get(context, linemap()->location_line(loc),
                               linemap()->location_column(loc),
                               currentDIScope());
}

llvm::DIScope *DIBuildHelper::currentDIScope()
{
  assert(diScopeStack_.size());
  return diScopeStack_.back();
}

llvm::DIScope *DIBuildHelper::popDIScope()
{
  assert(diScopeStack_.size());
  llvm::DIScope *popped = diScopeStack_.back();
  diScopeStack_.pop_back();
  return popped;
}

void DIBuildHelper::pushDIScope(llvm::DIScope *scope)
{
  assert(scope);
  diScopeStack_.push_back(scope);
}

void DIBuildHelper::cleanFileScope()
{
  assert(diScopeStack_.size());
  if (llvm::dyn_cast<llvm::DILexicalBlockFile>(diScopeStack_.back()) != nullptr)
    diScopeStack_.pop_back();
}
