//===-- go-llvm-dibuildhelper.cpp - implementation of DIBuildHelper -------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
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
  auto primaryFile =
      dibuilder_->createFile(linemap_->get_initial_file(), currentDir);
  bool isOptimized = true;
  std::string compileFlags; // FIXME
  unsigned runtimeVersion = 0; // not sure what would be for Go
  moduleScope_ =
      dibuilder_->createCompileUnit(llvm::dwarf::DW_LANG_Go, primaryFile,
                                    "llvm-goparse", isOptimized,
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
  bool isLocalToUnit = false; // FIXME -- look at exported/non-exported
  bool isDefinition = true;
  unsigned scopeLine = fcnLine; // FIXME -- determine correct value here
  llvm::DIFile *difile = diFileFromLocation(function->location());
  auto difunc =
      dibuilder().createFunction(moduleScope(), function->name(), function->asmName(),
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
  llvm::Instruction *insertionPoint = nullptr;
  llvm::Instruction *decl =
      dibuilder().insertDeclare(var->value(), dilv, expr, vloc, insertionPoint);
  if (var->initializer()) {
    assert(var->initializerInstruction()->getParent());
    insertionPoint = var->initializerInstruction();
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

  // In the case of the top block, we still want to process any local
  // variables it contains, but we'll want to insure that they are
  // parented by the function itself.
  //
  llvm::DIScope *scope =
      (block == topblock_ ? currentDIScope() : popDIScope());

  processVarsInBLock(block->vars(), scope);
}

void DIBuildHelper::processExprInst(Bexpression *expr, llvm::Instruction *inst)
{
  Location eloc = expr->location();
  // && !linemap()->is_predeclared(eloc)) {
  if (! linemap()->is_unknown(eloc)) {
    known_locations_ += 1;
    inst->setDebugLoc(debugLocFromLocation(eloc));
  }
}

llvm::DIFile *DIBuildHelper::diFileFromLocation(Location location)
{
  std::string locfile = linemap()->location_file(location);
  llvm::StringRef locdir = llvm::sys::path::parent_path(locfile);
  llvm::StringRef locfilename = llvm::sys::path::filename(locfile);
  if (linemap()->is_predeclared(location))
    locdir = "";
  return dibuilder().createFile(locfilename, locdir);
}

llvm::DebugLoc DIBuildHelper::debugLocFromLocation(Location loc)
{
  llvm::LLVMContext &context = typemanager()->context();
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
