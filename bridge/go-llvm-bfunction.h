//===-- go-llvm-bfunction.h - decls for gofrontend 'Bfunction' class ----===//
//
// Copyright 2018 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
//
//===----------------------------------------------------------------------===//
//
// Defines Bfunction and related classes.
//
//===----------------------------------------------------------------------===//

#ifndef LLVMGOFRONTEND_GO_LLVM_BFUNCTION_H
#define LLVMGOFRONTEND_GO_LLVM_BFUNCTION_H

// Currently these need to be included before backend.h
#include "go-linemap.h"
#include "go-location.h"
#include "go-llvm-btype.h"
#include "go-llvm-bexpression.h"

#include "namegen.h"
#include "backend.h"

namespace llvm {
class AllocaInst;
class Argument;
class BasicBlock;
class Constant;
class Function;
class Instruction;
class Value;
class raw_ostream;
}
class TypeManager;
class CABIOracle;
class CABIParamInfo;

// Class Bfunction encapsulates a reference to some llvm::Function.
// This can represent either a declaration of some external function,
// or a definition of some function within the current translation unit.
//
// For function declarations, the llvm::Value for this object will
// either be an llvm::Function or it may be a bitcast (type conversion)
// of a function.

class Bfunction : public NameGen {
public:
  Bfunction(llvm::Constant *fcnValue, BFunctionType *fcnType,
            const std::string &name, const std::string &asmName,
            Location location, TypeManager *tm);
  ~Bfunction();

  llvm::Constant *fcnValue() const { return fcnValue_; }
  void setFcnValue(llvm::Constant *fv) { fcnValue_ = fv; }
  llvm::Function *function() const;
  BFunctionType *fcnType() const { return fcnType_; }
  const std::string &name() const { return name_; }
  const std::string &asmName() const { return asmName_; }
  Location location() const { return location_; }

  enum SplitStackDisposition { YesSplit, NoSplit };
  void setSplitStack(SplitStackDisposition disp) { splitStack_ = disp; }
  SplitStackDisposition splitStack() const { return splitStack_; }

  // Add a local variable
  Bvariable *localVariable(const std::string &name,
                           Btype *btype,
                           Bvariable *decl_var,
                           bool is_address_taken,
                           Location location);

  // Add a parameter variable
  Bvariable *parameterVariable(const std::string &name,
                               Btype *btype,
                               bool is_address_taken,
                               Location location);

  // Create a Bvariable for the static chain param of the function.
  Bvariable *staticChainVariable(const std::string &name,
                                 Btype *btype,
                                 Location location);

  // Create a label address placeholder. This is a temporary construct
  // that we can use to record the use of a label address.
  llvm::Instruction *createLabelAddressPlaceholder(Btype *typ);

  // Replace all uses of the specified label address placeholder with
  // the specified real value.
  void replaceLabelAddressPlaceholder(llvm::Value *placeholder,
                                      llvm::BasicBlock *bbForLabel);

  // Record a new Bblock for this function.
  void addBlock(Bblock *block) { blocks_.push_back(block); }

  // Create a new label
  Blabel *newLabel(Location loc);

  // Register label def statement for label
  void registerLabelDefStatement(Bstatement *st, Blabel *label);

  // Create code to spill function arguments to entry block, insert
  // allocas for local variables.
  void genProlog(llvm::BasicBlock *entry);

  // Perform an necessary prolog fixups after instructions have been
  // assigned to LLVM basic blocks for the function. Here "entry" is
  // the the entry BB for the function, and "temps" is a set of temporary
  // variables that need to be adopted into the function.
  void fixupProlog(llvm::BasicBlock *entry,
                   const std::vector<llvm::AllocaInst *> &temps);

  // Create code to return a function value from this fcn, following ABI rules.
  llvm::Value *genReturnSequence(Bexpression *toRet,
                                 Binstructions *retInstrs,
                                 NameGen *namegen);

  // Return a vector of the local variables for the function. This will
  // not include block-scoped variables, only function-scoped locals.
  std::vector<Bvariable*> getFunctionLocalVars();

  // Return a vector of the parameter variables for the function.
  std::vector<Bvariable*> getParameterVars();

  // Return an alloca temporary of the specified type.
  llvm::Value *createTemporary(Btype *btype, const std::string &tag);
  llvm::Value *createTemporary(llvm::Type *type, const std::string &tag);

  // If the function return value is passing via memory instead of
  // directly, this function returns the location into which the
  // return has to go. Returns NULL if no return or direct return.
  llvm::Value *returnValueMem() const { return rtnValueMem_; }

  // Return the Bvariable corresponding to the Kth function parameter
  // (with respect to the abstract or high-level function type, not
  // the ABI type).  Exposed for unit testing.
  Bvariable *getNthParamVar(unsigned idx);

  // Get/set whether we've seen errors in this function. At the
  // moment this is used mainly to flag situations where there were
  // errors in the parameter declaration.
  bool errorSeen() const { return errorSeen_; }
  void setErrorSeen(bool val) { errorSeen_ = val; }

 private:

  // Perform ABI-related setup for this function.
  void lazyAbiSetup();

  // Generate code to spill a direct-passed var to a spill slot.
  unsigned genArgSpill(Bvariable *paramVar,
                       const CABIParamInfo &paramInfo,
                       Binstructions *spillInstructions,
                       llvm::Value *sploc);

  // Create an alloca with the specified type. The alloca is recorded
  // in a list so that it can be picked up during prolog generation.
  llvm::Instruction *addAlloca(llvm::Type *vtyp, const std::string &name);

  // Given an LLVM value, return the Bvariable we created to wrap it (either
  // local var or parameter var).
  Bvariable *getBvarForValue(llvm::Value *val);

  // for tmp name generation
  std::string namegen(const std::string &tag);

 private:

  // BFunctionType for this function.
  BFunctionType *fcnType_;

  // Function value for this Bfunction (either llvm::Function or bitcast)
  llvm::Constant *fcnValue_;

  // C ABI oracle for the function
  std::unique_ptr<CABIOracle> abiOracle_;

  // This includes all alloca's created for the function, including
  // local variables, temp vars, and spill locations for formal params.
  std::vector<llvm::Instruction *> allocas_;

  // Label address placeholders. To be deleted prior to finalization
  // of control flow for the function.
  std::set<llvm::Instruction *> labelAddressPlaceholders_;

  // List of local variables created for the function.
  std::vector<Bvariable *> localVariables_;

  // Blocks created for this function.
  std::vector<Bblock *> blocks_;

  // Maps LLVM value for a variable (for example, an alloc) to the
  // Bvariable used to represent the var.
  std::unordered_map<llvm::Value *, Bvariable *> valueVarMap_;

  // In the case where return value goes via memory,
  // rtnValueMem_ stores where we should store the value, otherwise
  // it will be NULL.
  llvm::Value *rtnValueMem_;

  // Holder the argument value for the static chain param of the function.
  llvm::Value *chainVal_;

  // Parameter values. Slot K in this vector will hold the llvm value
  // corresponding to the unpacked Kth parameter (this should be an
  // AllocaInst or an Argument).
  std::vector<llvm::Value *> paramValues_;

  // Function arguments. These reflect the rules of the ABI, hence the
  // length and types in this vector may not directly correspond to the
  // values above.
  std::vector<llvm::Argument *> arguments_;

  // This counts the number of formal params the front end has
  // registered so far via calls to parameter_variable.
  unsigned paramsRegistered_;

  // Maps label ID to defining statement
  std::vector<Bstatement *> labelmap_;

  // Stores all of the labels we've handed out.
  std::vector<Blabel *> labels_;

  // Function name and asm name
  std::string name_;
  std::string asmName_;

  // Location for this function.
  Location location_;

  // Whether this is a split-stack function.
  SplitStackDisposition splitStack_;

  // Whether prolog generation was completed successfully or not for
  // this function. May have been skipped due to errors or if we're
  // running unit tests.
  bool prologGenerated_;

  // Used to implement lazy ABI setup -- this avoids doing ABI setup on
  // external function we don't call, or in cases where there are errors.
  bool abiSetupComplete_;

  // Initially false; set to true in cases where we infer that an
  // error has taken place.  If a function is declared with N
  // parameters, but we only see N-2 parameter vars created, this is
  // typically an indication that a syntax error was encountered by
  // the FE somewhere along the line.
  bool errorSeen_;
};

#endif // LLVMGOFRONTEND_GO_LLVM_BFUNCTION_H
