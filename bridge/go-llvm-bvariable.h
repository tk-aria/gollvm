//===-- go-llvm-bvariable.h - decls for gofrontend 'Bvariable' class --===//
//
// Copyright 2018 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
//
//===----------------------------------------------------------------------===//
//
// Defines Bvariable and related classes.
//
//===----------------------------------------------------------------------===//

#ifndef LLVMGOFRONTEND_GO_LLVM_BVARIABLE_H
#define LLVMGOFRONTEND_GO_LLVM_BVARIABLE_H

#include "go-linemap.h"
#include "go-location.h"

#include "go-llvm-btype.h"

namespace llvm {
class Value;
class Instruction;
}

// Back end variable class

enum WhichVar { ParamVar, GlobalVar, LocalVar, BlockVar, ErrorVar };

class Bvariable {
public:

  // Constructor for regular/normal variables.
  Bvariable(Btype *type, Location location,
            const std::string &name, WhichVar which,
            bool address_taken, llvm::Value *value);

  // Constructor for zero-sized global variables. Here we have to play
  // tricks by creating a variable definition with an underlying
  // non-zero size, but then apply a conversion to the correct type
  // for references to the var.
  Bvariable(Btype *zeroSizeType, Btype *underlyingNonZeroSizeType,
            Location location, const std::string &name, WhichVar which,
            bool address_taken, llvm::Value *value);

  // Common to all varieties of variables
  Location location() { return location_; }
  Btype *btype() { return type_; }
  Btype *underlyingType() { return underlyingType_; }
  const std::string &name() { return name_; }
  llvm::Value *value() { return value_; }
  void setValue(llvm::Value *v) { value_ = v; }
  bool addrtaken() { return addrtaken_; }
  WhichVar flavor() const { return which_; }
  bool isTemporary() const { return temporary_; }
  void markAsTemporary() { temporary_ = true; }
  bool isDeclVar() const { return declvar_; }
  void markAsDeclVar() { declvar_ = true; }

  // Set/get variable initializer. Some variables may not have an
  // initializer, for example module-scoped globals, or variables
  // corresponding to by-address function params. Similarly, some
  // variables will have an initializer instruction, and others will
  // have just an initializer value.
  void setInitializer(llvm::Value *init);
  void setInitializerExpr(Bexpression *expr);
  llvm::Value *initializer() const { return initializer_; }
  llvm::Instruction *initializerInstruction() const;

  // debugging
  void dump();

  // dump with source line info
  void srcDump(Linemap *);

  // dump to raw_ostream
  void osdump(llvm::raw_ostream &os, unsigned ilevel = 0,
              Linemap *linemap = nullptr, bool terse = false);

private:
  Bvariable() = delete;
  const std::string name_;
  llvm::Value *value_;
  llvm::Value *initializer_;
  Btype *type_;
  Btype *underlyingType_;
  Location location_;
  WhichVar which_;
  bool addrtaken_;
  bool temporary_;
  bool declvar_;

  friend class Llvm_backend;
};

#endif // LLVMGOFRONTEND_GO_LLVM_BVARAIBLE_H
