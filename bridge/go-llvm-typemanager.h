//===-- go-llvm-typemanager.h - decls for 'TypeManager' class -------------===//
//
// Copyright 2018 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
//
//===----------------------------------------------------------------------===//
//
// Defines TypeManager class.
//
//===----------------------------------------------------------------------===//

#ifndef LLVMGOFRONTEND_GO_LLVM_TYPEMANAGER_H
#define LLVMGOFRONTEND_GO_LLVM_TYPEMANAGER_H

#include "go-linemap.h"
#include "go-location.h"
#include "go-llvm-btype.h"

#include "namegen.h"
#include "backend.h"

#include "llvm/IR/CallingConv.h"

namespace llvm {
class DataLayout;
class DIType;
class Instruction;
class LLVMContext;
class Module;
class Value;
class raw_ostream;
class FunctionType;
class StructType;
}

class DIBuildHelper;

using Btyped_identifier = Backend::Btyped_identifier;

enum PTDisp { Concrete, Placeholder };

class TypeManager {
 public:
  TypeManager(llvm::LLVMContext &context,
              llvm::CallingConv::ID cconv,
              unsigned addrspace);
  ~TypeManager();

  // These methods are intended to match up with the similarly
  // named Backend methods, e.g. errorType() corresponds to
  // Backend::error_type(), and so on.
  Btype *errorType();
  Btype *voidType();
  Btype *boolType();
  Btype *integerType(bool, int);
  Btype *floatType(int);
  Btype *complexType(int);
  Btype *pointerType(Btype *);
  Btype *functionType(const Btyped_identifier &,
                      const std::vector<Btyped_identifier> &,
                      const std::vector<Btyped_identifier> &,
                      Btype *, bool, const Location);
  Btype *structType(const std::vector<Btyped_identifier> &);
  Btype *arrayType(Btype *, Bexpression *);
  Btype *placeholderPointerType(const std::string &, Location, bool);
  bool setPlaceholderPointerType(Btype *, Btype *);
  bool setPlaceholderFunctionType(Btype *, Btype *);
  Btype *placeholderStructType(const std::string &, Location);
  bool setPlaceholderStructType(Btype *placeholder,
                                const std::vector<Btyped_identifier> &);
  Btype *placeholderArrayType(const std::string &, Location);
  bool setPlaceholderArrayType(Btype *, Btype *, Bexpression *);
  Btype *namedType(const std::string &, Btype *, Location);
  Btype *circularPointerType(Btype *, bool);
  bool isCircularPointerType(Btype *);
  bool isCircularPointerType(llvm::Type *);
  bool isCircularFunctionType(Btype *);
  bool isCircularFunctionType(llvm::Type *);
  int64_t typeSize(Btype *);
  int64_t typeAlignment(Btype *);
  int64_t typeFieldAlignment(Btype *);
  int64_t typeFieldOffset(Btype *, size_t index);

  // Create a new anonymous Btype based on LLVM type 'lt'. This is used
  // for types where there is a direct corresponding between the LLVM type
  // and the frontend type (ex: float32), and where we don't need to
  // do any later post-processing or checking.
  Btype *makeAuxType(llvm::Type *lt);

  // Create a new anonymous BFunctionType based on a corresponding
  // LLVM function type. This is slightly different from the routine
  // above in that it replicates information about parameter types and
  // results and creates a BFunctionType based on them (whereas
  // everything created by makeAuxTpe is an AuxT container). Not for
  // general use (should be used only for things like builtins), since
  // there is no way to express things like signed/unsigned param types.
  BFunctionType *makeAuxFcnType(llvm::FunctionType *eft);

  // Is this a placeholder type?
  bool isPlaceholderType(Btype *t);

  // Is this a Go boolean type
  bool isBooleanType(Btype *);

  // Replace the underlying type for a given placeholder type once
  // we've determined what the final type will be.
  void updatePlaceholderUnderlyingType(Btype *plt, Btype *totype);

  // Create an opaque type for use as part of a placeholder type.
  // Type will be named according to the tag passed in (name is relevant
  // only for debugging).
  llvm::Type *makeOpaqueLlvmType(const char *tag);

  // Precomputed LLVM types of various sorts.
  llvm::Type *llvmVoidType() const { return llvmVoidType_; }
  llvm::Type *llvmBoolType() const { return llvmBoolType_; }
  llvm::Type *llvmPtrType() const { return llvmPtrType_; }
  llvm::Type *llvmPtr0Type() const { return llvmPtr0Type_; } // pointer in address space 0
  llvm::Type *llvmInt8Type() const { return llvmInt8Type_; }
  llvm::Type *llvmInt32Type() const { return llvmInt32Type_; }
  llvm::Type *llvmInt64Type() const { return llvmInt64Type_; }
  llvm::Type *llvmIntegerType() const { return llvmIntegerType_; }
  llvm::Type *llvmSizeType() const { return llvmSizeType_; }
  llvm::Type *llvmFloatType() const { return llvmFloatType_; }
  llvm::Type *llvmDoubleType() const { return llvmDoubleType_; }
  llvm::Type *llvmLongDoubleType() const { return llvmLongDoubleType_; }
  llvm::Type *llvmTwoFloatVecType() const { return llvmTwoFloatVecType_; }
  llvm::Type *llvmArbitraryIntegerType(unsigned bytes);
  llvm::Type *landingPadExceptionType();
  llvm::FunctionType *personalityFunctionType();

  // Composite LLVM type helpers.
  static bool isLlvmCompositeType(llvm::Type *t);
  static llvm::Type *getLlvmTypeAtIndex(llvm::Type *t, unsigned i);

  // Size calculation methods for LLVM types.

  // Returns the offset in bytes between successive objects of a
  // given type as stored in memory (for example, in an array). This
  // includes alignment padding. For example, llvmTypeAllocSize() of
  // "struct { float x; char c; }" will be 8 bytes.
  uint64_t llvmTypeAllocSize(llvm::Type *t);

  // Returns number of bytes needed to hold the data in an object of
  // this type. For example, llvmTypeSize() of "struct { float x; char c; }"
  // will be 5 bytes.
  uint64_t llvmTypeSize(llvm::Type *t);

  // Return byte offset of field FIDX in llvm struct type LLST
  int64_t llvmTypeFieldOffset(llvm::StructType *llst, size_t fidx);

  // Context + address space.
  llvm::LLVMContext &context() const { return context_; }
  unsigned addressSpace() const { return addressSpace_; }

  // Same as pointerType, but explicitly specify address space.
  Btype *addrSpacePointerType(Btype *toType, unsigned addressSpace);

  // Go string type
  Btype *stringType() const { return stringType_; }

  // Go uintptr type. The FE manufactures this on its own, but there
  // are places where we need to materialize it in the bridge as well.
  Btype *uintPtrType() const { return uintPtrType_; }

  // LLVM type creation helpers
  llvm::Type *makeLLVMFloatType(int bits);
  llvm::Type *makeLLVMTwoElementStructType(llvm::Type *f1, llvm::Type *f2);
  llvm::Type *makeLLVMPointerType(llvm::Type *toTy);
  llvm::Type *makeLLVMStructType(const std::vector<Btyped_identifier> &fields);
  llvm::Type *makeLLVMStructType(const std::vector<llvm::Type *> &fields);
  llvm::Type *makeLLVMFunctionType(const std::vector<Btype *> &paramTypes,
                                   Btype *rbtype, bool followsCabi);

  // Returns field type from composite (struct/array) type and index.
  Btype *elementTypeByIndex(Btype *type, unsigned element_index);

  // Returns function type from pointer-to-function type or
  // pointer-to-function-descriptor type.
  BFunctionType *unpackFunctionType(Btype *fcnExprType);

  // When making a change to a Btype (for example,modifying its underlying
  // type or setting/resetting its placeholder flag) we need to
  // remove it from anonTypes and then reinstall it after we're
  // done making changes. These routines help with that process.
  // 'removeAnonType' returns true if the type in question was in
  // the anonTypes set.
  bool removeAnonType(Btype *typ);
  void reinstallAnonType(Btype *typ);

  // The specified placeholder 'btype' has been resolved to a
  // concrete type -- visit all of the types that refer to it
  // and see if we can completely resolve them.
  void postProcessResolvedPlaceholder(Btype *btype);

  // Helpers for the routine above
  void postProcessResolvedPointerPlaceholder(BPointerType *bpt, Btype *btype);
  void postProcessResolvedStructPlaceholder(BStructType *bst, Btype *btype);
  void postProcessResolvedArrayPlaceholder(BArrayType *bat, Btype *btype);
  void postProcessResolvedFunctionPlaceholder(BFunctionType *bft, Btype *btype);

  // For a newly create type, adds entries to the placeholderRefs
  // table for any contained types. Returns true if any placeholders
  // found.
  bool addPlaceholderRefs(Btype *type);

  // Helpers
  bool isFuncDescriptorType(llvm::Type *typ);
  bool isPtrToFuncDescriptorType(llvm::Type *typ);
  bool isPtrToIfaceStructType(llvm::Type *typ);
  bool isPtrToFuncType(llvm::Type *typ);
  bool isPtrToVoidType(llvm::Type *typ);
  bool isPtrToArrayOf(llvm::Type *ptyp, llvm::Type *arrayElmTyp);

  // This helper looks at two LLVM types and does a structural
  // comparison to determine if 'left' is equivalent to 'right' modulo
  // discrepancies between raw function pointers and "void *" (or
  // equivalent). This is to allow for cases where the front end will
  // store a function pointer in a table or struct somewhere as "void
  // *" instead of the precise function type.
  bool fcnPointerCompatible(llvm::Type *left,
                            llvm::Type *right,
                            std::set<llvm::Type *> &visited);

  // If specified type is a pointer flagged as being a circular
  // type, return conversion needed on load from that type, or NULL
  // if the type is not circular.
  Btype *circularTypeLoadConversion(Btype *typ);

  // Similar to the helper above, but for address operator.
  Btype *circularTypeAddrConversion(Btype *typ);

  // Create a dummy type to stand in for a zero-sized type, so as
  // to avoid creating variables that trigger linker bugs.
  Btype *synthesizeNonZeroSizeType(Btype *zeroSizeType, Bexpression *one);

  // Initialization helper. This passes in a few bits of
  // info from the parent backend that are might not immediately
  // available at the start of the containing constructor.
  void initializeTypeManager(Bexpression *errorExpression,
                             const llvm::DataLayout *datalayout,
                             NameGen *nt);

  // May be NULL prior to init call above
  const llvm::DataLayout *datalayout() const { return datalayout_; }

  // Calling convention
  llvm::CallingConv::ID callingConv() const { return cconv_; }

  // For named types, this returns the declared type name. If a type
  // is unnamed, then it returns a stringified representation of the
  // type (e.g, "[10]uint64").
  std::string typToString(Btype *typ);

  // Debug meta-data generation
  llvm::DIType *buildDIType(Btype *typ, DIBuildHelper &helper);

  // For debugging
  unsigned traceLevel() const { return traceLevel_; }
  void setTypeManagerTraceLevel(unsigned level) { traceLevel_ = level; }

  // for type name generation
  std::string tnamegen(const std::string &tag,
                       unsigned expl = NameGen::ChooseVer) {
    assert(nametags_);
    return nametags_->namegen(tag, expl);
  }

 private:

  std::string typToStringRec(Btype *typ, std::map<Btype *, std::string> &tab);

  llvm::DIType *buildStructDIType(BStructType *bst, DIBuildHelper &helper);

  std::vector<Btyped_identifier>
  sanitizeFields(const std::vector<Btyped_identifier> &fields);

  // For computing size-equivalent types for unresolved placeholders
  typedef std::unordered_map<Btype *, llvm::Type *> pproxymap;
  llvm::Type *placeholderProxyType(Btype *typ, pproxymap *pmap);

  // Checks for placeholder and invokes routine above if needed,
  // otherwise returns the LLVM type for the specified Btype.
  llvm::Type *getPlaceholderProxyIfNeeded(Btype *btype);

  // Context information needed for the LLVM backend.
  llvm::LLVMContext &context_;
  const llvm::DataLayout *datalayout_;
  llvm::CallingConv::ID cconv_;
  unsigned addressSpace_;
  unsigned traceLevel_;

  class btype_hash {
  public:
    unsigned int operator()(const Btype *t) const {
      return t->hash();
    }
  };

  class btype_equal {
  public:
    bool operator()(const Btype *t1, const Btype *t2) const {
      return t1->equal(*t2);
    }
  };

  typedef std::unordered_set<Btype *, btype_hash, btype_equal> anonTypeSetType;

  // Anonymous typed are hashed/commoned via this set.
  anonTypeSetType anonTypes_;

  // This map stores oddball types that get created internally by the
  // back end (ex: void type, or predefined complex). Key is LLVM
  // type, value is Btype.
  std::unordered_map<llvm::Type *, Btype *> auxTypeMap_;

  // Repository for named types (those specifically created by the
  // ::named_type method).
  std::unordered_set<Btype *> namedTypes_;

  // This maps a btype to the named type that was created from it.
  std::unordered_map<Btype *, Btype *> revNames_;

  // Records all placeholder types explicitly created via
  // Backend::placeholder_<XYZ>_type() method calls.
  std::unordered_set<Btype *> placeholders_;

  // These types became redundant/duplicate after one or more
  // of their placeholder children were updated.
  std::unordered_set<Btype *> duplicates_;

  // For managing placeholder types. An entry [X, {A,B,C}] indicates
  // that placeholder type X is referred to by the other placeholder
  // types A, B, and C.
  std::unordered_map<Btype *, std::set<Btype *> > placeholderRefs_;

  // Set of circular pointer types. These are pointers to opaque types that
  // are returned by the ::circular_pointer_type() method.
  std::unordered_set<llvm::Type *> circularPointerTypes_;

  // Map from placeholder type to circular pointer type. Key is placeholder
  // pointer type, value is circular pointer type marker.
  std::unordered_map<Btype *, Btype *> circularPointerTypeMap_;

  // Maps for inserting conversions involving circular pointers.
  std::unordered_map<llvm::Type *, Btype *> circularConversionLoadMap_;
  std::unordered_map<llvm::Type *, Btype *> circularConversionAddrMap_;

  // Set of top-level circular function types.
  std::unordered_set<Btype *> circularFunctionPlaceholderTypes_;

  // This set holds the marker types returned for the self-referential
  // elements within a circular function type, also any resolved LLVM
  // function types created from placeholders.
  std::unordered_set<llvm::Type *> circularFunctionTypes_;

  // Name generation helper
  NameGen *nametags_;

  // Error expression
  Bexpression *errorExpression_;

  // Various predefined or pre-computed types that we cache away
  Btype *errorType_;
  Btype *stringType_;
  Btype *uintPtrType_;
  llvm::Type *llvmVoidType_;
  llvm::Type *llvmBoolType_;
  llvm::Type *llvmPtrType_;
  llvm::Type *llvmPtr0Type_;
  llvm::Type *llvmSizeType_;
  llvm::Type *llvmIntegerType_;
  llvm::Type *llvmInt8Type_;
  llvm::Type *llvmInt32Type_;
  llvm::Type *llvmInt64Type_;
  llvm::Type *llvmFloatType_;
  llvm::Type *llvmDoubleType_;
  llvm::Type *llvmLongDoubleType_;
  llvm::Type *llvmTwoFloatVecType_;
};

#endif // LLVMGOFRONTEND_GO_LLVM_TYPEMANAGER_H
