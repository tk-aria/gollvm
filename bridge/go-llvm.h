//===-- go-llvm.h - LLVM implementation of gofrontend 'Backend' class -----===//
//
// Copyright 2018 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
//
//===----------------------------------------------------------------------===//
//
// Defines Llvm_backend and related classes
//
//===----------------------------------------------------------------------===//

#ifndef LLVMGOFRONTEND_GO_LLVM_H
#define LLVMGOFRONTEND_GO_LLVM_H

// Currently these need to be included before backend.h
#include "go-llvm-linemap.h"
#include "go-location.h"

// Definitions of Btype, Bexpression, and related B* classes.
#include "go-llvm-btype.h"
#include "go-llvm-bexpression.h"
#include "go-llvm-bstatement.h"
#include "go-llvm-bfunction.h"
#include "go-llvm-bvariable.h"

// Other helper classes
#include "go-llvm-containertypes.h"
#include "go-llvm-tree-integrity.h"
#include "go-llvm-typemanager.h"

#include "namegen.h"

#include "backend.h"

#include <unordered_map>
#include <unordered_set>

namespace llvm {
class Argument;
class ArrayType;
class BasicBlock;
class CallInst;
class Constant;
class ConstantFolder;
class DataLayout;
class DICompileUnit;
class DIFile;
class DIScope;
class DIBuilder;
class Function;
class Instruction;
class LLVMContext;
class Module;
class StructType;
class TargetLibraryInfo;
class Type;
class Value;
class raw_ostream;
}

class BuiltinEntry;
class BuiltinTable;
class BlockLIRBuilder;
class BinstructionsLIRBuilder;
struct GenCallState;

#include "llvm/IR/GlobalValue.h"

//
// LLVM-specific implementation of the Backend class; the code in
// gofrontend instantiates an object of this class and then invokes
// the various methods to convert its IR into LLVM IR. Nearly all of
// the interesting methods below are virtual.
//

class Llvm_backend : public Backend, public TypeManager, public NameGen {
public:
  Llvm_backend(llvm::LLVMContext &context,
               llvm::Module *module,
               Llvm_linemap *linemap);
  ~Llvm_backend();

  // Types.

  Btype *error_type();

  Btype *void_type();

  Btype *bool_type();

  Btype *integer_type(bool, int);

  Btype *float_type(int);

  Btype *complex_type(int);

  Btype *pointer_type(Btype *);

  Btype *function_type(const Btyped_identifier &,
                       const std::vector<Btyped_identifier> &,
                       const std::vector<Btyped_identifier> &, Btype *,
                       const Location);

  Btype *struct_type(const std::vector<Btyped_identifier> &);

  Btype *array_type(Btype *, Bexpression *);

  Btype *placeholder_pointer_type(const std::string &, Location, bool);

  bool set_placeholder_pointer_type(Btype *, Btype *);

  bool set_placeholder_function_type(Btype *, Btype *);

  Btype *placeholder_struct_type(const std::string &, Location);

  bool set_placeholder_struct_type(Btype *placeholder,
                                   const std::vector<Btyped_identifier> &);

  Btype *placeholder_array_type(const std::string &, Location);

  bool set_placeholder_array_type(Btype *, Btype *, Bexpression *);

  Btype *named_type(const std::string &, Btype *, Location);

  Btype *circular_pointer_type(Btype *, bool);

  bool is_circular_pointer_type(Btype *);

  int64_t type_size(Btype *);

  int64_t type_alignment(Btype *);

  int64_t type_field_alignment(Btype *);

  int64_t type_field_offset(Btype *, size_t index);

  // Expressions.

  Bexpression *zero_expression(Btype *);

  Bexpression *error_expression();

  Bexpression *nil_pointer_expression();

  Bexpression *var_expression(Bvariable *var, Location);

  Bexpression *indirect_expression(Btype *, Bexpression *expr, bool known_valid,
                                   Location);

  Bexpression *named_constant_expression(Btype *btype, const std::string &name,
                                         Bexpression *val, Location);

  Bexpression *integer_constant_expression(Btype *btype, mpz_t val);

  Bexpression *float_constant_expression(Btype *btype, mpfr_t val);

  Bexpression *complex_constant_expression(Btype *btype, mpc_t val);

  Bexpression *string_constant_expression(const std::string &val);

  Bexpression *boolean_constant_expression(bool val);

  Bexpression *real_part_expression(Bexpression *bcomplex, Location);

  Bexpression *imag_part_expression(Bexpression *bcomplex, Location);

  Bexpression *complex_expression(Bexpression *breal, Bexpression *bimag,
                                  Location);

  Bexpression *convert_expression(Btype *type, Bexpression *expr, Location);

  Bexpression *function_code_expression(Bfunction *, Location);

  Bexpression *address_expression(Bexpression *, Location);

  Bexpression *struct_field_expression(Bexpression *, size_t, Location);

  Bexpression *compound_expression(Bstatement *, Bexpression *, Location);

  Bexpression *conditional_expression(Bfunction *,
                                      Btype *, Bexpression *, Bexpression *,
                                      Bexpression *, Location);

  Bexpression *unary_expression(Operator, Bexpression *, Location);

  Bexpression *binary_expression(Operator, Bexpression *, Bexpression *,
                                 Location);

  Bexpression *
  constructor_expression(Btype *, const std::vector<Bexpression *> &, Location);

  Bexpression *array_constructor_expression(Btype *,
                                            const std::vector<unsigned long> &,
                                            const std::vector<Bexpression *> &,
                                            Location);

  Bexpression *pointer_offset_expression(Bexpression *base, Bexpression *offset,
                                         Location);

  Bexpression *array_index_expression(Bexpression *array, Bexpression *index,
                                      Location);

  Bexpression *call_expression(Bfunction *caller,
                               Bexpression *fn,
                               const std::vector<Bexpression *> &args,
                               Bexpression *static_chain,
                               Location);
  // Statements.

  Bstatement *error_statement();

  Bstatement *expression_statement(Bfunction *, Bexpression *);

  Bstatement *init_statement(Bfunction*, Bvariable *var, Bexpression *init);

  Bstatement *assignment_statement(Bfunction*,
                                   Bexpression *lhs, Bexpression *rhs,
                                   Location);

  Bstatement *return_statement(Bfunction *, const std::vector<Bexpression *> &,
                               Location);

  Bstatement *if_statement(Bfunction *func,
                           Bexpression *condition, Bblock *then_block,
                           Bblock *else_block, Location);

  Bstatement *
  switch_statement(Bfunction *function, Bexpression *value,
                   const std::vector<std::vector<Bexpression *>> &cases,
                   const std::vector<Bstatement *> &statements, Location);

  Bstatement *compound_statement(Bstatement *, Bstatement *);

  Bstatement *statement_list(const std::vector<Bstatement *> &);

  Bstatement *exception_handler_statement(Bstatement *bstat,
                                          Bstatement *except_stmt,
                                          Bstatement *finally_stmt, Location);

  // Blocks.

  Bblock *block(Bfunction *, Bblock *, const std::vector<Bvariable *> &,
                Location, Location);

  void block_add_statements(Bblock *, const std::vector<Bstatement *> &);

  Bstatement *block_statement(Bblock *);

  // Variables.

  Bvariable *error_variable();

  Bvariable *global_variable(const std::string &var_name,
                             const std::string &asm_name, Btype *btype,
                             bool is_external, bool is_hidden,
                             bool in_unique_section, Location location);

  void global_variable_set_init(Bvariable *, Bexpression *);

  Bvariable *local_variable(Bfunction *, const std::string &, Btype *,
                            Bvariable *, bool, Location);

  Bvariable *parameter_variable(Bfunction *, const std::string &, Btype *, bool,
                                Location);

  Bvariable *static_chain_variable(Bfunction *, const std::string &, Btype *,
                                   Location);

  Bvariable *temporary_variable(Bfunction *, Bblock *, Btype *, Bexpression *,
                                bool, Location, Bstatement **);

  Bvariable *implicit_variable(const std::string &, const std::string &,
                               Btype *, bool, bool, bool, int64_t);

  void implicit_variable_set_init(Bvariable *, const std::string &, Btype *,
                                  bool, bool, bool, Bexpression *);

  Bvariable *implicit_variable_reference(const std::string &,
                                         const std::string &, Btype *);

  Bvariable *immutable_struct(const std::string &, const std::string &, bool,
                              bool, Btype *, Location);

  void immutable_struct_set_init(Bvariable *, const std::string &, bool, bool,
                                 Btype *, Location, Bexpression *);

  Bvariable *immutable_struct_reference(const std::string &,
                                        const std::string &, Btype *, Location);

  // Labels.

  Blabel *label(Bfunction *, const std::string &name, Location);

  Bstatement *label_definition_statement(Blabel *);

  Bstatement *goto_statement(Blabel *, Location);

  Bexpression *label_address(Blabel *, Location);

  // Functions.

  Bfunction *error_function();

  Bfunction *function(Btype *fntype, const std::string &name,
                      const std::string &asm_name, bool is_visible,
                      bool is_declaration, bool is_inlinable,
                      bool disable_split_stack, bool does_not_return,
                      bool in_unique_section, Location);

  Bstatement *function_defer_statement(Bfunction *function,
                                       Bexpression *undefer, Bexpression *defer,
                                       Location);

  bool function_set_parameters(Bfunction *function,
                               const std::vector<Bvariable *> &);

  bool function_set_body(Bfunction *function, Bstatement *code_stmt);

  Bfunction *lookup_builtin(const std::string &);

  void write_global_definitions(const std::vector<Btype *> &,
                                const std::vector<Bexpression *> &,
                                const std::vector<Bfunction *> &,
                                const std::vector<Bvariable *> &);

  void write_export_data(const char *bytes, unsigned int size);

  Llvm_linemap *linemap() const { return linemap_; }

  // Module and datalayout
  llvm::Module &module() { return *module_; }
  const llvm::DataLayout &datalayout() { return *datalayout_; }

  // Type manager functionality
  TypeManager *typeManager() const;

  // DI build helper. Will be NULL if debug meta-data generation disabled.
  DIBuildHelper *dibuildhelper() { return dibuildhelper_.get(); }

  // Support for -fdebug-prefix=
  void addDebugPrefix(std::pair<llvm::StringRef, llvm::StringRef> prefix);

  // Bnode builder
  BnodeBuilder &nodeBuilder() { return nbuilder_; }

  // Finalize export data for the module. Exposed for unit testing.
  void finalizeExportData();

  // Run the module verifier.
  void verifyModule();

  // Dump LLVM IR for module
  void dumpModule();

  // Dump expression or stmt with line information. For debugging purposes.
  void dumpExpr(Bexpression *);
  void dumpStmt(Bstatement *);
  void dumpVar(Bvariable *);

  // Exposed for unit testing

  // Helpers to check tree integrity. Checks to make sure that we
  // don't have instructions that are parented by more than one
  // Bexpression or Bstatement, or Bexpressions parented by more than
  // one expr/stmt. Returns <TRUE,""> if tree is ok, otherwise returns
  // <FALSE,descriptive_message>.
  std::pair<bool, std::string>
  checkTreeIntegrity(Bnode *n, TreeIntegCtl control);

  // Similar to the above, but prints message to std::cerr and aborts if fail
  void enforceTreeIntegrity(Bnode *n);

  // Disable tree integrity checking. This is mainly
  // so that we can unit test the integrity checker.
  void disableIntegrityChecks();

  // Disable debug meta-data generation. Should be used only during
  // unit testing, where we're manufacturing IR that might not verify
  // if meta-data is created.
  void disableDebugMetaDataGeneration() { createDebugMetaData_ = false; }

  // Return true if this is a module-scope value such as a constant
  bool moduleScopeValue(llvm::Value *val, Btype *btype) const;

  // For debugging
  void setTraceLevel(unsigned level);
  unsigned traceLevel() const { return traceLevel_; }

  // Disable inlining if set to true.
  void setNoInline(bool b) { noInline_ = b; };

  // Disable frame pointer elimination if set to true.
  void setNoFpElim(bool b) { noFpElim_ = b; };

  // Target CPU and features
  void setTargetCpuAttr(const std::string &cpu);
  void setTargetFeaturesAttr(const std::string &attrs);

  // Set GC strategy
  void setGCStrategy(std::string s) { gcStrategy_ = s; };

  // Personality function
  llvm::Function *personalityFunction();

  // Dummy personality function
  llvm::Function *dummyPersonalityFunction();

 private:
  Bexpression *errorExpression() const { return errorExpression_; }
  Bstatement *errorStatement() const { return errorStatement_; }

  // create a Bfunction for an intrinsic function with specified name
  Bfunction *createIntrinsicFcn(const std::string &name,
                                llvm::Function *fcn);

  // create a Bfunction for a predefined builtin function. Used for
  // libcall builtins and inlined builtins.
  Bfunction *createBuiltinFcn(BuiltinEntry *be);

  // Certain Bexpressions we want to cache (constants for example,
  // or lvalue references to global variables). This helper looks up
  // the specified expr in a table keyed by <llvm::Value,Btype>. If
  // the lookup succeeds, the cached value is returned, otherwise the
  // specified Bexpression is installed in the table and returned.
  Bexpression *makeGlobalExpression(Bexpression *expr,
                                   llvm::Value *val,
                                   Btype *btype,
                                   Location location);

  enum ModVarConstant { MV_Constant, MV_NonConstant };
  enum ModVarSec { MV_UniqueSection, MV_DefaultSection };
  enum ModVarComdat { MV_InComdat, MV_NotInComdat };
  enum ModVarExtInit { MV_ExternallyInitialized, MV_NotExternallyInitialized };
  enum ModVarGenDebug { MV_GenDebug, MV_SkipDebug };

  // Make a module-scope variable (static, global, or external).
  Bvariable *makeModuleVar(Btype *btype,
                           const std::string &name,
                           const std::string &asm_name,
                           Location location,
                           ModVarConstant constant,
                           ModVarSec inUniqueSection,
                           ModVarComdat inComdat,
                           ModVarExtInit isExtInit,
                           ModVarGenDebug genDebug,
                           llvm::GlobalValue::LinkageTypes linkage,
                           llvm::Constant *initializer,
                           unsigned alignmentInBytes = 0);

  // Helper for creating a constant-valued array/struct expression.
  Bexpression *makeConstCompositeExpr(Btype *btype,
                                      llvm::CompositeType *llct,
                                      unsigned numElements,
                                      const std::vector<unsigned long> *indexes,
                                      const std::vector<Bexpression *> &vals,
                                      Location location);

  // Helper for creating a non-constant-valued array or struct expression.
  Bexpression *makeDelayedCompositeExpr(Btype *btype,
                                        llvm::CompositeType *llct,
                                        unsigned numElements,
                                        const std::vector<unsigned long> *idxs,
                                        const std::vector<Bexpression *> &vals,
                                        Location location);

  // Helper for creating a complex binary expression.
  Bexpression *makeComplexBinaryExpr(Operator op,
                                     Bexpression *left,
                                     Bexpression *right,
                                     Location location);

  // Helper for creating a complex conversion expression.
  Bexpression *makeComplexConvertExpr(Btype *type, Bexpression *expr,
                                      Location location);

  // Field GEP helper
  llvm::Value *makeFieldGEP(llvm::StructType *llst,
                            unsigned fieldIndex,
                            llvm::Value *sptr);

  // Array indexing GEP helper
  llvm::Value *makeArrayIndexGEP(llvm::ArrayType *at,
                                 llvm::Value *idx,
                                 llvm::Value *sptr);

  // Pointer indexing GEP helper
  llvm::Value *makePointerOffsetGEP(llvm::PointerType *pt,
                                    llvm::Value *idx,
                                    llvm::Value *sptr);

  // Assignment helper
  Bstatement *makeAssignment(Bfunction *function, llvm::Value *lvalue,
                             Bexpression *lhs, Bexpression *rhs, Location);

  // Helper to make init statement
  Bstatement *makeInitStatement(Bfunction *bfunction, Bvariable *var,
                              Bexpression *init);

  // Helper to make a temporary variable holding the value of an
  // expression
  std::pair<Bvariable*, Bstatement*> makeTempVar(Bexpression *expr,
                                                 Location location);

  // Helper to set up entry block for function
  llvm::BasicBlock *genEntryBlock(Bfunction *bfunction);

  // Helper to fix up epilog block for function (add return if needed)
  void fixupEpilogBlock(Bfunction *bfunction, llvm::BasicBlock *epilog);

  // Load-generation helper.
  // If resultTyp is null, this load is for resolving a pending
  // var expression, and the result type will be the same type
  // as space.
  Bexpression *genLoad(Bexpression *space,
                       Btype *resultTyp,
                       Location loc,
                       const std::string &tag);

  // Store generation helper. Creates store or memcpy call.
  Bexpression *genStore(Bfunction *func,
                        Bexpression *srcExpr,
                        Bexpression *dstExpr,
                        Location location);

  // Lower-level version of the above
  llvm::Value *genStore(BlockLIRBuilder *builder,
                        Btype *srcType,
                        bool srcConstant,
                        llvm::Type *dstType,
                        llvm::Value *srcValue,
                        llvm::Value *dstLoc);

  // Returns TRUE if loads/stores to/from the specified type should be
  // carried out via memcpy as opposed to concrete load/store
  // instructions.
  bool useCopyForLoadStore(Btype *typ) {
    assert(typ);
    assert(!typ->isPlaceholder());
    if (! typ->type()->isAggregateType())
      return false;
    //if (typeSize(typ) <= compositeSizeThreshold_)
    //  return false;
    return true;
  }

  // Similar to above but operates on LLVM type.
  bool useCopyForLoadStore(llvm::Type *typ) {
    assert(typ);
    if (! typ->isAggregateType())
      return false;
    //if (llvmTypeAllocSize(typ) <= compositeSizeThreshold_)
    //  return false;
    return true;
  }

  // Return context disposition based on expression type.
  // Composite values need to be referred to by address,
  // whereas non-composite values can be used directly.
  Varexpr_context varContextDisp(Bexpression *varexp);

  // Materialize a composite constant into a variable
  Bvariable *genVarForConstant(llvm::Constant *conval, Btype *type);

  // Examine vector of values to test whether they are constants.
  // Checks for and handles pending composite inits.
  static bool
  valuesAreConstant(const std::vector<Bexpression *> &vals);

  // Array init helper
  Bexpression *genArrayInit(llvm::ArrayType *llat,
                            Bexpression *expr,
                            llvm::Value *storage);

  // Struct init helper
  Bexpression *genStructInit(llvm::StructType *llst,
                             Bexpression *expr,
                             llvm::Value *storage);

  // Composite init management
  Bexpression *resolveCompositeInit(Bexpression *expr,
                                    llvm::Value *storage);
  // Var expr management
  Bexpression *resolveVarContext(Bexpression *expr,
                                 Varexpr_context ctx=VE_rvalue);

  // General-purpose resolver, handles var expr context and
  // composite init context.
  Bexpression *resolve(Bexpression *expr, Varexpr_context ctx=VE_rvalue);

  // Check a vector of Bexpression's to see if any are the
  // error expression, returning TRUE if so.
  bool exprVectorHasError(const std::vector<Bexpression *> &vals);

  // Check a vector of Bstatement's to see if any are the
  // error statement, returning TRUE if so.
  bool stmtVectorHasError(const std::vector<Bstatement *> &stmts);

  // Converts value "src" for assignment to container of type
  // "dstType" in assignment-like contexts. This helper exists to help
  // with cases where the frontend is creating an assignment of form
  // "X = Y" where X and Y's types are considered matching by the
  // front end, but are non-matching in an LLVM context. For example,
  //
  //   type Ifi func(int) int
  //   ...
  //   var fp Ifi = myfunctionfoobar
  //
  // Here the right hand side will come out as pointer-to-descriptor,
  // whereas variable "fp" will have type "pointer to functon", which are
  // not the same. Another example is assignments involving nil, e.g.
  //
  //   var ip *float32
  //   ...
  //   ip = nil
  //
  // The type of the right hand side of the assignment will be a
  // generic "*i64" as opposed to "*float32", since the backend
  // "nil_pointer_expression" does not allow for creation of nil
  // pointers of specific types.
  //
  // Return value will be a new convert Bexpression if a convert is
  // needed, NULL otherwise.
  llvm::Value *convertForAssignment(Bexpression *src,
                                    llvm::Type *dstToType);
  // lower-level version of the routine above
  llvm::Value *convertForAssignment(Btype *srcType,
                                    llvm::Value *srcVal,
                                    llvm::Type *dstToType,
                                    BlockLIRBuilder *builder);


  // Apply type conversion for a binary operation. This helper exists
  // to resolve situations where expressions are created by the front
  // end have incomplete or "polymorphic" type. A good example is pointer
  // comparison with nil, e.g.
  //
  //    var ip *A = foobar()
  //    if ip == nil { ...
  //
  // The type of the right hand side of the '==' above will be a generic
  // "*i64" as opposed to "*A", since the backend "nil_pointer_expression"
  // does not allow for creation of nil pointers of specific types.
  //
  // Input expressions 'left' and 'right' correspond to the original
  // uncoerced expressions; if conversions are needed, any additional
  // instructions will be added to the args and the resulting LLVM
  // values will be returned.
  std::pair<llvm::Value *, llvm::Value *>
  convertForBinary(Operator op, Bexpression *left, Bexpression *right);

  // Generate a conversion induced by the use of a circular pointer type.
  Bexpression *genCircularConversion(Btype *toType, Bexpression *expr,
                                     Location loc);

  // Helpers for call sequence generation.
  void genCallProlog(GenCallState &state);
  void genCallAttributes(GenCallState &state, llvm::CallInst *call);
  void genCallMarshallArgs(const std::vector<Bexpression *> &fn_args,
                           GenCallState &state);
  void genCallEpilog(GenCallState &state, llvm::Instruction *callInst,
                     Bexpression *callExpr);

  // Store the value in question to a temporary, returning the alloca
  // for the temp.
  llvm::Instruction *storeToTemporary(Bfunction *func, llvm::Value *val);

  // Generate a "late" type conversion (e.g. during materialization,
  // hence no delayed value creation).
  Bexpression *lateConvert(Btype *type, Bexpression *expr, Location);

  // Manufacture a floating point constant corresponding to -0.0
  Bexpression *minusZeroExpr(BFloatType *typ);

  // Create integer constant 1 (for use with type creation)
  Bexpression *makeIntegerOneExpr();

 public:

  // Performs a bottom-up walk to materialize LLVM values for each
  // node in the expression tree. Made public for unit testing.
  Bexpression *materialize(Bexpression *expr,
                           Varexpr_context lvalueContext=VE_rvalue);

  // Helper routines to materialize llvm::Value's for expression nodes,
  // invoked by routine above. Public primarily because we need to call
  // them from MaterializeVisitor (could be privatized if the materializer
  // was added as a friend).
  Bexpression *materializeIndirect(Bexpression *indExpr, bool isLHS);
  Bexpression *materializeAddress(Bexpression *addrExpr);
  Bexpression *materializeConversion(Bexpression *convExpr);
  Bexpression *materializeStructField(Bexpression *fieldExpr);
  Bexpression *materializeConditional(Bexpression *condExpr);
  Bexpression *materializeUnary(Bexpression *unaryExpr);
  Bexpression *materializeBinary(Bexpression *binExpr);
  Bexpression *materializeConstructor(Bexpression *conExpr);
  Bexpression *materializeComposite(Bexpression *comExpr);
  Bexpression *materializeCompound(Bexpression *comExpr);
  Bexpression *materializePointerOffset(Bexpression *ptroffExpr);
  Bexpression *materializeArrayIndex(Bexpression *arindExpr);
  Bexpression *materializeCall(Bexpression *callExpr);

 private:
  typedef std::pair<llvm::Value *, Btype *> valbtype;
  typedef pairvalmap<llvm::Value *, Btype *, Bexpression *>
  btyped_value_expr_maptyp;

  // Context information needed for the LLVM backend.
  llvm::LLVMContext &context_;

  // The LLVM module we're emitting IR into. If client did not supply
  // a module during construction, then ownModule_ is filled in.
  llvm::Module *module_;
  std::unique_ptr<llvm::Module> ownModule_;

  // Data layout info from the module.
  const llvm::DataLayout *datalayout_;

  // Builder for constructing Bexpressions and Bstatements.
  BnodeBuilder nbuilder_;

  // Debug info builder.
  std::unique_ptr<DIBuildHelper> dibuildhelper_;

  // Linemap to use. If client did not supply a linemap during
  // construction, then ownLinemap_ is filled in.
  Llvm_linemap *linemap_;
  std::unique_ptr<Llvm_linemap> ownLinemap_;

  // Address space designator for pointer types.
  unsigned addressSpace_;

  // Debug trace level
  unsigned traceLevel_;

  // Whether to disable inlining.
  bool noInline_;

  // Whether to disable frame pointer elimination.
  bool noFpElim_;

  // Whether to check for unexpected node sharing (e.g. same Bexpression
  // or statement pointed to by multiple parents).
  bool checkIntegrity_;

  // Whether to create debug meta data. On by default, can be
  // disabled for unit testing.
  bool createDebugMetaData_;

  // Whether we've started / finalized export data for the module.
  bool exportDataStarted_;
  bool exportDataFinalized_;

  // This counter gets incremented when the FE requests an error
  // object (error variable, error type, etc). We check to see whether
  // it is non-zero before walking function bodies to emit debug
  // meta-data (the idea being that there is no point going through
  // that process if there were errors).
  unsigned errorCount_;

  // Composite value load/store size threshold. Tells the bridge
  // that for any composite value whose size in bytes is greater than X,
  // emit memcpy operations for loads and stores as opposed to
  // emitting direct load/store instructions.
  unsigned compositeSizeThreshold_;

  // Target library info oracle
  llvm::TargetLibraryInfo *TLI_;

  // maps name to entry storing info on builtin function
  std::unique_ptr<BuiltinTable> builtinTable_;

  // Error function
  std::unique_ptr<Bfunction> errorFunction_;

  // Error expression
  Bexpression *errorExpression_;

  // Error statement
  Bstatement *errorStatement_;

  // Error variable
  std::unique_ptr<Bvariable> errorVariable_;

  // Map from LLVM-value/Btype pair to Bexpression. This is
  // used to cache + reuse things like global constants.
  btyped_value_expr_maptyp valueExprmap_;

  // Map from LLVM values to Bvariable. This is used for
  // module-scope variables, not vars local to a function.
  std::unordered_map<llvm::Value *, Bvariable *> valueVarMap_;

  // This is used to cache compiler-constructed vars to capture
  // constant values (created in genVarForConstant).
  std::unordered_map<llvm::Value *, Bvariable *> genVarConstMap_;

  // Table for commoning strings by value. String constants have
  // concrete types like "[5 x i8]", whereas we would like to return
  // things that have type "i8*". To manage this, we eagerly create
  // module-scope vars with strings, but this tends to defeat the
  // caching mechanisms, so here we have a map from constant string
  // value to Bexpression holding that string const.
  std::unordered_map<llvm::Value *, Bexpression*> stringConstantMap_;

  // For caching of immutable struct references. Similar situation here
  // as above, in that we can't look for such things in valueVarMap_
  // without creating what it is we're looking for.
  std::unordered_map<std::string, Bvariable *> immutableStructRefs_;

  typedef std::pair<BFunctionType *, std::string> fcnNameAndType;
  typedef pairvalmap<BFunctionType *, std::string, Bfunction*> fcnDeclMapTyp;

  // This maps from <name, type> pairs to Bfunction object; it is used
  // to cache declarations of external functions (for example,
  // well-known functions in the runtime). Only declarations will be
  // placed in this map-- if a function is being defined, it will only
  // be added to the functions_ list below.
  fcnDeclMapTyp fcnDeclMap_;

  // Currently we don't do any commoning of Bfunction objects created
  // by the frontend, so here we keep track of all returned Bfunctions
  // so that we can free them on exit.
  std::vector<Bfunction *> functions_;

  // Personality function
  llvm::Function *personalityFunction_;
  llvm::Function *dummyPersonalityFunction_;

  // Target cpu and attributes to be attached to any generated fcns.
  std::string targetCpuAttr_;
  std::string targetFeaturesAttr_;

  // GC strategy
  std::string gcStrategy_;
};

#endif
