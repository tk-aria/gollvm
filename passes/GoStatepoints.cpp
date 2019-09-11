//===- GoStatepoints.cpp - Insert statepoints for Go GC -------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// Rewrite call/invoke instructions so as to record live variables on
// stack for the use of garbage collector.
//
//===----------------------------------------------------------------------===//

#include "GoStatepoints.h"
#include "GoStackMap.h"
#include "GollvmPasses.h"

#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/BitVector.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/DenseSet.h"
#include "llvm/ADT/MapVector.h"
#include "llvm/ADT/None.h"
#include "llvm/ADT/Optional.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/SetVector.h"
#include "llvm/ADT/SmallSet.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/iterator_range.h"
#include "llvm/Analysis/DomTreeUpdater.h"
#include "llvm/Analysis/TargetLibraryInfo.h"
#include "llvm/Analysis/TargetTransformInfo.h"
#include "llvm/IR/Argument.h"
#include "llvm/IR/Attributes.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/CallingConv.h"
#include "llvm/IR/Constant.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Dominators.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/InstIterator.h"
#include "llvm/IR/InstrTypes.h"
#include "llvm/IR/Instruction.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/IntrinsicInst.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/MDBuilder.h"
#include "llvm/IR/Metadata.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Statepoint.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/User.h"
#include "llvm/IR/Value.h"
#include "llvm/IR/ValueHandle.h"
#include "llvm/Pass.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Compiler.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"
#include "llvm/Transforms/Utils/Local.h"
#include "llvm/Transforms/Utils/PromoteMemToReg.h"
#include <algorithm>
#include <cassert>
#include <cstddef>
#include <cstdint>
#include <iterator>
#include <set>
#include <string>
#include <utility>
#include <vector>

#define DEBUG_TYPE "go-statepoints"

using namespace llvm;
using namespace gollvm::passes;

// Print the liveset found at the insert location
static cl::opt<bool> PrintLiveSet("gogc-print-liveset", cl::Hidden,
                                  cl::init(false));

// Print the liveset only for the specified function.
static cl::opt<std::string> PrintFunc("gogc-print-func", cl::Hidden,
                                      cl::init(""));

// At each statepoint, clobber all the stack slots that are considered
// dead, for debugging purposes.
static cl::opt<bool> ClobberNonLive("gogc-clobber-non-live",
                                    cl::Hidden, cl::init(false));

// Statepoint ID. TODO: this is not thread safe.
static uint64_t ID = 0;

/// The IR fed into this pass may have had attributes and
/// metadata implying dereferenceability that are no longer valid/correct after
/// this pass has run. This is because semantically, after
/// this pass runs, all calls to gc.statepoint "free" the entire
/// heap. stripNonValidData (conservatively) restores
/// correctness by erasing all attributes in the module that externally imply
/// dereferenceability. Similar reasoning also applies to the noalias
/// attributes and metadata. gc.statepoint can touch the entire heap including
/// noalias objects.
/// Apart from attributes and metadata, we also remove instructions that imply
/// constant physical memory: llvm.invariant.start.
//
// TODO: revisit this. For a non-moving GC some attributes may still be valid.
// It probably doesn't really matter, as we run this pass at the end of
// optimization pipeline.
static void stripNonValidData(Module &M);

static bool shouldRewriteStatepointsIn(Function &F);

PreservedAnalyses GoStatepoints::run(Module &M,
                                     ModuleAnalysisManager &AM) {
  // Create a sentinel global variable for stack maps.
  Type *Int64Ty = Type::getInt64Ty(M.getContext());
  new GlobalVariable(M, Int64Ty, /* isConstant */ true,
                     GlobalValue::InternalLinkage,
                     ConstantInt::get(Int64Ty, GO_FUNC_SENTINEL),
                     GO_FUNC_SYM);

  bool Changed = false;
  auto &FAM = AM.getResult<FunctionAnalysisManagerModuleProxy>(M).getManager();
  for (Function &F : M) {
    // Nothing to do for declarations.
    if (F.isDeclaration() || F.empty())
      continue;

    // Policy choice says not to rewrite - the most common reason is that we're
    // compiling code without a GCStrategy.
    if (!shouldRewriteStatepointsIn(F))
      continue;

    auto &DT = FAM.getResult<DominatorTreeAnalysis>(F);
    auto &TTI = FAM.getResult<TargetIRAnalysis>(F);
    auto &TLI = FAM.getResult<TargetLibraryAnalysis>(F);
    Changed |= runOnFunction(F, DT, TTI, TLI);
  }
  if (!Changed)
    return PreservedAnalyses::all();

  // stripNonValidData asserts that shouldRewriteStatepointsIn
  // returns true for at least one function in the module.  Since at least
  // one function changed, we know that the precondition is satisfied.
  stripNonValidData(M);

  PreservedAnalyses PA;
  PA.preserve<TargetIRAnalysis>();
  PA.preserve<TargetLibraryAnalysis>();
  return PA;
}

namespace {

class GoStatepointsLegacyPass : public ModulePass {
  GoStatepoints Impl;

public:
  static char ID; // Pass identification, replacement for typeid

  GoStatepointsLegacyPass() : ModulePass(ID), Impl() {
    initializeGoStatepointsLegacyPassPass(
        *PassRegistry::getPassRegistry());
  }

  bool runOnModule(Module &M) override {
    // Create a sentinel global variable for stack maps.
    Type *Int64Ty = Type::getInt64Ty(M.getContext());
    new GlobalVariable(M, Int64Ty, /* isConstant */ true,
                       GlobalValue::InternalLinkage,
                       ConstantInt::get(Int64Ty, GO_FUNC_SENTINEL),
                       GO_FUNC_SYM);

    bool Changed = false;
    for (Function &F : M) {
      // Nothing to do for declarations.
      if (F.isDeclaration() || F.empty())
        continue;

      // Policy choice says not to rewrite - the most common reason is that
      // we're compiling code without a GCStrategy.
      if (!shouldRewriteStatepointsIn(F))
        continue;

      const TargetLibraryInfo &TLI =
          getAnalysis<TargetLibraryInfoWrapperPass>().getTLI(F);

      TargetTransformInfo &TTI =
          getAnalysis<TargetTransformInfoWrapperPass>().getTTI(F);
      auto &DT = getAnalysis<DominatorTreeWrapperPass>(F).getDomTree();

      Changed |= Impl.runOnFunction(F, DT, TTI, TLI);
    }

    if (!Changed)
      return false;

    // stripNonValidData asserts that shouldRewriteStatepointsIn
    // returns true for at least one function in the module.  Since at least
    // one function changed, we know that the precondition is satisfied.
    stripNonValidData(M);
    return true;
  }

  void getAnalysisUsage(AnalysisUsage &AU) const override {
    // We add and rewrite a bunch of instructions, but don't really do much
    // else.  We could in theory preserve a lot more analyses here.
    AU.addRequired<DominatorTreeWrapperPass>();
    AU.addRequired<TargetTransformInfoWrapperPass>();
    AU.addRequired<TargetLibraryInfoWrapperPass>();
  }
};

} // end anonymous namespace

char GoStatepointsLegacyPass::ID = 0;

ModulePass *llvm::createGoStatepointsLegacyPass() {
  return new GoStatepointsLegacyPass();
}

INITIALIZE_PASS_BEGIN(GoStatepointsLegacyPass,
                      "go-statepoints",
                      "Insert statepoints for Go GC", false, false)
INITIALIZE_PASS_DEPENDENCY(DominatorTreeWrapperPass)
INITIALIZE_PASS_DEPENDENCY(TargetTransformInfoWrapperPass)
INITIALIZE_PASS_END(GoStatepointsLegacyPass,
                    "go-statepoints",
                    "Insert statepoints for Go GC", false, false)

namespace {

// Liveness tracking has three parts: in-register values, non-address-taken
// allocas (stack slots), and address-taken allocas.
//
// In-register values are live since it is defined, until it has no more
// use. At statepoints they are spilled so the runtime can find them on
// the stack.
//
// Non-address-taken allocas are live since it is initialized, until it
// has no more use. At statepoints they are not spilled but its stack
// location is recorded.
//
// Address-taken allocas are live since it is initialized and remain live
// thereafter, unless an explicit lifetime.end is seen. As above, they
// are not spilled at statepoints but has its stack location recorded.
//
// In the data structure, there are some overlap. The live sets are used
// for both in-register values and non-address-taken allocas, but not
// for address-taken allocas. The alloca def/kill sets are used for both
// kinds of allocas.
struct GCPtrLivenessData {
  // In-register value and non-address-taken alloca.

  /// Values defined in this block.
  MapVector<BasicBlock *, SetVector<Value *>> KillSet;
  /// Values used in this block (and thus live); does not included values
  /// killed within this block.
  MapVector<BasicBlock *, SetVector<Value *>> LiveSet;
  /// Values live into this basic block (i.e. used by any
  /// instruction in this basic block or ones reachable from here)
  MapVector<BasicBlock *, SetVector<Value *>> LiveIn;
  /// Values live out of this basic block (i.e. live into
  /// any successor block)
  MapVector<BasicBlock *, SetVector<Value *>> LiveOut;

  // Alloca liveness.

  MapVector<BasicBlock *, SetVector<Value *>> AllocaDefSet;  // initialized in BB
  MapVector<BasicBlock *, SetVector<Value *>> AllocaKillSet; // killed (lifetime.end) in BB

  // Unlike above, these are propagated forwards, instead of backwards.
  MapVector<BasicBlock *, SetVector<Value *>> AllocaDefAny; // initialized at any path reaching the end of BB
  MapVector<BasicBlock *, SetVector<Value *>> AllocaDefAll; // initialized at all paths reaching the end of BB
};

// The type of the internal cache used inside the findBasePointers family
// of functions.  From the callers perspective, this is an opaque type and
// should not be inspected.
//
// In the actual implementation this caches two relations:
// - The base relation itself (i.e. this pointer is based on that one)
// - The base defining value relation (i.e. before base_phi insertion)
// Generally, after the execution of a full findBasePointer call, only the
// base relation will remain.  Internally, we add a mixture of the two
// types, then update all the second type to the first type
using DefiningValueMapTy = MapVector<Value *, Value *>;
using StatepointLiveSetTy = SetVector<Value *>;
using RematerializedValueMapTy =
    MapVector<AssertingVH<Instruction>, AssertingVH<Value>>;

struct PartiallyConstructedSafepointRecord {
  /// The set of values known to be live across this safepoint
  StatepointLiveSetTy LiveSet;

  /// Mapping from live pointers to a base-defining-value
  MapVector<Value *, Value *> PointerToBase;

  /// The *new* gc.statepoint instruction itself.  This produces the token
  /// that normal path gc.relocates and the gc.result are tied to.
  Instruction *StatepointToken;

  /// Instruction to which exceptional gc relocates are attached
  /// Makes it easier to iterate through them during relocationViaAlloca.
  Instruction *UnwindToken;

  /// Record live values we are rematerialized instead of relocating.
  /// They are not included into 'LiveSet' field.
  /// Maps rematerialized copy to it's original value.
  RematerializedValueMapTy RematerializedValues;
};

} // end anonymous namespace

/// Compute the live-in set for every basic block in the function
static void computeLiveInValues(DominatorTree &DT, Function &F,
                                GCPtrLivenessData &Data,
                                SetVector<Value *> &AddrTakenAllocas,
                                SetVector<Value *> &ToZero,
                                SetVector<Value *> &BadLoads,
                                DefiningValueMapTy &DVCache);

/// Given results from the dataflow liveness computation, find the set of live
/// Values at a particular instruction.
static void findLiveSetAtInst(Instruction *inst, GCPtrLivenessData &Data,
                              SetVector<Value *> &AddrTakenAllocas,
                              StatepointLiveSetTy &out,
                              SetVector<Value *> &AllAllocas,
                              DefiningValueMapTy &DVCache);

// TODO: Once we can get to the GCStrategy, this becomes
// Optional<bool> isGCManagedPointer(const Type *Ty) const override {

static bool isGCPointerType(Type *T) {
  return isa<PointerType>(T);
}

// Return true if this type is one which a) is a gc pointer or contains a GC
// pointer and b) is of a type this code expects to encounter as a live value.
// (The insertion code will assert that a type which matches (a) and not (b)
// is not encountered.)
static bool isHandledGCPointerType(Type *T) {
  // We fully support gc pointers
  if (isGCPointerType(T))
    return true;
  // We partially support vectors of gc pointers. The code will assert if it
  // can't handle something.
  if (auto VT = dyn_cast<VectorType>(T))
    if (isGCPointerType(VT->getElementType()))
      return true;
  // FCA is supported.
  if (T->isStructTy())
    return hasPointer(T);
  return false;
}

#ifndef NDEBUG
/// Returns true if this type contains a gc pointer whether we know how to
/// handle that type or not.
static bool containsGCPtrType(Type *Ty) {
  if (isGCPointerType(Ty))
    return true;
  if (VectorType *VT = dyn_cast<VectorType>(Ty))
    return isGCPointerType(VT->getScalarType());
  if (ArrayType *AT = dyn_cast<ArrayType>(Ty))
    return containsGCPtrType(AT->getElementType());
  if (StructType *ST = dyn_cast<StructType>(Ty))
    return llvm::any_of(ST->subtypes(), containsGCPtrType);
  return false;
}

// Returns true if this is a type which a) is a gc pointer or contains a GC
// pointer and b) is of a type which the code doesn't expect (i.e. first class
// aggregates).  Used to trip assertions.
static bool isUnhandledGCPointerType(Type *Ty) {
  return containsGCPtrType(Ty) && !isHandledGCPointerType(Ty);
}
#endif

// Return the name of the value suffixed with the provided value, or if the
// value didn't have a name, the default value specified.
static std::string suffixed_name_or(Value *V, StringRef Suffix,
                                    StringRef DefaultName) {
  return V->hasName() ? (V->getName() + Suffix).str() : DefaultName.str();
}

// Helper function to print a live set, for debugging.
static void
printLiveSet(SetVector<Value *> &LiveSet) {
  for (Value *V : LiveSet)
    dbgs() << "\t" << *V << "\n";
}

// Conservatively identifies any definitions which might be live at the
// given instruction. The  analysis is performed immediately before the
// given instruction. Values defined by that instruction are not considered
// live.  Values used by that instruction are considered live.
static void
analyzeParsePointLiveness(DominatorTree &DT,
                          GCPtrLivenessData &OriginalLivenessData,
                          SetVector<Value *> &AddrTakenAllocas, CallBase *Call,
                          PartiallyConstructedSafepointRecord &Result,
                          SetVector<Value *> &AllAllocas,
                          DefiningValueMapTy &DVCache) {
  StatepointLiveSetTy LiveSet;
  findLiveSetAtInst(Call, OriginalLivenessData, AddrTakenAllocas,
                    LiveSet, AllAllocas, DVCache);

  if (PrintLiveSet) {
    dbgs() << "Live Variables at " << *Call << ":\n";
    printLiveSet(LiveSet);
  }
  Result.LiveSet = LiveSet;
}

static bool isKnownBaseResult(Value *V);

namespace {

/// A single base defining value - An immediate base defining value for an
/// instruction 'Def' is an input to 'Def' whose base is also a base of 'Def'.
/// For instructions which have multiple pointer [vector] inputs or that
/// transition between vector and scalar types, there is no immediate base
/// defining value.  The 'base defining value' for 'Def' is the transitive
/// closure of this relation stopping at the first instruction which has no
/// immediate base defining value.  The b.d.v. might itself be a base pointer,
/// but it can also be an arbitrary derived pointer.
struct BaseDefiningValueResult {
  /// Contains the value which is the base defining value.
  Value * const BDV;

  /// True if the base defining value is also known to be an actual base
  /// pointer.
  const bool IsKnownBase;

  BaseDefiningValueResult(Value *BDV, bool IsKnownBase)
    : BDV(BDV), IsKnownBase(IsKnownBase) {
#ifndef NDEBUG
    // Check consistency between new and old means of checking whether a BDV is
    // a base.
    bool MustBeBase = isKnownBaseResult(BDV);
    assert(!MustBeBase || MustBeBase == IsKnownBase);
#endif
  }
};

} // end anonymous namespace

static BaseDefiningValueResult findBaseDefiningValue(Value *I);

/// Return a base defining value for the 'Index' element of the given vector
/// instruction 'I'.  If Index is null, returns a BDV for the entire vector
/// 'I'.  As an optimization, this method will try to determine when the
/// element is known to already be a base pointer.  If this can be established,
/// the second value in the returned pair will be true.  Note that either a
/// vector or a pointer typed value can be returned.  For the former, the
/// vector returned is a BDV (and possibly a base) of the entire vector 'I'.
/// If the later, the return pointer is a BDV (or possibly a base) for the
/// particular element in 'I'.
static BaseDefiningValueResult
findBaseDefiningValueOfVector(Value *I) {
  // Each case parallels findBaseDefiningValue below, see that code for
  // detailed motivation.

  if (isa<Argument>(I))
    // An incoming argument to the function is a base pointer
    return BaseDefiningValueResult(I, true);

  if (isa<Constant>(I))
    // Base of constant vector consists only of constant null pointers.
    // For reasoning see similar case inside 'findBaseDefiningValue' function.
    return BaseDefiningValueResult(ConstantAggregateZero::get(I->getType()),
                                   true);

  if (isa<LoadInst>(I))
    return BaseDefiningValueResult(I, true);

  if (isa<InsertElementInst>(I))
    // We don't know whether this vector contains entirely base pointers or
    // not.  To be conservatively correct, we treat it as a BDV and will
    // duplicate code as needed to construct a parallel vector of bases.
    return BaseDefiningValueResult(I, false);

  if (isa<ShuffleVectorInst>(I))
    // We don't know whether this vector contains entirely base pointers or
    // not.  To be conservatively correct, we treat it as a BDV and will
    // duplicate code as needed to construct a parallel vector of bases.
    // TODO: There a number of local optimizations which could be applied here
    // for particular sufflevector patterns.
    return BaseDefiningValueResult(I, false);

  // The behavior of getelementptr instructions is the same for vector and
  // non-vector data types.
  if (auto *GEP = dyn_cast<GetElementPtrInst>(I))
    return findBaseDefiningValue(GEP->getPointerOperand());

  // If the pointer comes through a bitcast of a vector of pointers to
  // a vector of another type of pointer, then look through the bitcast
  if (auto *BC = dyn_cast<BitCastInst>(I))
    return findBaseDefiningValue(BC->getOperand(0));

  // We assume that functions in the source language only return base
  // pointers.  This should probably be generalized via attributes to support
  // both source language and internal functions.
  if (isa<CallInst>(I) || isa<InvokeInst>(I))
    return BaseDefiningValueResult(I, true);

  // A PHI or Select is a base defining value.  The outer findBasePointer
  // algorithm is responsible for constructing a base value for this BDV.
  assert((isa<SelectInst>(I) || isa<PHINode>(I)) &&
         "unknown vector instruction - no base found for vector element");
  return BaseDefiningValueResult(I, false);
}

/// Helper function for findBasePointer - Will return a value which either a)
/// defines the base pointer for the input, b) blocks the simple search
/// (i.e. a PHI or Select of two derived pointers), or c) involves a change
/// from pointer to vector type or back.
static BaseDefiningValueResult findBaseDefiningValue(Value *I) {
  if (I->getType()->isStructTy())
    // Assuming FCA is always base.
    // FCAs appear mostly in the call sequnce where we pass/return multiple
    // values in registers, e.g. { i8*, i64 }. If it contains the address of
    // an alloca, the alloca should already be address taken (at least when
    // creating the FCA), so we don't need to link the FCA back to the alloca.
    // It is also unlikely to contain past-the-end pointer (we cannot do
    // pointer arithmetic directly with FCA). So it is safe to treat FCA as
    // base.
    return BaseDefiningValueResult(I, true);

  assert(I->getType()->isPtrOrPtrVectorTy() &&
         "Illegal to ask for the base pointer of a non-pointer type");

  if (I->getType()->isVectorTy())
    return findBaseDefiningValueOfVector(I);

  if (isa<Argument>(I))
    // An incoming argument to the function is a base pointer
    // We should have never reached here if this argument isn't an gc value
    return BaseDefiningValueResult(I, true);

  if (isa<Constant>(I)) {
    // We assume that objects with a constant base (e.g. a global) can't move
    // and don't need to be reported to the collector because they are always
    // live. Besides global references, all kinds of constants (e.g. undef,
    // constant expressions, null pointers) can be introduced by the inliner or
    // the optimizer, especially on dynamically dead paths.
    // Here we treat all of them as having single null base. By doing this we
    // trying to avoid problems reporting various conflicts in a form of
    // "phi (const1, const2)" or "phi (const, regular gc ptr)".
    // See constant.ll file for relevant test cases.

    return BaseDefiningValueResult(
        ConstantPointerNull::get(cast<PointerType>(I->getType())), true);
  }

  if (CastInst *CI = dyn_cast<CastInst>(I)) {
    Value *Def = CI->stripPointerCasts();
    if (isa<IntToPtrInst>(Def))
      // Pointer converted from integer is a base.
      return BaseDefiningValueResult(Def, true);

    // Pointer-to-pointer and int-to-pointer casts are handled above.
    // We don't know how to handle other type of casts.
    assert(!isa<CastInst>(Def) && "shouldn't find another cast here");
    return findBaseDefiningValue(Def);
  }

  if (isa<AllocaInst>(I))
    // alloca is a gc base
    return BaseDefiningValueResult(I, true);

  if (isa<LoadInst>(I))
    // The value loaded is a gc base itself
    return BaseDefiningValueResult(I, true);

  if (GetElementPtrInst *GEP = dyn_cast<GetElementPtrInst>(I))
    // The base of this GEP is the base
    return findBaseDefiningValue(GEP->getPointerOperand());

  if (IntrinsicInst *II = dyn_cast<IntrinsicInst>(I)) {
    switch (II->getIntrinsicID()) {
    default:
      // fall through to general call handling
      break;
    case Intrinsic::experimental_gc_statepoint:
      llvm_unreachable("statepoints don't produce pointers");
    case Intrinsic::experimental_gc_relocate:
      // Rerunning safepoint insertion after safepoints are already
      // inserted is not supported.  It could probably be made to work,
      // but why are you doing this?  There's no good reason.
      llvm_unreachable("repeat safepoint insertion is not supported");
    case Intrinsic::gcroot:
      // Currently, this mechanism hasn't been extended to work with gcroot.
      // There's no reason it couldn't be, but I haven't thought about the
      // implications much.
      llvm_unreachable(
          "interaction with the gcroot mechanism is not supported");
    }
  }
  // We assume that functions in the source language only return base
  // pointers.  This should probably be generalized via attributes to support
  // both source language and internal functions.
  if (isa<CallInst>(I) || isa<InvokeInst>(I))
    return BaseDefiningValueResult(I, true);

  // TODO: I have absolutely no idea how to implement this part yet.  It's not
  // necessarily hard, I just haven't really looked at it yet.
  assert(!isa<LandingPadInst>(I) && "Landing Pad is unimplemented");

  if (isa<AtomicCmpXchgInst>(I))
    // A CAS is effectively a atomic store and load combined under a
    // predicate.  From the perspective of base pointers, we just treat it
    // like a load.
    return BaseDefiningValueResult(I, true);

  assert(!isa<AtomicRMWInst>(I) && "Xchg handled above, all others are "
                                   "binary ops which don't apply to pointers");

  // The aggregate ops.  Aggregates can either be in the heap or on the
  // stack, but in either case, this is simply a field load.  As a result,
  // this is a defining definition of the base just like a load is.
  if (isa<ExtractValueInst>(I))
    return BaseDefiningValueResult(I, true);

  // We should never see an insert vector since that would require we be
  // tracing back a struct value not a pointer value.
  assert(!isa<InsertValueInst>(I) &&
         "Base pointer for a struct is meaningless");

  // An extractelement produces a base result exactly when it's input does.
  // We may need to insert a parallel instruction to extract the appropriate
  // element out of the base vector corresponding to the input. Given this,
  // it's analogous to the phi and select case even though it's not a merge.
  if (isa<ExtractElementInst>(I))
    // Note: There a lot of obvious peephole cases here.  This are deliberately
    // handled after the main base pointer inference algorithm to make writing
    // test cases to exercise that code easier.
    return BaseDefiningValueResult(I, false);

  // The last two cases here don't return a base pointer.  Instead, they
  // return a value which dynamically selects from among several base
  // derived pointers (each with it's own base potentially).  It's the job of
  // the caller to resolve these.
  assert((isa<SelectInst>(I) || isa<PHINode>(I)) &&
         "missing instruction case in findBaseDefiningValing");
  return BaseDefiningValueResult(I, false);
}

/// Returns the base defining value for this value.
static Value *findBaseDefiningValueCached(Value *I, DefiningValueMapTy &Cache) {
  Value *&Cached = Cache[I];
  if (!Cached) {
    Cached = findBaseDefiningValue(I).BDV;
    LLVM_DEBUG(dbgs() << "fBDV-cached: " << I->getName() << " -> "
                      << Cached->getName() << "\n");
  }
  assert(Cache[I] != nullptr);
  return Cached;
}

/// Return a base pointer for this value if known.  Otherwise, return it's
/// base defining value.
static Value *findBaseOrBDV(Value *I, DefiningValueMapTy &Cache) {
  Value *Def = findBaseDefiningValueCached(I, Cache);
  auto Found = Cache.find(Def);
  if (Found != Cache.end()) {
    // Either a base-of relation, or a self reference.  Caller must check.
    return Found->second;
  }
  // Only a BDV available
  return Def;
}

/// Given the result of a call to findBaseDefiningValue, or findBaseOrBDV,
/// is it known to be a base pointer?  Or do we need to continue searching.
static bool isKnownBaseResult(Value *V) {
  if (!isa<PHINode>(V) && !isa<SelectInst>(V) &&
      !isa<ExtractElementInst>(V) && !isa<InsertElementInst>(V) &&
      !isa<ShuffleVectorInst>(V)) {
    // no recursion possible
    return true;
  }
  if (isa<Instruction>(V) &&
      cast<Instruction>(V)->getMetadata("is_base_value")) {
    // This is a previously inserted base phi or select.  We know
    // that this is a base value.
    return true;
  }

  // We need to keep searching
  return false;
}

namespace {

/// Models the state of a single base defining value in the findBasePointer
/// algorithm for determining where a new instruction is needed to propagate
/// the base of this BDV.
class BDVState {
public:
  enum Status { Unknown, Base, Conflict };

  BDVState() : BaseValue(nullptr) {}

  explicit BDVState(Status Status, Value *BaseValue = nullptr)
      : Status(Status), BaseValue(BaseValue) {
    assert(Status != Base || BaseValue);
  }

  explicit BDVState(Value *BaseValue) : Status(Base), BaseValue(BaseValue) {}

  Status getStatus() const { return Status; }
  Value *getBaseValue() const { return BaseValue; }

  bool isBase() const { return getStatus() == Base; }
  bool isUnknown() const { return getStatus() == Unknown; }
  bool isConflict() const { return getStatus() == Conflict; }

  bool operator==(const BDVState &Other) const {
    return BaseValue == Other.BaseValue && Status == Other.Status;
  }

  bool operator!=(const BDVState &other) const { return !(*this == other); }

  LLVM_DUMP_METHOD
  void dump() const {
    print(dbgs());
    dbgs() << '\n';
  }

  void print(raw_ostream &OS) const {
    switch (getStatus()) {
    case Unknown:
      OS << "U";
      break;
    case Base:
      OS << "B";
      break;
    case Conflict:
      OS << "C";
      break;
    }
    OS << " (" << getBaseValue() << " - "
       << (getBaseValue() ? getBaseValue()->getName() : "nullptr") << "): ";
  }

private:
  Status Status = Unknown;
  AssertingVH<Value> BaseValue; // Non-null only if Status == Base.
};

} // end anonymous namespace

#ifndef NDEBUG
static raw_ostream &operator<<(raw_ostream &OS, const BDVState &State) {
  State.print(OS);
  return OS;
}
#endif

static BDVState meetBDVStateImpl(const BDVState &LHS, const BDVState &RHS) {
  switch (LHS.getStatus()) {
  case BDVState::Unknown:
    return RHS;

  case BDVState::Base:
    assert(LHS.getBaseValue() && "can't be null");
    if (RHS.isUnknown())
      return LHS;

    if (RHS.isBase()) {
      if (LHS.getBaseValue() == RHS.getBaseValue()) {
        assert(LHS == RHS && "equality broken!");
        return LHS;
      }
      return BDVState(BDVState::Conflict);
    }
    assert(RHS.isConflict() && "only three states!");
    return BDVState(BDVState::Conflict);

  case BDVState::Conflict:
    return LHS;
  }
  llvm_unreachable("only three states!");
}

// Values of type BDVState form a lattice, and this function implements the meet
// operation.
static BDVState meetBDVState(const BDVState &LHS, const BDVState &RHS) {
  BDVState Result = meetBDVStateImpl(LHS, RHS);
  assert(Result == meetBDVStateImpl(RHS, LHS) &&
         "Math is wrong: meet does not commute!");
  return Result;
}

/// For a given value or instruction, figure out what base ptr its derived from.
/// For gc objects, this is simply itself.  On success, returns a value which is
/// the base pointer.  (This is reliable and can be used for relocation.)  On
/// failure, returns nullptr.
static Value *findBasePointer(Value *I, DefiningValueMapTy &Cache) {
  Value *Def = findBaseOrBDV(I, Cache);

  if (isKnownBaseResult(Def))
    return Def;

  // Here's the rough algorithm:
  // - For every SSA value, construct a mapping to either an actual base
  //   pointer or a PHI which obscures the base pointer.
  // - Construct a mapping from PHI to unknown TOP state.  Use an
  //   optimistic algorithm to propagate base pointer information.  Lattice
  //   looks like:
  //   UNKNOWN
  //   b1 b2 b3 b4
  //   CONFLICT
  //   When algorithm terminates, all PHIs will either have a single concrete
  //   base or be in a conflict state.
  // - For every conflict, insert a dummy PHI node without arguments.  Add
  //   these to the base[Instruction] = BasePtr mapping.  For every
  //   non-conflict, add the actual base.
  //  - For every conflict, add arguments for the base[a] of each input
  //   arguments.
  //
  // Note: A simpler form of this would be to add the conflict form of all
  // PHIs without running the optimistic algorithm.  This would be
  // analogous to pessimistic data flow and would likely lead to an
  // overall worse solution.

#ifndef NDEBUG
  auto isExpectedBDVType = [](Value *BDV) {
    return isa<PHINode>(BDV) || isa<SelectInst>(BDV) ||
           isa<ExtractElementInst>(BDV) || isa<InsertElementInst>(BDV) ||
           isa<ShuffleVectorInst>(BDV);
  };
#endif

  // Once populated, will contain a mapping from each potentially non-base BDV
  // to a lattice value (described above) which corresponds to that BDV.
  // We use the order of insertion (DFS over the def/use graph) to provide a
  // stable deterministic ordering for visiting DenseMaps (which are unordered)
  // below.  This is important for deterministic compilation.
  MapVector<Value *, BDVState> States;

  // Recursively fill in all base defining values reachable from the initial
  // one for which we don't already know a definite base value for
  /* scope */ {
    SmallVector<Value*, 16> Worklist;
    Worklist.push_back(Def);
    States.insert({Def, BDVState()});
    while (!Worklist.empty()) {
      Value *Current = Worklist.pop_back_val();
      assert(!isKnownBaseResult(Current) && "why did it get added?");

      auto visitIncomingValue = [&](Value *InVal) {
        Value *Base = findBaseOrBDV(InVal, Cache);
        if (isKnownBaseResult(Base))
          // Known bases won't need new instructions introduced and can be
          // ignored safely
          return;
        assert(isExpectedBDVType(Base) && "the only non-base values "
               "we see should be base defining values");
        if (States.insert(std::make_pair(Base, BDVState())).second)
          Worklist.push_back(Base);
      };
      if (PHINode *PN = dyn_cast<PHINode>(Current)) {
        for (Value *InVal : PN->incoming_values())
          visitIncomingValue(InVal);
      } else if (SelectInst *SI = dyn_cast<SelectInst>(Current)) {
        visitIncomingValue(SI->getTrueValue());
        visitIncomingValue(SI->getFalseValue());
      } else if (auto *EE = dyn_cast<ExtractElementInst>(Current)) {
        visitIncomingValue(EE->getVectorOperand());
      } else if (auto *IE = dyn_cast<InsertElementInst>(Current)) {
        visitIncomingValue(IE->getOperand(0)); // vector operand
        visitIncomingValue(IE->getOperand(1)); // scalar operand
      } else if (auto *SV = dyn_cast<ShuffleVectorInst>(Current)) {
        visitIncomingValue(SV->getOperand(0));
        visitIncomingValue(SV->getOperand(1));
      }
      else {
        llvm_unreachable("Unimplemented instruction case");
      }
    }
  }

#ifndef NDEBUG
  LLVM_DEBUG(dbgs() << "States after initialization:\n");
  for (auto Pair : States) {
    LLVM_DEBUG(dbgs() << " " << Pair.second << " for " << *Pair.first << "\n");
  }
#endif

  // Return a phi state for a base defining value.  We'll generate a new
  // base state for known bases and expect to find a cached state otherwise.
  auto getStateForBDV = [&](Value *baseValue) {
    if (isKnownBaseResult(baseValue))
      return BDVState(baseValue);
    auto I = States.find(baseValue);
    assert(I != States.end() && "lookup failed!");
    return I->second;
  };

  bool Progress = true;
  while (Progress) {
#ifndef NDEBUG
    const size_t OldSize = States.size();
#endif
    Progress = false;
    // We're only changing values in this loop, thus safe to keep iterators.
    // Since this is computing a fixed point, the order of visit does not
    // effect the result.  TODO: We could use a worklist here and make this run
    // much faster.
    for (auto Pair : States) {
      Value *BDV = Pair.first;
      assert(!isKnownBaseResult(BDV) && "why did it get added?");

      // Given an input value for the current instruction, return a BDVState
      // instance which represents the BDV of that value.
      auto getStateForInput = [&](Value *V) mutable {
        Value *BDV = findBaseOrBDV(V, Cache);
        return getStateForBDV(BDV);
      };

      BDVState NewState;
      if (SelectInst *SI = dyn_cast<SelectInst>(BDV)) {
        NewState = meetBDVState(NewState, getStateForInput(SI->getTrueValue()));
        NewState =
            meetBDVState(NewState, getStateForInput(SI->getFalseValue()));
      } else if (PHINode *PN = dyn_cast<PHINode>(BDV)) {
        for (Value *Val : PN->incoming_values())
          NewState = meetBDVState(NewState, getStateForInput(Val));
      } else if (auto *EE = dyn_cast<ExtractElementInst>(BDV)) {
        // The 'meet' for an extractelement is slightly trivial, but it's still
        // useful in that it drives us to conflict if our input is.
        NewState =
            meetBDVState(NewState, getStateForInput(EE->getVectorOperand()));
      } else if (auto *IE = dyn_cast<InsertElementInst>(BDV)){
        // Given there's a inherent type mismatch between the operands, will
        // *always* produce Conflict.
        NewState = meetBDVState(NewState, getStateForInput(IE->getOperand(0)));
        NewState = meetBDVState(NewState, getStateForInput(IE->getOperand(1)));
      } else {
        // The only instance this does not return a Conflict is when both the
        // vector operands are the same vector.
        auto *SV = cast<ShuffleVectorInst>(BDV);
        NewState = meetBDVState(NewState, getStateForInput(SV->getOperand(0)));
        NewState = meetBDVState(NewState, getStateForInput(SV->getOperand(1)));
      }

      BDVState OldState = States[BDV];
      if (OldState != NewState) {
        Progress = true;
        States[BDV] = NewState;
      }
    }

    assert(OldSize == States.size() &&
           "fixed point shouldn't be adding any new nodes to state");
  }

#ifndef NDEBUG
  LLVM_DEBUG(dbgs() << "States after meet iteration:\n");
  for (auto Pair : States) {
    LLVM_DEBUG(dbgs() << " " << Pair.second << " for " << *Pair.first << "\n");
  }
#endif

  // Insert Phis for all conflicts
  // TODO: adjust naming patterns to avoid this order of iteration dependency
  for (auto Pair : States) {
    Instruction *I = cast<Instruction>(Pair.first);
    BDVState State = Pair.second;
    assert(!isKnownBaseResult(I) && "why did it get added?");
    assert(!State.isUnknown() && "Optimistic algorithm didn't complete!");

    // extractelement instructions are a bit special in that we may need to
    // insert an extract even when we know an exact base for the instruction.
    // The problem is that we need to convert from a vector base to a scalar
    // base for the particular indice we're interested in.
    if (State.isBase() && isa<ExtractElementInst>(I) &&
        isa<VectorType>(State.getBaseValue()->getType())) {
      auto *EE = cast<ExtractElementInst>(I);
      // TODO: In many cases, the new instruction is just EE itself.  We should
      // exploit this, but can't do it here since it would break the invariant
      // about the BDV not being known to be a base.
      auto *BaseInst = ExtractElementInst::Create(
          State.getBaseValue(), EE->getIndexOperand(), "base_ee", EE);
      BaseInst->setMetadata("is_base_value", MDNode::get(I->getContext(), {}));
      States[I] = BDVState(BDVState::Base, BaseInst);
    }

    // Since we're joining a vector and scalar base, they can never be the
    // same.  As a result, we should always see insert element having reached
    // the conflict state.
    assert(!isa<InsertElementInst>(I) || State.isConflict());

    if (!State.isConflict())
      continue;

    /// Create and insert a new instruction which will represent the base of
    /// the given instruction 'I'.
    auto MakeBaseInstPlaceholder = [](Instruction *I) -> Instruction* {
      if (isa<PHINode>(I)) {
        BasicBlock *BB = I->getParent();
        int NumPreds = pred_size(BB);
        assert(NumPreds > 0 && "how did we reach here");
        std::string Name = suffixed_name_or(I, ".base", "base_phi");
        return PHINode::Create(I->getType(), NumPreds, Name, I);
      } else if (SelectInst *SI = dyn_cast<SelectInst>(I)) {
        // The undef will be replaced later
        UndefValue *Undef = UndefValue::get(SI->getType());
        std::string Name = suffixed_name_or(I, ".base", "base_select");
        return SelectInst::Create(SI->getCondition(), Undef, Undef, Name, SI);
      } else if (auto *EE = dyn_cast<ExtractElementInst>(I)) {
        UndefValue *Undef = UndefValue::get(EE->getVectorOperand()->getType());
        std::string Name = suffixed_name_or(I, ".base", "base_ee");
        return ExtractElementInst::Create(Undef, EE->getIndexOperand(), Name,
                                          EE);
      } else if (auto *IE = dyn_cast<InsertElementInst>(I)) {
        UndefValue *VecUndef = UndefValue::get(IE->getOperand(0)->getType());
        UndefValue *ScalarUndef = UndefValue::get(IE->getOperand(1)->getType());
        std::string Name = suffixed_name_or(I, ".base", "base_ie");
        return InsertElementInst::Create(VecUndef, ScalarUndef,
                                         IE->getOperand(2), Name, IE);
      } else {
        auto *SV = cast<ShuffleVectorInst>(I);
        UndefValue *VecUndef = UndefValue::get(SV->getOperand(0)->getType());
        std::string Name = suffixed_name_or(I, ".base", "base_sv");
        return new ShuffleVectorInst(VecUndef, VecUndef, SV->getOperand(2),
                                     Name, SV);
      }
    };
    Instruction *BaseInst = MakeBaseInstPlaceholder(I);
    // Add metadata marking this as a base value
    BaseInst->setMetadata("is_base_value", MDNode::get(I->getContext(), {}));
    States[I] = BDVState(BDVState::Conflict, BaseInst);
  }

  // Returns a instruction which produces the base pointer for a given
  // instruction.  The instruction is assumed to be an input to one of the BDVs
  // seen in the inference algorithm above.  As such, we must either already
  // know it's base defining value is a base, or have inserted a new
  // instruction to propagate the base of it's BDV and have entered that newly
  // introduced instruction into the state table.  In either case, we are
  // assured to be able to determine an instruction which produces it's base
  // pointer.
  auto getBaseForInput = [&](Value *Input, Instruction *InsertPt) {
    Value *BDV = findBaseOrBDV(Input, Cache);
    Value *Base = nullptr;
    if (isKnownBaseResult(BDV)) {
      Base = BDV;
    } else {
      // Either conflict or base.
      assert(States.count(BDV));
      Base = States[BDV].getBaseValue();
    }
    assert(Base && "Can't be null");
    // The cast is needed since base traversal may strip away bitcasts
    if (Base->getType() != Input->getType() && InsertPt)
      Base = CastInst::CreatePointerBitCastOrAddrSpaceCast(Base, Input->getType(), "cast", InsertPt);
    return Base;
  };

  // Fixup all the inputs of the new PHIs.  Visit order needs to be
  // deterministic and predictable because we're naming newly created
  // instructions.
  for (auto Pair : States) {
    Instruction *BDV = cast<Instruction>(Pair.first);
    BDVState State = Pair.second;

    assert(!isKnownBaseResult(BDV) && "why did it get added?");
    assert(!State.isUnknown() && "Optimistic algorithm didn't complete!");
    if (!State.isConflict())
      continue;

    if (PHINode *BasePHI = dyn_cast<PHINode>(State.getBaseValue())) {
      PHINode *PN = cast<PHINode>(BDV);
      unsigned NumPHIValues = PN->getNumIncomingValues();
      for (unsigned i = 0; i < NumPHIValues; i++) {
        Value *InVal = PN->getIncomingValue(i);
        BasicBlock *InBB = PN->getIncomingBlock(i);

        // If we've already seen InBB, add the same incoming value
        // we added for it earlier.  The IR verifier requires phi
        // nodes with multiple entries from the same basic block
        // to have the same incoming value for each of those
        // entries.  If we don't do this check here and basephi
        // has a different type than base, we'll end up adding two
        // bitcasts (and hence two distinct values) as incoming
        // values for the same basic block.

        int BlockIndex = BasePHI->getBasicBlockIndex(InBB);
        if (BlockIndex != -1) {
          Value *OldBase = BasePHI->getIncomingValue(BlockIndex);
          BasePHI->addIncoming(OldBase, InBB);

#ifndef NDEBUG
          Value *Base = getBaseForInput(InVal, nullptr);
          // In essence this assert states: the only way two values
          // incoming from the same basic block may be different is by
          // being different bitcasts of the same value.  A cleanup
          // that remains TODO is changing findBaseOrBDV to return an
          // llvm::Value of the correct type (and still remain pure).
          // This will remove the need to add bitcasts.
          assert(Base->stripPointerCasts() == OldBase->stripPointerCasts() &&
                 "Sanity -- findBaseOrBDV should be pure!");
#endif
          continue;
        }

        // Find the instruction which produces the base for each input.  We may
        // need to insert a bitcast in the incoming block.
        // TODO: Need to split critical edges if insertion is needed
        Value *Base = getBaseForInput(InVal, InBB->getTerminator());
        BasePHI->addIncoming(Base, InBB);
      }
      assert(BasePHI->getNumIncomingValues() == NumPHIValues);
    } else if (SelectInst *BaseSI =
                   dyn_cast<SelectInst>(State.getBaseValue())) {
      SelectInst *SI = cast<SelectInst>(BDV);

      // Find the instruction which produces the base for each input.
      // We may need to insert a bitcast.
      BaseSI->setTrueValue(getBaseForInput(SI->getTrueValue(), BaseSI));
      BaseSI->setFalseValue(getBaseForInput(SI->getFalseValue(), BaseSI));
    } else if (auto *BaseEE =
                   dyn_cast<ExtractElementInst>(State.getBaseValue())) {
      Value *InVal = cast<ExtractElementInst>(BDV)->getVectorOperand();
      // Find the instruction which produces the base for each input.  We may
      // need to insert a bitcast.
      BaseEE->setOperand(0, getBaseForInput(InVal, BaseEE));
    } else if (auto *BaseIE = dyn_cast<InsertElementInst>(State.getBaseValue())){
      auto *BdvIE = cast<InsertElementInst>(BDV);
      auto UpdateOperand = [&](int OperandIdx) {
        Value *InVal = BdvIE->getOperand(OperandIdx);
        Value *Base = getBaseForInput(InVal, BaseIE);
        BaseIE->setOperand(OperandIdx, Base);
      };
      UpdateOperand(0); // vector operand
      UpdateOperand(1); // scalar operand
    } else {
      auto *BaseSV = cast<ShuffleVectorInst>(State.getBaseValue());
      auto *BdvSV = cast<ShuffleVectorInst>(BDV);
      auto UpdateOperand = [&](int OperandIdx) {
        Value *InVal = BdvSV->getOperand(OperandIdx);
        Value *Base = getBaseForInput(InVal, BaseSV);
        BaseSV->setOperand(OperandIdx, Base);
      };
      UpdateOperand(0); // vector operand
      UpdateOperand(1); // vector operand
    }
  }

  // Cache all of our results so we can cheaply reuse them
  // NOTE: This is actually two caches: one of the base defining value
  // relation and one of the base pointer relation!  FIXME
  for (auto Pair : States) {
    auto *BDV = Pair.first;
    Value *Base = Pair.second.getBaseValue();
    assert(BDV && Base);
    assert(!isKnownBaseResult(BDV) && "why did it get added?");

    LLVM_DEBUG(
        dbgs() << "Updating base value cache"
               << " for: " << BDV->getName() << " from: "
               << (Cache.count(BDV) ? Cache[BDV]->getName().str() : "none")
               << " to: " << Base->getName() << "\n");

    if (Cache.count(BDV)) {
      assert(isKnownBaseResult(Base) &&
             "must be something we 'know' is a base pointer");
      // Once we transition from the BDV relation being store in the Cache to
      // the base relation being stored, it must be stable
      assert((!isKnownBaseResult(Cache[BDV]) || Cache[BDV] == Base) &&
             "base relation should be stable");
    }
    Cache[BDV] = Base;
  }
  assert(Cache.count(Def));
  return Cache[Def];
}

// For a set of live pointers (base and/or derived), identify the base
// pointer of the object which they are derived from.  This routine will
// mutate the IR graph as needed to make the 'base' pointer live at the
// definition site of 'derived'.  This ensures that any use of 'derived' can
// also use 'base'.  This may involve the insertion of a number of
// additional PHI nodes.
//
// preconditions: live is a set of pointer type Values
//
// side effects: may insert PHI nodes into the existing CFG, will preserve
// CFG, will not remove or mutate any existing nodes
//
// post condition: PointerToBase contains one (derived, base) pair for every
// pointer in live.  Note that derived can be equal to base if the original
// pointer was a base pointer.
static void
findBasePointers(const StatepointLiveSetTy &live,
                 MapVector<Value *, Value *> &PointerToBase,
                 DominatorTree *DT, DefiningValueMapTy &DVCache) {
  for (Value *ptr : live) {
    Value *base = findBasePointer(ptr, DVCache);
    assert(base && "failed to find base pointer");
    PointerToBase[ptr] = base;
    assert((!isa<Instruction>(base) || !isa<Instruction>(ptr) ||
            DT->dominates(cast<Instruction>(base)->getParent(),
                          cast<Instruction>(ptr)->getParent())) &&
           "The base we found better dominate the derived pointer");
  }
}

/// Find the required based pointers (and adjust the live set) for the given
/// parse point.
static void findBasePointers(DominatorTree &DT, DefiningValueMapTy &DVCache,
                             CallBase *Call,
                             PartiallyConstructedSafepointRecord &result) {
  MapVector<Value *, Value *> PointerToBase;
  findBasePointers(result.LiveSet, PointerToBase, &DT, DVCache);

  result.PointerToBase = PointerToBase;
}

// When inserting gc.relocate and gc.result calls, we need to ensure there are
// no uses of the original value / return value between the gc.statepoint and
// the gc.relocate / gc.result call.  One case which can arise is a phi node
// starting one of the successor blocks.  We also need to be able to insert the
// gc.relocates only on the path which goes through the statepoint.  We might
// need to split an edge to make this possible.
static BasicBlock *
normalizeForInvokeSafepoint(BasicBlock *BB, BasicBlock *InvokeParent,
                            DominatorTree &DT) {
  BasicBlock *Ret = BB;
  if (!BB->getUniquePredecessor())
    Ret = SplitBlockPredecessors(BB, InvokeParent, "", &DT);

  // Now that 'Ret' has unique predecessor we can safely remove all phi nodes
  // from it
  FoldSingleEntryPHINodes(Ret);
  assert(!isa<PHINode>(Ret->begin()) &&
         "All PHI nodes should have been removed!");

  // At this point, we can safely insert a gc.relocate or gc.result as the first
  // instruction in Ret if needed.
  return Ret;
}

// Create new attribute set containing only attributes which can be transferred
// from original call to the safepoint.
static AttributeList legalizeCallAttributes(AttributeList AL) {
  if (AL.isEmpty())
    return AL;

  // Remove the readonly, readnone, and statepoint function attributes.
  AttrBuilder FnAttrs = AL.getFnAttributes();
  FnAttrs.removeAttribute(Attribute::ReadNone);
  FnAttrs.removeAttribute(Attribute::ReadOnly);
  for (Attribute A : AL.getFnAttributes()) {
    if (isStatepointDirectiveAttr(A))
      FnAttrs.remove(A);
  }

  LLVMContext &Ctx = AL.getContext();
  AttributeList Ret = AttributeList::get(Ctx, AttributeList::FunctionIndex,
                                         AttributeSet::get(Ctx, FnAttrs));

  // Add parameter attrs.
  // TODO: use AttrBuilder?
  for (unsigned i = AttributeList::FirstArgIndex, e = AL.getNumAttrSets(); i < e; ++i)
    Ret = Ret.addAttributes(Ctx, i+5,  AL.getAttributes(i));

  // Skip return attributes
  return Ret;
}

namespace {

/// This struct is used to defer RAUWs and `eraseFromParent` s.  Using this
/// avoids having to worry about keeping around dangling pointers to Values.
class DeferredReplacement {
  AssertingVH<Instruction> Old;
  AssertingVH<Instruction> New;
  bool IsDeoptimize = false;

  DeferredReplacement() = default;

public:
  static DeferredReplacement createRAUW(Instruction *Old, Instruction *New) {
    assert(Old != New && Old && New &&
           "Cannot RAUW equal values or to / from null!");

    DeferredReplacement D;
    D.Old = Old;
    D.New = New;
    return D;
  }

  static DeferredReplacement createDelete(Instruction *ToErase) {
    DeferredReplacement D;
    D.Old = ToErase;
    return D;
  }

  static DeferredReplacement createDeoptimizeReplacement(Instruction *Old) {
#ifndef NDEBUG
    auto *F = cast<CallInst>(Old)->getCalledFunction();
    assert(F && F->getIntrinsicID() == Intrinsic::experimental_deoptimize &&
           "Only way to construct a deoptimize deferred replacement");
#endif
    DeferredReplacement D;
    D.Old = Old;
    D.IsDeoptimize = true;
    return D;
  }

  /// Does the task represented by this instance.
  void doReplacement() {
    Instruction *OldI = Old;
    Instruction *NewI = New;

    assert(OldI != NewI && "Disallowed at construction?!");
    assert((!IsDeoptimize || !New) &&
           "Deoptimize intrinsics are not replaced!");

    Old = nullptr;
    New = nullptr;

    if (NewI)
      OldI->replaceAllUsesWith(NewI);

    if (IsDeoptimize) {
      // Note: we've inserted instructions, so the call to llvm.deoptimize may
      // not necessarily be followed by the matching return.
      auto *RI = cast<ReturnInst>(OldI->getParent()->getTerminator());
      new UnreachableInst(RI->getContext(), RI);
      RI->eraseFromParent();
    }

    OldI->eraseFromParent();
  }
};

} // end anonymous namespace

/// A unique function which doesn't require we sort the input vector.
template <typename T> static void unique_unsorted(SmallVectorImpl<T> &Vec) {
  SmallSet<T, 8> Seen;
  Vec.erase(remove_if(Vec, [&](const T &V) { return !Seen.insert(V).second; }),
            Vec.end());
}

// Attach the stack map to the statepoint. The statepoint is an invoke
// with the given landing pad. The stack map (pointer) is attached as
// the type info of the landing pad.
static void
attachStackMap(uint64_t StatepointID, Instruction *LandingPad) {
  if (cast<LandingPadInst>(LandingPad)->isCleanup())
    return;
  Module *M = LandingPad->getModule();
  std::string Name = (Twine(GO_STACKMAP_SYM_PREFIX) + Twine(StatepointID)).str();
  Constant *C = M->getOrInsertGlobal(Name, Type::getInt64Ty(M->getContext()));
  LandingPad->setOperand(0, C);
}

// Extract pointer fields from an FCA.
static void
extractPointerFromFCA(Value *V, IRBuilder<> &Builder,
                      SmallVectorImpl<Value *> &PtrFields) {
  Type *T = V->getType();
  assert(T->isStructTy());
  for (unsigned i = 0, e = T->getStructNumElements(); i < e; ++i) {
    Type *ElemT = T->getStructElementType(i);
    if (ElemT->isPointerTy()) {
      Value *Field = Builder.CreateExtractValue(V, {i});
      PtrFields.push_back(Field);
    } else
     assert(!hasPointer(ElemT) && "nested FCA is not supported");
  }
}

static Value *phiHasConstantValue(PHINode *Phi0);

static void
makeStatepointExplicitImpl(CallBase *Call, /* to replace */
                           SmallVectorImpl<Value *> &BasePtrs,
                           SmallVectorImpl<Value *> &LiveVariables,
                           PartiallyConstructedSafepointRecord &Result,
                           std::vector<DeferredReplacement> &Replacements) {
  assert(BasePtrs.size() == LiveVariables.size());

  // Then go ahead and use the builder do actually do the inserts.  We insert
  // immediately before the previous instruction under the assumption that all
  // arguments will be available here.  We can't insert afterwards since we may
  // be replacing a terminator.
  const DataLayout &DL = Call->getModule()->getDataLayout();
  IRBuilder<> Builder(Call);

  unique_unsorted(BasePtrs);

  // For aggregate typed stack slots, attach a bitmap identifying its
  // pointer fields.
  SmallVector<Value *, 64> PtrFields;
  for (Value *V : BasePtrs) {
    if (auto *Phi = dyn_cast<PHINode>(V))
      assert(!phiHasConstantValue(Phi) && "constant phi should not be in liveset");
    if (isa<AllocaInst>(V) ||
        (isa<Argument>(V) && cast<Argument>(V)->hasByValAttr())) {
      // Byval argument is at a fixed frame offset. Treat it the same as alloca.
      Type *T = cast<PointerType>(V->getType())->getElementType();
      if (hasPointer(T)) {
        PtrFields.push_back(V);
        getPtrBitmapForType(T, DL, PtrFields);
      }
    } else if (V->getType()->isStructTy()) {
      // Statepoint lowering doesn't handle FCA. So we do it ourselves by
      // extracting all the pointer fields and letting the statepoint lowering
      // spill them.
      extractPointerFromFCA(V, Builder, PtrFields);
    } else
      PtrFields.push_back(V);
  }

  ArrayRef<Value *> GCArgs(PtrFields);
  uint64_t StatepointID = ID;
  ID++;
  uint32_t NumPatchBytes = 0;
  uint32_t Flags = uint32_t(StatepointFlags::None);

  ArrayRef<Use> CallArgs(Call->arg_begin(), Call->arg_end());
  ArrayRef<Use> TransitionArgs;
  if (auto TransitionBundle =
      Call->getOperandBundle(LLVMContext::OB_gc_transition)) {
    Flags |= uint32_t(StatepointFlags::GCTransition);
    TransitionArgs = TransitionBundle->Inputs;
  }

  // Instead of lowering calls to @llvm.experimental.deoptimize as normal calls
  // with a return value, we lower then as never returning calls to
  // __llvm_deoptimize that are followed by unreachable to get better codegen.
  bool IsDeoptimize = false;

  StatepointDirectives SD =
      parseStatepointDirectivesFromAttrs(Call->getAttributes());
  if (SD.NumPatchBytes)
    NumPatchBytes = *SD.NumPatchBytes;
  if (SD.StatepointID)
    StatepointID = *SD.StatepointID;

  Value *CallTarget = Call->getCalledValue();
  if (Function *F = dyn_cast<Function>(CallTarget)) {
    if (F->getIntrinsicID() == Intrinsic::experimental_deoptimize) {
      // Calls to llvm.experimental.deoptimize are lowered to calls to the
      // __llvm_deoptimize symbol.  We want to resolve this now, since the
      // verifier does not allow taking the address of an intrinsic function.

      SmallVector<Type *, 8> DomainTy;
      for (Value *Arg : CallArgs)
        DomainTy.push_back(Arg->getType());
      auto *FTy = FunctionType::get(Type::getVoidTy(F->getContext()), DomainTy,
                                    /* isVarArg = */ false);

      // Note: CallTarget can be a bitcast instruction of a symbol if there are
      // calls to @llvm.experimental.deoptimize with different argument types in
      // the same module.  This is fine -- we assume the frontend knew what it
      // was doing when generating this kind of IR.
      CallTarget = F->getParent()
          ->getOrInsertFunction("__llvm_deoptimize", FTy).getCallee();

      IsDeoptimize = true;
    }
  }

  // Create the statepoint given all the arguments
  Instruction *Token = nullptr;
  if (isa<CallInst>(Call)) {
    // We should have converted all statepoints to invoke.
    assert(false && "statepoint is not an invoke");
  } else {
    InvokeInst *ToReplace = cast<InvokeInst>(Call);

    // Insert the new invoke into the old block.  We'll remove the old one in a
    // moment at which point this will become the new terminator for the
    // original block.

    // Note (Go specific):
    // Here we attach GCArgs actually to the "deopt arg" slots, instead of
    // the "gc arg" slots, of the statepoint. Both are recorded in the stack
    // map the same way. The difference is that "deopt arg" doesn't need
    // relocation. We're implementing non-moving GC (for now).
    InvokeInst *Invoke = Builder.CreateGCStatepointInvoke(
        StatepointID, NumPatchBytes, CallTarget, ToReplace->getNormalDest(),
        ToReplace->getUnwindDest(), CallArgs, GCArgs, ArrayRef<Value*>(),
        "statepoint_token");

    Invoke->setCallingConv(ToReplace->getCallingConv());

    // Currently we will fail on parameter attributes and on certain
    // function attributes.  In case if we can handle this set of attributes -
    // set up function attrs directly on statepoint and return attrs later for
    // gc_result intrinsic.
    Invoke->setAttributes(legalizeCallAttributes(ToReplace->getAttributes()));

    Token = Invoke;

    // Generate gc relocates in exceptional path
    BasicBlock *UnwindBlock = ToReplace->getUnwindDest();
    assert(!isa<PHINode>(UnwindBlock->begin()) &&
           UnwindBlock->getUniquePredecessor() &&
           "can't safely insert in this block!");

    Builder.SetInsertPoint(&*UnwindBlock->getFirstInsertionPt());
    Builder.SetCurrentDebugLocation(ToReplace->getDebugLoc());

    // Attach exceptional gc relocates to the landingpad.
    Instruction *ExceptionalToken = UnwindBlock->getLandingPadInst();
    Result.UnwindToken = ExceptionalToken;

    attachStackMap(StatepointID, ExceptionalToken);

    BasicBlock *NormalDest = ToReplace->getNormalDest();
    assert(!isa<PHINode>(NormalDest->begin()) &&
           NormalDest->getUniquePredecessor() &&
           "can't safely insert in this block!");

    Builder.SetInsertPoint(&*NormalDest->getFirstInsertionPt());
  }
  assert(Token && "Should be set in one of the above branches!");

  if (IsDeoptimize) {
    // If we're wrapping an @llvm.experimental.deoptimize in a statepoint, we
    // transform the tail-call like structure to a call to a void function
    // followed by unreachable to get better codegen.
    Replacements.push_back(
        DeferredReplacement::createDeoptimizeReplacement(Call));
  } else {
    Token->setName("statepoint_token");
    if (!Call->getType()->isVoidTy() && !Call->use_empty()) {
      StringRef Name =
          Call->hasName() ? Call->getName() : "";
      CallInst *GCResult = Builder.CreateGCResult(Token, Call->getType(), Name);
      GCResult->setAttributes(
          AttributeList::get(GCResult->getContext(), AttributeList::ReturnIndex,
                             Call->getAttributes().getRetAttributes()));

      // We cannot RAUW or delete CS.getInstruction() because it could be in the
      // live set of some other safepoint, in which case that safepoint's
      // PartiallyConstructedSafepointRecord will hold a raw pointer to this
      // llvm::Instruction.  Instead, we defer the replacement and deletion to
      // after the live sets have been made explicit in the IR, and we no longer
      // have raw pointers to worry about.
      Replacements.emplace_back(
          DeferredReplacement::createRAUW(Call, GCResult));
    } else {
      Replacements.emplace_back(
          DeferredReplacement::createDelete(Call));
    }
  }

  Result.StatepointToken = Token;
}

// Replace an existing gc.statepoint with a new one and a set of gc.relocates
// which make the relocations happening at this safepoint explicit.
//
// WARNING: Does not do any fixup to adjust users of the original live
// values.  That's the callers responsibility.
static void
makeStatepointExplicit(DominatorTree &DT, CallBase *Call,
                       PartiallyConstructedSafepointRecord &Result,
                       std::vector<DeferredReplacement> &Replacements) {
  const auto &LiveSet = Result.LiveSet;
  const auto &PointerToBase = Result.PointerToBase;

  // Convert to vector for efficient cross referencing.
  SmallVector<Value *, 64> BaseVec, LiveVec;
  LiveVec.reserve(LiveSet.size());
  BaseVec.reserve(LiveSet.size());
  for (Value *L : LiveSet) {
    LiveVec.push_back(L);
    assert(PointerToBase.count(L));
    Value *Base = PointerToBase.find(L)->second;
    BaseVec.push_back(Base);
  }
  assert(LiveVec.size() == BaseVec.size());

  // Do the actual rewriting and delete the old statepoint
  makeStatepointExplicitImpl(Call, BaseVec, LiveVec, Result, Replacements);
}

static void findLiveReferences(
    Function &F, DominatorTree &DT, ArrayRef<CallBase *> toUpdate,
    MutableArrayRef<struct PartiallyConstructedSafepointRecord> records,
    SetVector<Value *> &AddrTakenAllocas,
    SetVector<Value *> &ToZero,
    SetVector<Value *> &BadLoads,
    DefiningValueMapTy &DVCache) {
  GCPtrLivenessData OriginalLivenessData;

  // Find all allocas.
  SetVector<Value *> AllAllocas;
  if (ClobberNonLive)
    for (Instruction &I : F.getEntryBlock())
      if (isa<AllocaInst>(I) && hasPointer(I.getType()->getPointerElementType()))
        AllAllocas.insert(&I);

  computeLiveInValues(DT, F, OriginalLivenessData, AddrTakenAllocas,
                      ToZero, BadLoads, DVCache);
  for (size_t i = 0; i < records.size(); i++) {
    struct PartiallyConstructedSafepointRecord &info = records[i];
    analyzeParsePointLiveness(DT, OriginalLivenessData, AddrTakenAllocas,
                              toUpdate[i], info, AllAllocas, DVCache);
  }
}

// A helper function that reports whether V is a Phi that contains an
// ambiguously live alloca as input.
static bool
phiContainsAlloca(Value *V, SetVector<Value *> &ToZero,
                  SetVector<Value *> &AddrTakenAllocas) {
  PHINode *Phi0 = dyn_cast<PHINode>(V);
  if (!Phi0)
    return false;

  // Visit all the Phi inputs. Discover new Phis on the go, and visit them.
  SmallSet<PHINode *, 4> Phis, Visited;
  Phis.insert(Phi0);
  while (!Phis.empty()) {
    PHINode *Phi = *Phis.begin();
    Visited.insert(Phi);
    for (Value *Operand : Phi->incoming_values()) {
      if (PHINode *P = dyn_cast<PHINode>(Operand)) {
        if (!Visited.count(P))
          Phis.insert(P);
        continue;
      }
      Value *Base = Operand->stripPointerCasts();
      if (ToZero.count(Base) != 0 && AddrTakenAllocas.count(Base) != 0)
        return true;
    }
    Phis.erase(Phi);
  }
  return false;
}

// Zero ambigously lived stack slots. We insert zeroing at lifetime
// start (or the entry block), so the GC won't see uninitialized
// content. We also insert zeroing at kill sites, to ensure the GC
// won't see a dead slot come back to life.
// We also conservatively extend the lifetime of address-taken slots,
// to prevent the slot being reused while it is still recorded live.
static void
zeroAmbiguouslyLiveSlots(Function &F, SetVector<Value *> &ToZero,
                         SetVector<Value *> &AddrTakenAllocas) {
  SmallVector<Instruction *, 16> InstToDelete;
  SetVector<Value *> Done;
  const DataLayout &DL = F.getParent()->getDataLayout();
  IntegerType *Int64Ty = Type::getInt64Ty(F.getParent()->getContext());
  IntegerType *Int8Ty = Type::getInt8Ty(F.getParent()->getContext());

  // If a slot has lifetime.start, place the zeroing right after it.
  for (Instruction &I : instructions(F)) {
    if (CallInst *CI = dyn_cast<CallInst>(&I))
      if (Function *Fn = CI->getCalledFunction()) {
        if (Fn->getIntrinsicID() == Intrinsic::lifetime_start) {
          Value *V = I.getOperand(1)->stripPointerCasts();
          if ((ToZero.count(V) != 0 && AddrTakenAllocas.count(V) != 0) ||
              phiContainsAlloca(V, ToZero, AddrTakenAllocas)) {
            // For addrtaken alloca, the lifetime start may not dominate all
            // safepoints where the slot can be live.
            // For now, zero it in the entry block and remove the lifetime
            // marker.
            // Also remove lifetime markers on Phis that contain interesting
            // allocas (which must be address-taken).
            InstToDelete.push_back(&I);
          } else if (ToZero.count(V) != 0) {
            // Non-addrtaken alloca. Just insert zeroing, keep the lifetime marker.
            IRBuilder<> Builder(I.getNextNode());
            Value *Zero = Constant::getNullValue(V->getType()->getPointerElementType());
            Builder.CreateStore(Zero, V);
            // Don't remove V from ToZero for now, as there may be multiple
            // lifetime start markers, where we need to insert zeroing.
            Done.insert(V);
          }
        } else if (Fn->getIntrinsicID() == Intrinsic::lifetime_end) {
          Value *V = I.getOperand(1)->stripPointerCasts();
          if ((ToZero.count(V) != 0 && AddrTakenAllocas.count(V) != 0) ||
              phiContainsAlloca(V, ToZero, AddrTakenAllocas)) {
            if (!succ_empty(I.getParent())) { // no need to zero at exit block
              // What to zero in the Phi case?
              // We just zero whatever the Phi points to, using the size on the
              // lifetime marker. This also works in the alloca case.
              IRBuilder<> Builder(&I);
              Value *Zero = Constant::getNullValue(Int8Ty);
              Value *Siz = I.getOperand(0);
              Builder.CreateMemSet(V, Zero, Siz, 0);
            }
            InstToDelete.push_back(&I);
          }
        }
      }
  }

  for (Instruction *I : InstToDelete)
    I->eraseFromParent();
  ToZero.set_subtract(Done);
  if (ToZero.empty())
    return;

  // Otherwise, place the zeroing in the entry block after the alloca.
  for (Instruction &I : F.getEntryBlock())
    if (ToZero.count(&I) != 0) {
      IRBuilder<> Builder(I.getNextNode());
      Type *ElemTyp = I.getType()->getPointerElementType();
      if (AddrTakenAllocas.count(&I) != 0) {
        // For addrtaken alloca, we removed the lifetime marker above.
        // Insert a new one at the entry block.
        unsigned Size = DL.getTypeStoreSize(ElemTyp);
        Builder.CreateLifetimeStart(&I, ConstantInt::get(Int64Ty, Size));
      }
      Value *Zero = Constant::getNullValue(ElemTyp);
      Builder.CreateStore(Zero, &I);
      ToZero.remove(&I);
    }

  assert(ToZero.empty());
}

// Detect degenerate Phi.
// Try harder to handle mutually dependent case, like
//   a = phi(null, b)
// (in a different block)
//   b = phi(a, null)
static Value *
phiHasConstantValue(PHINode *Phi0) {
  Value *V = Phi0->hasConstantValue();
  if (V && isa<Constant>(V))
    return V;

  V = nullptr;

  // Visit all the Phi inputs. Discover new Phis on the go, and visit them.
  // Early exit if we see a non-constant or two different constants.
  SmallSet<PHINode *, 4> Phis, Visited;
  Phis.insert(Phi0);
  while (!Phis.empty()) {
    PHINode *Phi = *Phis.begin();
    Visited.insert(Phi);
    for (Value *Operand : Phi->incoming_values()) {
      if (PHINode *P = dyn_cast<PHINode>(Operand)) {
        if (!Visited.count(P))
          Phis.insert(P);
        continue;
      }
      if (isa<Constant>(Operand)) {
        if (V && V != Operand)
          return nullptr; // operands not same
        V = Operand;
      } else
        return nullptr; // has non-constant input
    }
    Phis.erase(Phi);
  }

  return V;
}

static void
fixBadPhiOperands(Function &F, SetVector<Value *> &BadLoads) {
  // Don't delete instructions yet -- they may be referenced in the liveness
  // map. We'll delete them at the end.

  for (auto *I : BadLoads)
    I->replaceAllUsesWith(Constant::getNullValue(I->getType()));

  // The replacement above may lead to degenerate Phis, which, if live,
  // will be encoded as constants in the stack map, which is bad
  // (confusing with pointer bitmaps). Clean them up.
  bool Changed = true;
  while (Changed) {
    Changed = false;
    for (Instruction &I : instructions(F))
      if (auto *Phi = dyn_cast<PHINode>(&I))
        if (!BadLoads.count(Phi))
          if (Value *V = phiHasConstantValue(Phi)) {
            Phi->replaceAllUsesWith(V);
            BadLoads.insert(Phi);
            Changed = true;
          }
  }
}

static void fixStackWriteBarriers(Function &F, DefiningValueMapTy &DVCache);

static bool insertParsePoints(Function &F, DominatorTree &DT,
                              TargetTransformInfo &TTI,
                              SmallVectorImpl<CallBase *> &ToUpdate) {
#ifndef NDEBUG
  // sanity check the input
  std::set<CallBase *> Uniqued;
  Uniqued.insert(ToUpdate.begin(), ToUpdate.end());
  assert(Uniqued.size() == ToUpdate.size() && "no duplicates please!");

  for (CallBase *Call : ToUpdate)
    assert(Call->getFunction() == &F);
#endif

  // When inserting gc.relocates for invokes, we need to be able to insert at
  // the top of the successor blocks.  See the comment on
  // normalForInvokeSafepoint on exactly what is needed.  Note that this step
  // may restructure the CFG.
  for (CallBase *Call : ToUpdate) {
    auto *II = dyn_cast<InvokeInst>(Call);
    if (!II)
      continue;
    normalizeForInvokeSafepoint(II->getNormalDest(), II->getParent(), DT);
    normalizeForInvokeSafepoint(II->getUnwindDest(), II->getParent(), DT);
  }

  SmallVector<PartiallyConstructedSafepointRecord, 64> Records(ToUpdate.size());

  SetVector<Value *> AddrTakenAllocas, ToZero;

  // In some rare cases, the optimizer may generate a load
  // from an uninitialized slot. I saw this happens with LICM's
  // load promotion. A load is moved out of the loop, and its
  // only used in Phis. In the actual execution when the value
  // is used, the Phi is always holding other incoming values,
  // which are valid. The problem is that the load or the Phi
  // may be recorded live, and the stack scan may heppen before
  // it holding valid data, which is bad. We'll record those bad
  // loads and remove them from the live sets.
  SetVector<Value *> BadLoads;

  // Cache the 'defining value' relation used in the computation and
  // insertion of base phis and selects.  This ensures that we don't insert
  // large numbers of duplicate base_phis.
  DefiningValueMapTy DVCache;

  fixStackWriteBarriers(F, DVCache);

  // A) Identify all gc pointers which are statically live at the given call
  // site.
  findLiveReferences(F, DT, ToUpdate, Records, AddrTakenAllocas, ToZero,
                     BadLoads, DVCache);

  // B) Find the base pointers for each live pointer
  for (size_t i = 0; i < Records.size(); i++) {
    PartiallyConstructedSafepointRecord &info = Records[i];
    findBasePointers(DT, DVCache, ToUpdate[i], info);
  }

  fixBadPhiOperands(F, BadLoads);

  // It is possible that non-constant live variables have a constant base.  For
  // example, a GEP with a variable offset from a global.  In this case we can
  // remove it from the liveset.  We already don't add constants to the liveset
  // because we assume they won't move at runtime and the GC doesn't need to be
  // informed about them.  The same reasoning applies if the base is constant.
  // Note that the relocation placement code relies on this filtering for
  // correctness as it expects the base to be in the liveset, which isn't true
  // if the base is constant.
  // Also make sure bad loads are not included in the liveset.
  for (auto &Info : Records)
    for (auto &BasePair : Info.PointerToBase)
      if (isa<Constant>(BasePair.second) || BadLoads.count(BasePair.second))
        Info.LiveSet.remove(BasePair.first);

  // We need this to safely RAUW and delete call or invoke return values that
  // may themselves be live over a statepoint.  For details, please see usage in
  // makeStatepointExplicitImpl.
  std::vector<DeferredReplacement> Replacements;

  // Now run through and replace the existing statepoints with new ones with
  // the live variables listed.  We do not yet update uses of the values being
  // relocated. We have references to live variables that need to
  // survive to the last iteration of this loop.  (By construction, the
  // previous statepoint can not be a live variable, thus we can and remove
  // the old statepoint calls as we go.)
  for (size_t i = 0; i < Records.size(); i++)
    makeStatepointExplicit(DT, ToUpdate[i], Records[i], Replacements);

  ToUpdate.clear(); // prevent accident use of invalid calls

  for (auto &PR : Replacements)
    PR.doReplacement();

  Replacements.clear();

  // At this point we should be able to delete all the bad loads and Phis.
  // There should be no reference to them. If there are, it will assert,
  // since the liveness args are already linked into the IR.
  for (Value *V : BadLoads)
    cast<Instruction>(V)->eraseFromParent();

  for (auto &Info : Records) {
    // These live sets may contain state Value pointers, since we replaced calls
    // with operand bundles with calls wrapped in gc.statepoint, and some of
    // those calls may have been def'ing live gc pointers.  Clear these out to
    // avoid accidentally using them.
    //
    // TODO: We should create a separate data structure that does not contain
    // these live sets, and migrate to using that data structure from this point
    // onward.
    Info.LiveSet.clear();
    Info.PointerToBase.clear();
  }

  // Do all the fixups of the original live variables to their relocated selves
  SmallVector<Value *, 128> Live;
  for (size_t i = 0; i < Records.size(); i++) {
    PartiallyConstructedSafepointRecord &Info = Records[i];

    // We can't simply save the live set from the original insertion.  One of
    // the live values might be the result of a call which needs a safepoint.
    // That Value* no longer exists and we need to use the new gc_result.
    // Thankfully, the live set is embedded in the statepoint (and updated), so
    // we just grab that.
    Statepoint Statepoint(Info.StatepointToken);
    Live.insert(Live.end(), Statepoint.gc_args_begin(),
                Statepoint.gc_args_end());
#ifndef NDEBUG
    // Do some basic sanity checks on our liveness results before performing
    // relocation.  Relocation can and will turn mistakes in liveness results
    // into non-sensical code which is must harder to debug.
    // TODO: It would be nice to test consistency as well
    assert(DT.isReachableFromEntry(Info.StatepointToken->getParent()) &&
           "statepoint must be reachable or liveness is meaningless");
    for (Value *V : Statepoint.gc_args()) {
      if (!isa<Instruction>(V))
        // Non-instruction values trivial dominate all possible uses
        continue;
      auto *LiveInst = cast<Instruction>(V);
      assert(DT.isReachableFromEntry(LiveInst->getParent()) &&
             "unreachable values should never be live");
      assert(DT.dominates(LiveInst, Info.StatepointToken) &&
             "basic SSA liveness expectation violated by liveness analysis");
    }
#endif
  }

#ifndef NDEBUG
  // sanity check
  for (auto *Ptr : Live)
    assert(isHandledGCPointerType(Ptr->getType()) &&
           "must be a gc pointer type");
#endif

  zeroAmbiguouslyLiveSlots(F, ToZero, AddrTakenAllocas);

  // In clobber-non-live mode, delete all lifetime markers, as the
  // inserted clobbering may be beyond the original lifetime.
  if (ClobberNonLive) {
    SetVector<Instruction *> ToDel;

    for (Instruction &I : instructions(F))
      if (CallInst *CI = dyn_cast<CallInst>(&I))
        if (Function *Fn = CI->getCalledFunction())
          if (Fn->getIntrinsicID() == Intrinsic::lifetime_start ||
              Fn->getIntrinsicID() == Intrinsic::lifetime_end)
            ToDel.insert(&I);

    for (Instruction *I : ToDel)
      I->eraseFromParent();
  }

  return !Records.empty();
}

// Handles both return values and arguments for Functions and calls.
template <typename AttrHolder>
static void RemoveNonValidAttrAtIndex(LLVMContext &Ctx, AttrHolder &AH,
                                      unsigned Index) {
  AttrBuilder R;
  if (AH.getDereferenceableBytes(Index))
    R.addAttribute(Attribute::get(Ctx, Attribute::Dereferenceable,
                                  AH.getDereferenceableBytes(Index)));
  if (AH.getDereferenceableOrNullBytes(Index))
    R.addAttribute(Attribute::get(Ctx, Attribute::DereferenceableOrNull,
                                  AH.getDereferenceableOrNullBytes(Index)));
  if (AH.getAttributes().hasAttribute(Index, Attribute::NoAlias))
    R.addAttribute(Attribute::NoAlias);

  if (!R.empty())
    AH.setAttributes(AH.getAttributes().removeAttributes(Ctx, Index, R));
}

static void stripNonValidAttributesFromPrototype(Function &F) {
  LLVMContext &Ctx = F.getContext();

  for (Argument &A : F.args())
    if (isa<PointerType>(A.getType()))
      RemoveNonValidAttrAtIndex(Ctx, F,
                                A.getArgNo() + AttributeList::FirstArgIndex);

  if (isa<PointerType>(F.getReturnType()))
    RemoveNonValidAttrAtIndex(Ctx, F, AttributeList::ReturnIndex);
}

/// Certain metadata on instructions are invalid after running this pass.
/// Optimizations that run after this can incorrectly use this metadata to
/// optimize functions. We drop such metadata on the instruction.
static void stripInvalidMetadataFromInstruction(Instruction &I) {
  if (!isa<LoadInst>(I) && !isa<StoreInst>(I))
    return;
  // These are the attributes that are still valid on loads and stores after
  // this pass.
  // The metadata implying dereferenceability and noalias are (conservatively)
  // dropped.  This is because semantically, after this pass runs,
  // all calls to gc.statepoint "free" the entire heap. Also, gc.statepoint can
  // touch the entire heap including noalias objects. Note: The reasoning is
  // same as stripping the dereferenceability and noalias attributes that are
  // analogous to the metadata counterparts.
  // We also drop the invariant.load metadata on the load because that metadata
  // implies the address operand to the load points to memory that is never
  // changed once it became dereferenceable. This is no longer true after this
  // pass. Similar reasoning applies to invariant.group metadata, which applies
  // to loads within a group.
  unsigned ValidMetadata[] = {LLVMContext::MD_tbaa,
                              LLVMContext::MD_range,
                              LLVMContext::MD_alias_scope,
                              LLVMContext::MD_nontemporal,
                              LLVMContext::MD_nonnull,
                              LLVMContext::MD_align,
                              LLVMContext::MD_type};

  // Drops all metadata on the instruction other than ValidMetadata.
  I.dropUnknownNonDebugMetadata(ValidMetadata);
}

static void stripNonValidDataFromBody(Function &F) {
  if (F.empty())
    return;

  LLVMContext &Ctx = F.getContext();
  MDBuilder Builder(Ctx);

  // Set of invariantstart instructions that we need to remove.
  // Use this to avoid invalidating the instruction iterator.
  SmallVector<IntrinsicInst*, 12> InvariantStartInstructions;

  for (Instruction &I : instructions(F)) {
    // invariant.start on memory location implies that the referenced memory
    // location is constant and unchanging. This is no longer true after
    // this pass runs because there can be calls to gc.statepoint
    // which frees the entire heap and the presence of invariant.start allows
    // the optimizer to sink the load of a memory location past a statepoint,
    // which is incorrect.
    if (auto *II = dyn_cast<IntrinsicInst>(&I))
      if (II->getIntrinsicID() == Intrinsic::invariant_start) {
        InvariantStartInstructions.push_back(II);
        continue;
      }

    if (MDNode *Tag = I.getMetadata(LLVMContext::MD_tbaa)) {
      MDNode *MutableTBAA = Builder.createMutableTBAAAccessTag(Tag);
      I.setMetadata(LLVMContext::MD_tbaa, MutableTBAA);
    }

    stripInvalidMetadataFromInstruction(I);

    if (auto *Call = dyn_cast<CallBase>(&I)) {
      for (int i = 0, e = Call->arg_size(); i != e; i++)
        if (isa<PointerType>(Call->getArgOperand(i)->getType()))
          RemoveNonValidAttrAtIndex(Ctx, *Call, i + AttributeList::FirstArgIndex);
      if (isa<PointerType>(Call->getType()))
        RemoveNonValidAttrAtIndex(Ctx, *Call, AttributeList::ReturnIndex);
    }
  }

  // Delete the invariant.start instructions and RAUW undef.
  for (auto *II : InvariantStartInstructions) {
    II->replaceAllUsesWith(UndefValue::get(II->getType()));
    II->eraseFromParent();
  }
}

/// Returns true if this function should be rewritten by this pass.
static bool shouldRewriteStatepointsIn(Function &F) {
  return F.hasGC();
}

static void stripNonValidData(Module &M) {
#ifndef NDEBUG
  assert(llvm::any_of(M, shouldRewriteStatepointsIn) && "precondition!");
#endif

  for (Function &F : M)
    stripNonValidAttributesFromPrototype(F);

  for (Function &F : M)
    stripNonValidDataFromBody(F);
}

bool GoStatepoints::runOnFunction(Function &F, DominatorTree &DT,
                                            TargetTransformInfo &TTI,
                                            const TargetLibraryInfo &TLI) {
  assert(!F.isDeclaration() && !F.empty() &&
         "need function body to rewrite statepoints in");
  assert(shouldRewriteStatepointsIn(F) && "mismatch in rewrite decision");

  if (!PrintFunc.empty())
    PrintLiveSet = F.getName() == PrintFunc;
  if (PrintLiveSet)
    dbgs() << "\n********** Liveness of function " << F.getName() << " **********\n";

  auto NeedsRewrite = [&TLI](Instruction &I) {
    if (const auto *Call = dyn_cast<CallBase>(&I))
      return !callsGCLeafFunction(Call, TLI) && !isStatepoint(Call);
    return false;
  };

  // Delete any unreachable statepoints so that we don't have unrewritten
  // statepoints surviving this pass.  This makes testing easier and the
  // resulting IR less confusing to human readers.
  DomTreeUpdater DTU(DT, DomTreeUpdater::UpdateStrategy::Lazy);
  bool MadeChange = removeUnreachableBlocks(F, nullptr, &DTU);

  // Rewrite all the calls that need statepoints to invokes, so we can
  // attach a stack map through its landing pad.
  SmallVector<CallInst *, 64> Calls;
  for (Instruction &I : instructions(F))
    if (NeedsRewrite(I) && isa<CallInst>(I))
      Calls.push_back(cast<CallInst>(&I));

  if (!Calls.empty()) {
    MadeChange = true;

    for (CallInst *CI : Calls) {
      // Create a dummy landing pad block.
      LLVMContext &C = F.getContext();
      BasicBlock *PadBB = BasicBlock::Create(C, "dummy", &F);
      Type *ExnTy = StructType::get(Type::getInt8PtrTy(C), Type::getInt32Ty(C));

      LandingPadInst *LPad =
          LandingPadInst::Create(ExnTy, 1, "dummy.ex", PadBB);
      LPad->addClause(Constant::getNullValue(Type::getInt8PtrTy(C)));
      new UnreachableInst(PadBB->getContext(), PadBB);

      BasicBlock *Old = CI->getParent();
      BasicBlock *New = changeToInvokeAndSplitBasicBlock(CI, PadBB);

      // Old dominates New. New node dominates all other nodes dominated by Old.
      DomTreeNode *OldNode = DT.getNode(Old);
      std::vector<DomTreeNode *> Children(OldNode->begin(), OldNode->end());

      DomTreeNode *NewNode = DT.addNewBlock(New, Old);
      for (DomTreeNode *I : Children)
        DT.changeImmediateDominator(I, NewNode);

      DTU.insertEdge(Old, PadBB);
    }
  }

  // We should not run removeUnreachableBlocks at this point, as it
  // will remove the dummy landing pads.

  // Flush the Dominator Tree.
  DTU.getDomTree();

  // Gather all the statepoints which need rewritten.  Be careful to only
  // consider those in reachable code since we need to ask dominance queries
  // when rewriting.  We'll delete the unreachable ones in a moment.
  SmallVector<CallBase *, 64> ParsePointNeeded;
  for (Instruction &I : instructions(F)) {
    // TODO: only the ones with the flag set!
    if (NeedsRewrite(I)) {
      // NOTE removeUnreachableBlocks() is stronger than
      // DominatorTree::isReachableFromEntry(). In other words
      // removeUnreachableBlocks can remove some blocks for which
      // isReachableFromEntry() returns true.
      assert(DT.isReachableFromEntry(I.getParent()) &&
            "no unreachable blocks expected");
      ParsePointNeeded.push_back(cast<CallBase>(&I));
    }
  }

  // Return early if no work to do.
  if (ParsePointNeeded.empty())
    return MadeChange;

  // As a prepass, go ahead and aggressively destroy single entry phi nodes.
  // These are created by LCSSA.  They have the effect of increasing the size
  // of liveness sets for no good reason.  It may be harder to do this post
  // insertion since relocations and base phis can confuse things.
  for (BasicBlock &BB : F)
    if (BB.getUniquePredecessor()) {
      MadeChange = true;
      FoldSingleEntryPHINodes(&BB);
    }

  // Before we start introducing relocations, we want to tweak the IR a bit to
  // avoid unfortunate code generation effects.  The main example is that we
  // want to try to make sure the comparison feeding a branch is after any
  // safepoints.  Otherwise, we end up with a comparison of pre-relocation
  // values feeding a branch after relocation.  This is semantically correct,
  // but results in extra register pressure since both the pre-relocation and
  // post-relocation copies must be available in registers.  For code without
  // relocations this is handled elsewhere, but teaching the scheduler to
  // reverse the transform we're about to do would be slightly complex.
  // Note: This may extend the live range of the inputs to the icmp and thus
  // increase the liveset of any statepoint we move over.  This is profitable
  // as long as all statepoints are in rare blocks.  If we had in-register
  // lowering for live values this would be a much safer transform.
  auto getConditionInst = [](Instruction *TI) -> Instruction* {
    if (auto *BI = dyn_cast<BranchInst>(TI))
      if (BI->isConditional())
        return dyn_cast<Instruction>(BI->getCondition());
    // TODO: Extend this to handle switches
    return nullptr;
  };
  for (BasicBlock &BB : F) {
    Instruction *TI = BB.getTerminator();
    if (auto *Cond = getConditionInst(TI))
      // TODO: Handle more than just ICmps here.  We should be able to move
      // most instructions without side effects or memory access.
      if (isa<ICmpInst>(Cond) && Cond->hasOneUse()) {
        MadeChange = true;
        Cond->moveBefore(TI);
      }
  }

  MadeChange |= insertParsePoints(F, DT, TTI, ParsePointNeeded);
  return MadeChange;
}

// liveness computation via standard dataflow
// -------------------------------------------------------------------

// TODO: Consider using bitvectors for liveness, the set of potentially
// interesting values should be small and easy to pre-compute.

static Value*
isAlloca(Value *V, DefiningValueMapTy &DVCache) {
  Value *Base = findBaseOrBDV(V, DVCache);
  return isa<AllocaInst>(Base) ? Base : nullptr;
}

static Value*
isTrackedAlloca(Value *V, DefiningValueMapTy &DVCache) {
  Value *Base = isAlloca(V, DVCache);
  if (Base &&
      hasPointer(Base->getType()->getPointerElementType()))
    return Base;
  return nullptr;
}

static bool
hasStructRetAttr(CallBase *Call) {
  return Call->hasStructRetAttr() ||
         (Call->getNumArgOperands() > 0 &&
          Call->getParamAttr(0, "go_sret") != Attribute());
}

/// Compute the live-in set for the location rbegin starting from
/// the live-out set of the basic block
static void computeLiveInValues(BasicBlock::reverse_iterator Begin,
                                BasicBlock::reverse_iterator End,
                                SetVector<Value *> &LiveTmp,
                                SetVector<Value *> &AddrTakenAllocas,
                                DefiningValueMapTy &DVCache) {
  for (auto &I : make_range(Begin, End)) {
    // KILL/Def - Remove this definition from LiveIn
    LiveTmp.remove(&I);

    // Don't consider *uses* in PHI nodes, we handle their contribution to
    // predecessor blocks when we seed the LiveOut sets
    if (isa<PHINode>(I))
      continue;

    // USE - Add to the LiveIn set for this instruction
    for (Value *V : I.operands()) {
      assert(!isUnhandledGCPointerType(V->getType()) &&
             "unexpected value type");
      if (isHandledGCPointerType(V->getType()) && !isa<Constant>(V)) {
        // The choice to exclude all things constant here is slightly subtle.
        // There are two independent reasons:
        // - We assume that things which are constant (from LLVM's definition)
        // do not move at runtime.  For example, the address of a global
        // variable is fixed, even though it's contents may not be.
        // - Second, we can't disallow arbitrary inttoptr constants even
        // if the language frontend does.  Optimization passes are free to
        // locally exploit facts without respect to global reachability.  This
        // can create sections of code which are dynamically unreachable and
        // contain just about anything.  (see constants.ll in tests)

        if (isAlloca(V, DVCache)) {
          Value *Base = isTrackedAlloca(V, DVCache);
          if (!Base || AddrTakenAllocas.count(Base))
            continue;

          // For non-address-taken alloca, record its use.
          if (isa<DbgInfoIntrinsic>(I) || isa<BitCastInst>(I) ||
              isa<GetElementPtrInst>(I) || isa<ICmpInst>(I) ||
              isa<AddrSpaceCastInst>(I))
            // Not real use.
            continue;
          if (isa<LoadInst>(I) || isa<StoreInst>(I) || isa<InvokeInst>(I)) {
            LiveTmp.insert(Base);
            continue;
          }

          if (CallInst *CI = dyn_cast<CallInst>(&I)) {
            if (Function *Fn = CI->getCalledFunction())
              switch (Fn->getIntrinsicID()) {
              case Intrinsic::lifetime_start:
              case Intrinsic::lifetime_end:
              case Intrinsic::assume:
                // Not real use.
                continue;
              default:
                break;
              }
            LiveTmp.insert(Base);
            continue;
          }

          // We know it is not address-taken, other operation should not happen.
          assert(false && "illegal operation on non-address-taken alloca");
        }

        LiveTmp.insert(V);
      }
    }
  }
}

// Compute the def and kill of allocas.
static void
computeAllocaDefs(BasicBlock::iterator Begin,
                  BasicBlock::iterator End,
                  SetVector<Value *> &AllocaDefs,
                  SetVector<Value *> &AllocaKills,
                  DefiningValueMapTy &DVCache) {
  // Iterate forwards over the instructions, record the defs and kills
  // of allocas.
  // Notes on the special cases about def and kill appearing in the same
  // block:
  // - kill after def:
  //   Record the kill but don't remove it from def set, since we will
  //   subtract the kill set anyway. And when a slot is initialized and
  //   then killed in the same block, we don't lose information.
  // - def after kill:
  //   The def overrides the kill, i.e. remove it from the kill set
  //   (unless we see a kill again later). So we have the information
  //   that the slot is initialized at the end of the block, even after
  //   we subtract the kill set.
  for (auto &I : make_range(Begin, End)) {
    // skip Phi ?
    if (isa<PHINode>(I))
      continue;

    if (StoreInst *SI = dyn_cast<StoreInst>(&I)){
      Value *V = SI->getPointerOperand();
      if (Value *Base = isTrackedAlloca(V, DVCache)) {
        AllocaDefs.insert(Base);
        AllocaKills.remove(Base);
      }
      continue;
    }

    if (CallInst *CI = dyn_cast<CallInst>(&I)){
      if (hasStructRetAttr(CI)) {
        Value *V = CI->getOperand(0);
        if (Value *Base = isTrackedAlloca(V, DVCache)) {
          AllocaDefs.insert(Base);
          AllocaKills.remove(Base);
        }
      }
      if (Function *Fn = CI->getCalledFunction())
        switch (Fn->getIntrinsicID()) {
        case Intrinsic::memmove:
        case Intrinsic::memcpy:
        case Intrinsic::memset: {
          // We're writing to the first arg.
          Value *V = CI->getOperand(0);
          if (Value *Base = isTrackedAlloca(V, DVCache)) {
            AllocaDefs.insert(Base);
            AllocaKills.remove(Base);
          }
          break;
        }
        case Intrinsic::lifetime_end: {
          Value *V = CI->getOperand(1);
          if (Value *Base = isTrackedAlloca(V, DVCache)) {
            AllocaKills.insert(Base);
          }
          break;
        }
        default:
          break;
        }
      continue;
    }

    if (InvokeInst *II = dyn_cast<InvokeInst>(&I)) {
      if (hasStructRetAttr(II)) {
        Value *V = II->getOperand(0);
        if (Value *Base = isTrackedAlloca(V, DVCache)) {
          AllocaDefs.insert(Base);
          AllocaKills.remove(Base);
        }
      }
      continue;
    }
  }
}

// Determine whether an alloca has its address taken.
// We use different mechanisms to track the liveness of
// address-taken and non-address-taken allocas.
// Also keep track the location where the address is used.
// The alloca needs to be live where its address is taken.
static void
determineAllocaAddrTaken(Function &F,
                         SetVector<Value *> &AddrTakenAllocas,
                         MapVector<BasicBlock *, SetVector<Value *>> &AllocaAddrUse,
                         DefiningValueMapTy &DVCache) {
  // Use the metadata inserted by the FE.
  for (Instruction &I : F.getEntryBlock())
    if (isa<AllocaInst>(I) && I.getMetadata("go_addrtaken") &&
        hasPointer(I.getType()->getPointerElementType()))
      AddrTakenAllocas.insert(&I);

  // The FE's addrtaken mark may be imprecise. Look for certain
  // operations in the IR to mark as addrtaken.
  // The address may be passed as argument to functions. We trust
  // the FE that if it is not marked as addrtaken, the function
  // won't hold its address. (for example, the equality function
  // of aggregate types.)
  for (Instruction &I : instructions(F)) {
    if (isa<PHINode>(I) || isa<SelectInst>(I))
      // Phi/select could happen even it is not really addrtaken:
      // for example, an IR transformation like
      //   if (cond) { x = a } else { x = b }
      // ==>
      //   if (cond) { tmp = &a } else { tmp = &b }; x = *tmp
      // Things get complicated with them. For now, treat them as
      // address taken.
      for (Value *V : I.operands()) {
        if (!isHandledGCPointerType(V->getType()))
          continue;
        if (Value *Base = isTrackedAlloca(V, DVCache)) {
          AddrTakenAllocas.insert(Base);
          AllocaAddrUse[I.getParent()].insert(Base);
        }
      }
    else if (StoreInst *SI = dyn_cast<StoreInst>(&I)) {
      // If the address of a slot is stored, it must be addrtaken.
      // In most cases the FE marks it. One exception is the array
      // holding the ... args.
      // TODO: maybe we should fix the FE?
      Value *V = SI->getValueOperand();
      if (isHandledGCPointerType(V->getType()))
        if (Value *Base = isTrackedAlloca(V, DVCache)) {
          AddrTakenAllocas.insert(Base);
          AllocaAddrUse[I.getParent()].insert(Base);
        }
    }
  }
}

static void computeLiveOutSeed(BasicBlock *BB, SetVector<Value *> &LiveTmp,
                               DefiningValueMapTy &DVCache) {
  for (BasicBlock *Succ : successors(BB)) {
    for (auto &I : *Succ) {
      PHINode *PN = dyn_cast<PHINode>(&I);
      if (!PN)
        break;

      Value *V = PN->getIncomingValueForBlock(BB);
      assert(!isUnhandledGCPointerType(V->getType()) &&
             "unexpected value type");
      if (isHandledGCPointerType(V->getType()) && !isa<Constant>(V)) {
        if (isAlloca(V, DVCache))
          // Alloca is tracked separately. (It is a Phi arg so it
          // must be address-taken.)
          continue;
        LiveTmp.insert(V);
      }
    }
  }
}

static SetVector<Value *> computeKillSet(BasicBlock *BB, DefiningValueMapTy &DVCache) {
  SetVector<Value *> KillSet;
  for (Instruction &I : *BB) {
    if (isHandledGCPointerType(I.getType()))
      KillSet.insert(&I);
  }
  return KillSet;
}

#ifndef NDEBUG
/// Check that the items in 'Live' dominate 'TI'.  This is used as a basic
/// sanity check for the liveness computation.
static void checkBasicSSA(DominatorTree &DT, SetVector<Value *> &Live,
                          Instruction *TI, bool TermOkay = false) {
  for (Value *V : Live) {
    if (auto *I = dyn_cast<Instruction>(V)) {
      // The terminator can be a member of the LiveOut set.  LLVM's definition
      // of instruction dominance states that V does not dominate itself.  As
      // such, we need to special case this to allow it.
      if (TermOkay && TI == I)
        continue;
      assert(DT.dominates(I, TI) &&
             "basic SSA liveness expectation violated by liveness analysis");
    }
  }
}

/// Check that all the liveness sets used during the computation of liveness
/// obey basic SSA properties.  This is useful for finding cases where we miss
/// a def.
static void checkBasicSSA(DominatorTree &DT, GCPtrLivenessData &Data,
                          BasicBlock &BB) {
  checkBasicSSA(DT, Data.LiveSet[&BB], BB.getTerminator());
  checkBasicSSA(DT, Data.LiveOut[&BB], BB.getTerminator(), true);
  checkBasicSSA(DT, Data.LiveIn[&BB], BB.getTerminator());
}
#endif

// For initialization of an aggregate-typed slot, check whether the
// whole storage is initialized before we reach a statepoint, and
// insert zeroing if not.
// Normally the FE has lifted calls out of the initialization sequence.
// But they may occur due to optimizations, for example,
//   type A struct { ...; b B; ... }
//   type B struct { ... }
//   a := A{ ..., b: SomeB(), ... }
// The FE generates something like
//   %a = alloca A
//   %tmp = alloca B
//   call SomeB(%tmp) // as outgoing arg
//   initialize part of a
//   call memmove(gep %a, %tmp)
//   initialize the rest of a
// The memmove may be optimized out, with direct store to A, as
//   %a = alloca A
//   initialize part of a
//   call SomeB(gep %a)
//   initialize the rest of a
// a is live at the call site, but not fully initialized.
// We need to make sure a doesn't contain bad pointers.
// TODO: this function is a little too conservative (see below).
// TODO: instead of zeroing, maybe we can record only the part
// of A that is live?
static void
checkStoreSize(Value *V, BasicBlock &BB, const DataLayout &DL,
               SetVector<Value *> &ToZero,
               DefiningValueMapTy &DVCache) {
  unsigned PtrSize = DL.getPointerSize();
  unsigned Size = DL.getTypeStoreSize(V->getType()->getPointerElementType());
  if (Size <= PtrSize)
    return;

  // We simply add the sizes of all stores in the block, assuming
  // no overlapping stores (which are silly).
  unsigned StoreSize = 0;
  for (Instruction &I : BB) {
    if (StoreInst *SI = dyn_cast<StoreInst>(&I)) {
      Value *Ptr = SI->getPointerOperand();
      if (isTrackedAlloca(Ptr, DVCache) == V)
        StoreSize += DL.getTypeStoreSize(SI->getValueOperand()->getType());
    } else if (CallInst *CI = dyn_cast<CallInst>(&I)) {
      if (hasStructRetAttr(CI)) {
        Value *Ptr = CI->getOperand(0);
        if (isTrackedAlloca(Ptr, DVCache) == V)
          StoreSize += DL.getTypeStoreSize(Ptr->getType()->getPointerElementType());
      }
      if (Function *Fn = CI->getCalledFunction())
        switch (Fn->getIntrinsicID()) {
        case Intrinsic::memmove:
        case Intrinsic::memcpy:
        case Intrinsic::memset: {
          // We're writing to the first arg. The third arg is size.
          Value *Ptr = CI->getOperand(0);
          if (isTrackedAlloca(Ptr, DVCache) == V) {
            ConstantInt *Len =
                dyn_cast<ConstantInt>(cast<MemIntrinsic>(CI)->getLength());
            if (Len)
              StoreSize += Len->getZExtValue();
          }
          break;
        }
        default:
          break;
        }
    } else if (InvokeInst *II = dyn_cast<InvokeInst>(&I)) {
      if (hasStructRetAttr(II)) {
        Value *Ptr = II->getOperand(0);
        if (isTrackedAlloca(Ptr, DVCache) == V)
          if (DL.getTypeStoreSize(Ptr->getType()->getPointerElementType()) + PtrSize - 1 >= Size)
            // We are storing the whole type
            return;

          // Othersize we may have stored pointers into the alloca, which
          // need to be live, but it is not completely initialized. We need
          // to zero it.
          // TODO: no need to zero if all previous stores are scalars.
      }
    }

    // We only care about pointers, so it's safe to round up to a pointer size.
    // TODO: things with more than a simple padding may still be false positive.
    // TODO: if the missing fields are all scalars, no need to zero.
    if (StoreSize + PtrSize - 1 >= Size)
      return; // early return if we have stored enough.
  }

  // Incomplete initialization, needs zeroing.
  if (StoreSize + PtrSize - 1 < Size)
    ToZero.insert(V);
}

static void computeLiveInValues(DominatorTree &DT, Function &F,
                                GCPtrLivenessData &Data,
                                SetVector<Value *> &AddrTakenAllocas,
                                SetVector<Value *> &ToZero,
                                SetVector<Value *> &BadLoads,
                                DefiningValueMapTy &DVCache) {
  MapVector<BasicBlock *, SetVector<Value *>> AllocaAddrUse;
  determineAllocaAddrTaken(F, AddrTakenAllocas, AllocaAddrUse, DVCache);
  if (PrintLiveSet) {
    dbgs() << "AddrTakenAllocas:\n";
    printLiveSet(AddrTakenAllocas);
  }

  // Seed the liveness for each individual block
  for (BasicBlock &BB : F) {
    Data.KillSet[&BB] = computeKillSet(&BB, DVCache);
    Data.LiveSet[&BB].clear();
    computeLiveInValues(BB.rbegin(), BB.rend(), Data.LiveSet[&BB], AddrTakenAllocas, DVCache);
    computeAllocaDefs(BB.begin(), BB.end(), Data.AllocaDefSet[&BB], Data.AllocaKillSet[&BB], DVCache);
    Data.AllocaDefAny[&BB] = Data.AllocaDefSet[&BB];
    Data.AllocaDefAny[&BB].set_subtract(Data.AllocaKillSet[&BB]);
    Data.LiveOut[&BB] = SetVector<Value *>();
    computeLiveOutSeed(&BB, Data.LiveOut[&BB], DVCache);

#ifndef NDEBUG
    for (Value *Kill : Data.KillSet[&BB])
      assert(!Data.LiveSet[&BB].count(Kill) && "live set contains kill");
#endif
  }

  // Propagate Alloca def any until stable.
  bool changed = true;
again:
  while (changed) {
    changed = false;
    for (BasicBlock &BB : F) {
      unsigned OldSize = Data.AllocaDefAny[&BB].size();
      for (BasicBlock *Pred : predecessors(&BB))
        Data.AllocaDefAny[&BB].set_union(Data.AllocaDefAny[Pred]);
      Data.AllocaDefAny[&BB].set_subtract(Data.AllocaKillSet[&BB]);
      if (Data.AllocaDefAny[&BB].size() != OldSize)
        changed = true;
    }
  }

  // When a slot's address is taken, it can be live any point after it.
  // It can also be initialized "indirectly", like
  //   tmp = phi(&a, &b)
  //   *tmp = ...
  // computeAllocaDefs doesn't see this initialization.
  // It can be initialized some time later, or never. We don't know for
  // sure. The slot needs to be live. And we need to pre-zero it, if we
  // don't otherwise know it is initialized.
  if (!AllocaAddrUse.empty()) {
    for (BasicBlock &BB : F) {
      AllocaAddrUse[&BB].set_subtract(Data.AllocaKillSet[&BB]);
      for (Value *V : AllocaAddrUse[&BB])
        if (!Data.AllocaDefAny[&BB].count(V)) {
          Data.AllocaDefAny[&BB].insert(V);
          ToZero.insert(V);
          changed = true;
        }
    }
    AllocaAddrUse.clear();
    if (changed)
      goto again; // re-propagate alloca def any
  }

  // Propagate Alloca def all until stable.
  for (BasicBlock &BB : F)
    Data.AllocaDefAll[&BB] = Data.AllocaDefAny[&BB];
  changed = true;
  while (changed) {
    changed = false;
    for (BasicBlock &BB : F) {
      auto NotDefAll = [&](Value *V){
        if (Data.AllocaDefSet[&BB].count(V) != 0)
          return false;
        for (BasicBlock *Pred : predecessors(&BB))
          if (Data.AllocaDefAll[Pred].count(V) == 0) {
            if (PrintLiveSet)
              dbgs() << ">>> removing " << V->getName() << " from " <<
                        BB.getName() << " DefAll;  pred = " <<
                        Pred->getName() << "\n";
            return true;
          }
        return false;
      };
      if (Data.AllocaDefAll[&BB].remove_if(NotDefAll))
        changed = true;
    }
  }

  const DataLayout &DL = F.getParent()->getDataLayout();

  // An alloca is live only after it is initialized.
  // It is initialized in a block if it is defined there and not defined
  // in all of the predecessors (or there is no predecessors).
  for (BasicBlock &BB : F) {
    for (Value *V : Data.AllocaDefSet[&BB]) {
      bool Init = false;
      if (&BB == &F.getEntryBlock())
        Init = true;
      for (BasicBlock *Pred : predecessors(&BB))
        if (!Data.AllocaDefAll[Pred].count(V)) {
          Init = true;
          break;
        }
      if (!Init)
        continue;
      if (!AddrTakenAllocas.count(V)) { // addr-taken alloca is tracked separately
        Data.KillSet[&BB].insert(V);
        Data.LiveSet[&BB].remove(V);
      }

      // If it is incomplete initialization, it needs zeroing.
      checkStoreSize(V, BB, DL, ToZero, DVCache);
    }
  }

  SmallSetVector<BasicBlock *, 32> Worklist;
  for (BasicBlock &BB : F) {
    Data.LiveIn[&BB] = Data.LiveSet[&BB];
    Data.LiveIn[&BB].set_union(Data.LiveOut[&BB]);
    Data.LiveIn[&BB].set_subtract(Data.KillSet[&BB]);
    if (!Data.LiveIn[&BB].empty())
      Worklist.insert(pred_begin(&BB), pred_end(&BB));
  }

  // Propagate liveness until stable
  while (!Worklist.empty()) {
    BasicBlock *BB = Worklist.pop_back_val();

    // Compute our new liveout set, then exit early if it hasn't changed despite
    // the contribution of our successor.
    SetVector<Value *> LiveOut = Data.LiveOut[BB];
    const auto OldLiveOutSize = LiveOut.size();
    for (BasicBlock *Succ : successors(BB)) {
      assert(Data.LiveIn.count(Succ));
      LiveOut.set_union(Data.LiveIn[Succ]);
    }
    // assert OutLiveOut is a subset of LiveOut
    if (OldLiveOutSize == LiveOut.size()) {
      // If the sets are the same size, then we didn't actually add anything
      // when unioning our successors LiveIn.  Thus, the LiveIn of this block
      // hasn't changed.
      continue;
    }
    Data.LiveOut[BB] = LiveOut;

    // Apply the effects of this basic block
    SetVector<Value *> LiveTmp = LiveOut;
    LiveTmp.set_union(Data.LiveSet[BB]);
    LiveTmp.set_subtract(Data.KillSet[BB]);

    assert(Data.LiveIn.count(BB));
    const SetVector<Value *> &OldLiveIn = Data.LiveIn[BB];
    // assert: OldLiveIn is a subset of LiveTmp
    if (OldLiveIn.size() != LiveTmp.size()) {
      Data.LiveIn[BB] = LiveTmp;
      Worklist.insert(pred_begin(BB), pred_end(BB));
    }
  } // while (!Worklist.empty())

  // Find the bad loads, i.e. loads from uninitialized slots.
  // See also the comment in function insertParsePoints.
  for (Instruction &I : instructions(F))
    if (LoadInst *LI = dyn_cast<LoadInst>(&I)) {
      Value *V = LI->getPointerOperand();
      if (Value *Base = isTrackedAlloca(V, DVCache)) {
        BasicBlock *BB = LI->getParent();
        // AllocaDefAll is the set of allocas initialized at the
        // end of BB. It doesn't include ones that are killed in
        // BB (as they don't reach the end). Add KillSet explicitly.
        if (!Data.AllocaDefAll[BB].count(Base) &&
            !Data.AllocaKillSet[BB].count(Base) &&
            !AddrTakenAllocas.count(Base)) {
          BadLoads.insert(LI);
          //dbgs() << "!!! load off uninitialized slot:\n\t" <<
          //          F.getName() << "\n\t" << *LI << "\n";
        }
      }
    }

  // Sanity check: live alloca must be initialized.
  // Due to the reason above, uninitialized slot may appear live,
  // as the bad load counts as a use. Remove them, as well as the
  // bad loads.
  for (BasicBlock &BB : F) {
    auto NotDefAll = [&](Value *V){
      if (isa<AllocaInst>(V)) {
        if (!Data.AllocaDefAll[&BB].count(V)) {
          //dbgs() << "!!! alloca live but not initialized:\n\t" <<
          //          F.getName() << "  " << BB.getName() << "\n" << *V << "\n";
          return true;
        }
      }
      return false;
    };
    Data.LiveOut[&BB].remove_if(NotDefAll);
    Data.LiveOut[&BB].set_subtract(BadLoads);
  }

  // After this point, we only care address-taken allocas. Remove the rest.
  for (BasicBlock &BB : F) {
    auto NotAddrTaken = [&](Value *V){ return !AddrTakenAllocas.count(V); };
    Data.AllocaDefAny[&BB].remove_if(NotAddrTaken);

    // AllocaDefAll doesn't really matter, because we subtract it below.
    // Update it just for printing.
    Data.AllocaDefAll[&BB].remove_if(NotAddrTaken);
  }

  // Address-taken allocas initialized and not killed at the end of block is live-out.
  // We don't update live-in sets, since live-in is not used after this point.
  for (BasicBlock &BB : F)
    Data.LiveOut[&BB].set_union(Data.AllocaDefAny[&BB]);

  // Record ambiguously live slots (AllocaDefAny - AllocaDefAll), which we need to zero.
  for (BasicBlock &BB : F) {
    if (PrintLiveSet) {
      dbgs() << BB.getName() << " AllocaDefAny:\n";
      printLiveSet(Data.AllocaDefAny[&BB]);
      dbgs() << BB.getName() << " AllocaDefAll:\n";
      printLiveSet(Data.AllocaDefAll[&BB]);
    }

    // NOTE: this clobbers AllocaDefAny. Don't use it after this point.
    Data.AllocaDefAny[&BB].set_subtract(Data.AllocaDefAll[&BB]);
    ToZero.set_union(Data.AllocaDefAny[&BB]);

    if (PrintLiveSet) {
      dbgs() << BB.getName() << " ambiguously live:\n";
      printLiveSet(Data.AllocaDefAny[&BB]);
      dbgs() << BB.getName() << " LiveOut:\n";
      printLiveSet(Data.LiveOut[&BB]);
    }
  }

#ifndef NDEBUG
  // Sanity check our output against SSA properties.  This helps catch any
  // missing kills during the above iteration.
  for (BasicBlock &BB : F)
    checkBasicSSA(DT, Data, BB);
#endif
}

// Compute the set of values live at Inst, store the result in Out.
//
// Side effect: in clobber-non-live mode, the clobbering instructions
// are inserted here.
static void findLiveSetAtInst(Instruction *Inst, GCPtrLivenessData &Data,
                              SetVector<Value *> &AddrTakenAllocas,
                              StatepointLiveSetTy &Out,
                              SetVector<Value *> &AllAllocas,
                              DefiningValueMapTy &DVCache) {
  BasicBlock *BB = Inst->getParent();

  // Note: The copy is intentional and required
  assert(Data.LiveOut.count(BB));
  SetVector<Value *> LiveOut = Data.LiveOut[BB];

  // We want to handle the statepoint itself oddly.  It's
  // call result is not live (normal), nor are it's arguments
  // (unless they're used again later).
  // The statepoint is always an invoke instruction, which is the last
  // instruction in the block. The only thing it can initialize is its
  // result (passed directly, or indirectly as outgoing arg).
  LiveOut.remove(Inst);
  if (InvokeInst *II = dyn_cast<InvokeInst>(Inst))
    if (hasStructRetAttr(II)) {
      Value *Ptr = II->getOperand(0);
      Value *V = Ptr->stripPointerCasts();
      const DataLayout &DL = Inst->getModule()->getDataLayout();
      if (!Data.LiveIn[BB].count(V) &&
          (DL.getTypeStoreSize(Ptr->getType()->getPointerElementType()) >=
           DL.getTypeStoreSize(V->getType()->getPointerElementType())))
        LiveOut.remove(V);
    }

  // Clobber all non-live allocas.
  if (ClobberNonLive) {
    SetVector<Value *> ToClobber(AllAllocas);
    ToClobber.set_subtract(LiveOut);
    if (!ToClobber.empty()) {
      IRBuilder<> Builder(Inst);
      Type *Int8Ty = IntegerType::get(Inst->getModule()->getContext(), 8);
      const DataLayout &DL = Inst->getModule()->getDataLayout();
      Value *Bad = ConstantInt::get(Int8Ty, 0xff);
      for (Value *Alloca : ToClobber) {
        unsigned Siz =
            DL.getTypeStoreSize(Alloca->getType()->getPointerElementType());
        Builder.CreateMemSet(Alloca, Bad, Siz, 0);
        //dbgs() << "clobber " << *Alloca << " at " << *Inst << "\n";
      }
    }
  }

  Out.insert(LiveOut.begin(), LiveOut.end());
}

// Remove write barriers for stack writes, for
// 1. write barriers are unnecessary for stack writes,
// 2. if a write barrier is applied to a write on an uninitialized slot,
//    the GC may see the bad content.
// This is not the best way to do it: it doesn't remove the conditional
// branch that tests if the write barrier is on.
// It may be better that we insert write barriers not that early.
//
// This function is not really related to statepoints. It is here so
// it can reuse the base pointer calculations (and caching).
static void
fixStackWriteBarriers(Function &F, DefiningValueMapTy &DVCache) {
  SmallSet<Instruction *, 8> ToDel;

  for (Instruction &I : instructions(F)) {
    if (auto *CI = dyn_cast<CallInst>(&I))
      if (Function *Callee = CI->getCalledFunction()) {
        if (Callee->getName().equals("runtime.gcWriteBarrier")) {
          // gcWriteBarrier(dst, val)
          // there is an extra "nest" argument.
          Value *Dst = CI->getArgOperand(1), *Val = CI->getArgOperand(2);
          if (!isAlloca(Dst, DVCache))
            continue;
          IRBuilder<> Builder(CI);
          unsigned AS = Dst->getType()->getPointerAddressSpace();
          Dst = Builder.CreateBitCast(Dst,
                                      PointerType::get(Val->getType(), AS));
          Builder.CreateStore(Val, Dst);
          ToDel.insert(CI);
        } else if (Callee->getName().equals("runtime.typedmemmove")) {
          // typedmemmove(typ, dst, src)
          // there is an extra "nest" argument.
          Value *Dst = CI->getArgOperand(2), *Src = CI->getArgOperand(3);
          if (!isAlloca(Dst, DVCache))
            continue;
          IRBuilder<> Builder(CI);
          // We should know the size at compile time, but at this stage I
          // don't know how to retrieve it. Load from the type descriptor
          // for now. The size is the first field. The optimizer should be
          // able to constant-fold it.
          Value *TD = CI->getArgOperand(1);
          Value *GEP = Builder.CreateConstInBoundsGEP2_32(
              TD->getType()->getPointerElementType(), TD, 0, 0);
          Value *Siz = Builder.CreateLoad(GEP);
          Builder.CreateMemMove(Dst, 0, Src, 0, Siz);
          ToDel.insert(CI);
        }
      }
  }

  for (Instruction *I : ToDel)
    I->eraseFromParent();
}
