//===--- Util.cpp ---------------------------------------------------------===//
//
// Copyright 2018 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
//
//===----------------------------------------------------------------------===//
//
// Helper functions.
//
//===----------------------------------------------------------------------===//

#include "GollvmPasses.h"

#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/BitVector.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Value.h"

using namespace llvm;
using namespace gollvm::passes;

// Whether a type contains pointer.
bool
gollvm::passes::hasPointer(Type *T) {
  switch (T->getTypeID()) {
  case Type::PointerTyID:
    return true;
  case Type::ArrayTyID:
    return hasPointer(T->getArrayElementType());
  case Type::VectorTyID:
    return hasPointer(T->getVectorElementType());
  case Type::StructTyID: {
    for (unsigned i = 0, e = T->getStructNumElements(); i < e; ++i)
      if (hasPointer(T->getStructElementType(i)))
        return true;
    return false;
  }
  default:
    return false;
  }
}

static void
getPtrBitmapForTypeHelper(Type *T, const DataLayout &DL, uint64_t BaseOffset, BitVector &BV) {
  if (!hasPointer(T))
    return;

  const unsigned PtrSize = DL.getPointerSize();
  Type *Int32Ty = Type::getInt32Ty(T->getContext());
  switch (T->getTypeID()) {
  case Type::PointerTyID:
    BV.set(BaseOffset / PtrSize);
    break;;
  case Type::ArrayTyID: {
    Type *ET = T->getArrayElementType();
    for (unsigned i = 0, n = T->getArrayNumElements(); i < n; ++i) {
      Value *ivals[2] = { ConstantInt::get(Int32Ty, 0),
                          ConstantInt::get(Int32Ty, i) };
      ArrayRef<Value*> Idx = makeArrayRef(ivals, 2);
      uint64_t Offset = DL.getIndexedOffsetInType(T, Idx);
      getPtrBitmapForTypeHelper(ET, DL, BaseOffset+Offset, BV);
    }
    break;
  }
  case Type::VectorTyID: {
    Type *ET = T->getVectorElementType();
    for (unsigned i = 0, n = T->getVectorNumElements(); i < n; ++i) {
      Value *ivals[2] = { ConstantInt::get(Int32Ty, 0),
                          ConstantInt::get(Int32Ty, i) };
      ArrayRef<Value*> Idx = makeArrayRef(ivals, 2);
      uint64_t Offset = DL.getIndexedOffsetInType(T, Idx);
      getPtrBitmapForTypeHelper(ET, DL, BaseOffset+Offset, BV);
    }
    break;
  }
  case Type::StructTyID: {
    for (unsigned i = 0, n = T->getStructNumElements(); i < n; ++i) {
      Type *ET = T->getStructElementType(i);
      if (!hasPointer(ET))
        continue;
      Value *ivals[2] = { ConstantInt::get(Int32Ty, 0),
                          ConstantInt::get(Int32Ty, i) };
      ArrayRef<Value*> Idx = makeArrayRef(ivals, 2);
      uint64_t Offset = DL.getIndexedOffsetInType(T, Idx);
      getPtrBitmapForTypeHelper(ET, DL, BaseOffset+Offset, BV);
    }
    break;
  }
  default:
    break;
  }
}

// Compute the pointer bitmap for type T, stored into Words.
void
gollvm::passes::getPtrBitmapForType(Type *T, const DataLayout &DL,
                                    SmallVectorImpl<Value *> &Words) {
  // TODO: this function is silly -- BitVector internally has
  // a bitmap storage, but it is private. Can we do better?

  const unsigned PtrSize = DL.getPointerSize();
  Type *Int32Ty = Type::getInt32Ty(T->getContext());
  uint64_t Size = DL.getTypeStoreSize(T);
  BitVector BV(Size/PtrSize);

  getPtrBitmapForTypeHelper(T, DL, 0, BV);

  if (BV.none())
    return;
  unsigned last = BV.find_last();
  if (last == 0) // a single pointer field, no need of a bitmap
    return;
  //Words.reserve(last/32 + 1);
  for (unsigned i = 0; i <= last; i += 32) {
    uint32_t w = 0;
    for (unsigned j = 0; j < 32 && i+j <= last; j++)
      w |= BV[i+j] ? 1<<j : 0;
    Words.push_back(ConstantInt::get(Int32Ty, w));
  }
}
