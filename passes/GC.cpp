//===--- GC.cpp -----------------------------------------------------------===//
//
// Copyright 2018 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
//
//===----------------------------------------------------------------------===//
//
// GC strategy for Go runtime.
//
//===----------------------------------------------------------------------===//

#include "GoStackMap.h"
#include "GollvmPasses.h"

#include "llvm/ADT/BitVector.h"
#include "llvm/ADT/Twine.h"
#include "llvm/CodeGen/AsmPrinter.h"
#include "llvm/CodeGen/GCMetadataPrinter.h"
#include "llvm/CodeGen/GCStrategy.h"
#include "llvm/CodeGen/StackMaps.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/MC/MCContext.h"
#include "llvm/MC/MCObjectFileInfo.h"
#include "llvm/MC/MCStreamer.h"
#include "llvm/Support/Casting.h"
#include <vector>

using namespace llvm;

namespace {

class GoGC : public GCStrategy {
public:
  GoGC() {
    UseStatepoints = true;
    UsesMetadata = true; // using GCMetadataPrinter to emit stack maps

    // TODO: write barrier?
  }

  Optional<bool> isGCManagedPointer(const Type *Ty) const override {
    return isa<PointerType>(Ty);
  }
};

GCRegistry::Add<GoGC> X("go", "Go garbage collector.");

class GoGCPrinter : public GCMetadataPrinter {
public:
  bool emitStackMaps(StackMaps &SM, AsmPrinter &AP) override;
};

GCMetadataPrinterRegistry::Add<GoGCPrinter>
  X2("go", "Go garbage collector.");

} // namespace

// Returns the bitmap (in a vector of bytes) encoding live
// locations.
static std::vector<uint8_t>
computeBitVector(uint64_t StackSize,
                 const StackMaps::LocationVec &CSLocs,
                 uint32_t &Size) {
  // TODO: this function is silly -- BitVector internally has
  // a bitmap storage, but it is private. We basically recompute
  // it. Can we do better?

  const int PtrSize = 8; // TODO: get from target info
  BitVector BV(StackSize / PtrSize);
  for (unsigned i = 0, n = CSLocs.size(); i < n; i++) {
    const auto &Loc = CSLocs[i];
    switch (Loc.Type) {
    case StackMaps::Location::Direct:
    case StackMaps::Location::Indirect: {
      // TODO: verify that the base register is SP.

      unsigned Idx = Loc.Offset / PtrSize;

      // If a slot is following a "Constant" location, it is an aggregate type
      // and that constant encodes the pointer bitmap.
      // Constant 0 cannot be a pointer bitmap. Instead, it encodes a null pointer
      // as live (happens with degenerated Phis).
      if (i+1 < n &&
          CSLocs[i+1].Type == StackMaps::Location::Constant &&
          CSLocs[i+1].Offset != 0) {
        // Make sure it is a local/arg slot, not a spill of in-register value.
        assert(Loc.Type == StackMaps::Location::Direct);

        i++;
        for (; i < n && CSLocs[i].Type == StackMaps::Location::Constant; i++) {
          if (Idx + 32 >= BV.size())
            BV.resize(Idx + 32);
          uint32_t Bits = CSLocs[i].Offset;
          for (unsigned j = 0; j < 32; j++)
            if ((Bits>>j)&1)
              BV.set(Idx + j);
          Idx += 32;
        }
        i--; // compensate the increment in outer loop
        break;
      }

      // A single pointer slot.
      if (Idx >= BV.size())
        // This can happen if Loc is an arg slot (for byVal aggregate type).
        BV.resize(Idx + 1);
      BV.set(Idx);
      break;
    }
    default:
      break;
    }
  }

  Size = 0;
  std::vector<uint8_t> Bytes;
  if (BV.none())
    return Bytes;
  int last = BV.find_last();
  Size = last + 1;
  Bytes.reserve(last/8 + 1);
  int i;
  for (i = 0; i <= last; i += 8) {
    uint8_t b = 0;
    for (int j = 0; j < 8 && i+j <= last; j++)
      b |= BV[i+j] ? 1<<j : 0;
    Bytes.push_back(b);
  }
  Bytes.resize(last/8 + 1);
  return Bytes;
}

static void
emitCallsiteEntries(StackMaps &SM, MCStreamer &OS) {
  auto &CSInfos = SM.getCSInfos();
  if (CSInfos.empty())
    return;

  MCContext &OutContext = OS.getContext();
  auto CSI = CSInfos.begin();
  for (auto const &FR : SM.getFnInfos()) {
    for (unsigned i = 0; i < FR.second.RecordCount; i++, CSI++) {
      Twine Name = Twine(GO_STACKMAP_SYM_PREFIX) + Twine((*CSI).ID);
      MCSymbol *Sym = OutContext.getOrCreateSymbol(Name);
      if (Sym->isDefined())
        // We may have emitted one, due to tail duplication.
        continue;
      OS.EmitLabel(Sym);

      // Stack map entry:
      //   uint32_t nbits;
      //   uint8_t *data;
      uint32_t Size;
      std::vector<uint8_t> V =
          computeBitVector(FR.second.StackSize, (*CSI).Locations, Size);
      OS.EmitIntValue(Size, 4);
      for (uint8_t Byte : V)
        OS.EmitIntValue(Byte, 1);
    }
    OS.EmitValueToAlignment(8);
  }
}

bool
GoGCPrinter::emitStackMaps(StackMaps &SM, AsmPrinter &AP) {
  MCContext &OutContext = AP.OutStreamer->getContext();
  MCStreamer &OS = *AP.OutStreamer;

  // Create the section.
  MCSection *StackMapSection =
      OutContext.getObjectFileInfo()->getStackMapSection();
  OS.SwitchSection(StackMapSection);

  emitCallsiteEntries(SM, OS);

  return true;
}

void llvm::linkGoGC() {}
void llvm::linkGoGCPrinter() {}
