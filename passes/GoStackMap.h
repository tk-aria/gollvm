//===- GoStatepoints.h - --------------------------------------------------===//
//
// Copyright 2018 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_GOLLVM_PASSES_GOSTACKMAP_H
#define LLVM_GOLLVM_PASSES_GOSTACKMAP_H

// Special symbols used for stack maps.

#define GO_FUNC_SYM            "go..func"
#define GO_STACKMAP_SYM_PREFIX "go..stackmap."

// A sentinel value that will be inserted to the exception table
// to indicate this is a Go function. The value is known to the
// runtime.
#define GO_FUNC_SENTINEL ((uint64_t)'G' | ((uint64_t)'O'<<8) | \
                          ((uint64_t)'.'<<16) | ((uint64_t)'.'<<24) | \
                          ((uint64_t)'F'<<32) | ((uint64_t)'U'<<40) | \
                          ((uint64_t)'N'<<48) | ((uint64_t)'C'<<56))

#endif
