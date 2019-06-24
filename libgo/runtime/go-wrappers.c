// Copyright 2019 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// Libc function wrappers built with split-stack. So the
// linker won't patch the caller to force a stack split.

#include <stddef.h>
#include "unwind.h"

void *__go_memmove(void*, const void*, size_t) __attribute__((no_split_stack));
void *__go_memmove(void *dst, const void *src, size_t n)
{
	return __builtin_memmove(dst, src, n);
}

void *__go_memcpy(void*, const void*, size_t) __attribute__((no_split_stack));
void *__go_memcpy(void *dst, const void *src, size_t n)
{
	return __builtin_memcpy(dst, src, n);
}

void *__go_memset(void*, int, size_t) __attribute__((no_split_stack));
void *__go_memset(void *dst, int c, size_t n)
{
	return __builtin_memset(dst, c, n);
}

int __go_memcmp(const void*, const void*, size_t) __attribute__((no_split_stack));
int __go_memcmp(const void *a, const void *b, size_t n)
{
	return __builtin_memcmp(a, b, n);
}

// Not no_split_stack -- _Unwind_Resume does use some stack.
// This wrapper is useful in that it forces a split in here, not in
// the caller. So the forced split happens only when _Unwind_Resume
// is actually called.
void __go_Unwind_Resume (struct _Unwind_Exception *e)
{
	_Unwind_Resume(e);
}
