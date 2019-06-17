// Copyright 2019 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#include <stddef.h>

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
