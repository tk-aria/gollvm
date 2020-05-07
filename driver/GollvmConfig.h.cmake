
// Copyright 2018 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#ifndef GOLLVM_CONFIG_H
#define GOLLVM_CONFIG_H

// Root of gollvm install
#cmakedefine GOLLVM_INSTALL_DIR "@GOLLVM_INSTALL_DIR@"

// Library subdir within install
#cmakedefine GOLLVM_INSTALL_LIBDIR "@GOLLVM_INSTALL_LIBDIR@"

// Library version (e.g. 7, 4.5, 8.0.1)
#cmakedefine GOLLVM_LIBVERSION "@GOLLVM_LIBVERSION@"

// Define if the compiler supports -fsplit-stack
#cmakedefine USING_SPLIT_STACK

// Compiler version. Same as library version currently.
#define GOLLVM_COMPILERVERSION GOLLVM_LIBVERSION

// Gollvm default linker
#cmakedefine GOLLVM_DEFAULT_LINKER "@GOLLVM_DEFAULT_LINKER@"

#endif // GOLLVM_CONFIG_H
