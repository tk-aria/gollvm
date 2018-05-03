
#ifndef GOLLVM_CONFIG_H
#define GOLLVM_CONFIG_H

// Root of gollvm install
#cmakedefine GOLLVM_INSTALL_DIR "@GOLLVM_INSTALL_DIR@"

// Library subdir within install
#cmakedefine GOLLVM_INSTALL_LIBDIR "@GOLLVM_INSTALL_LIBDIR@"

// Library version (e.g. 7, 4.5, 8.0.1)
#cmakedefine GOLLVM_LIBVERSION "@GOLLVM_LIBVERSION@"

// Compiler version. Same as library version currently.
#define GOLLVM_COMPILERVERSION GOLLVM_LIBVERSION

#endif // GOLLVM_CONFIG_H
