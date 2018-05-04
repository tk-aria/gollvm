
# gollvm

Gollvm is an LLVM-based Go compiler. It incorporates "gofrontend" (a Go language front end written in C++ and shared with GCCGO), a bridge component (which translates from gofrontend IR to LLVM IR), and a driver that sends the resulting IR through the LLVM back end.

Gollvm (name still not finalized) is set up to be a subproject within the LLVM tools directory, similar to how things work for "clang" or "compiler-rt": you check out a copy of the LLVM source tree, then within the LLVM tree you check out additional git repos.

You'll need to have an up-to-date copy of cmake on your system (3.6 vintage).

## Setting up a gollvm work area:

```
// Here 'workarea' will contain a copy of the LLVM source tree and one or more build areas
% mkdir workarea
% cd workarea

// Sources
% git clone http://llvm.org/git/llvm.git
...
% cd llvm/tools
% git clone https://go.googlesource.com/gollvm
...
% cd gollvm
% git clone https://go.googlesource.com/gofrontend
...
% cd libgo
% git clone https://github.com/libffi/libffi.git
...
% git clone https://github.com/ianlancetaylor/libbacktrace.git
...
%

// Create a build directory, and run cmake
% mkdir build.opt
% cd build.opt
% cmake -DCMAKE_BUILD_TYPE=Debug -DLLVM_USE_LINKER=gold -G Ninja ../llvm

// Build
ninja <gollvm target(s)>
```

## Installing gollvm

A gollvm installation will contain 'llvm-goc', the Gollvm compiler, the libgo standard Go libraries, and the standard Go tools ("go", "vet", "cgo", etc).

The installation directory for gollvm needs to be specified when invoking cmake prior to the build:

```
% mkdir build.rel
% cd build.rel
% cmake -DCMAKE_INSTALL_PREFIX=/my/install/dir -DCMAKE_BUILD_TYPE=Release -DLLVM_USE_LINKER=gold -G Ninja ../llvm

// Build all of gollvm
% ninja gollvm
...

// Install gollvm
% ninja install-gollvm

```

## Using an installed copy of gollvm

Programs build with the Gollvm Go compiler default to shared linkage, meaning that they need to pick up the Go runtime library via LD_LIBRARY_PATH:

```

// Root of Gollvm install is /tmp/gollvm-install
% export LD_LIBRARY_PATH=/tmp/gollvm-install/lib64
% export PATH=/tmp/gollvm-install/bin:$PATH
% go run himom.go
hi mom!
%

```

## Source code structure

Within <workarea>/llvm/tools/gollvm, the following directories are of interest:

.../llvm/tools/gollvm:

 * contains rules to build third party libraries needed for gollvm,
   along with common definitions for subdirs.

.../llvm/tools/gollvm/driver,
.../llvm/tools/gollvm/driver-main:

 * contains build rules and source code for llvm-goc

.../llvm/tools/gollvm/gofrontend:

 * source code for gofrontend and libgo (note: no cmake files here)

.../llvm/tools/gollvm/bridge:

 * contains build rules for the libLLVMCppGoFrontEnd.a, a library that contains both the gofrontend code and the LLVM-specific middle layer (for example, the definition of the class Llvm_backend, which inherits from Backend).

.../llvm/tools/gollvm/unittests:

 * source code for the unit tests

## Building and running llvm-goc

The executable llvm-goc is the main compiler driver for gollvm; it functions as a compiler (consuming source for a Go package and producing an object file), an assembler, and/or a linker.  While it is possible to build and run llvm-goc directly from the command line, in practice there is little point in doing this (better to build using "go build", which will invoke llvm-goc on your behalf.

```
// From within <workarea>/build.opt:

% ninja llvm-goc
...
% cat micro.go
package foo
func Bar() int {
	return 1
}
% ./bin/llvm-goc -fgo-pkgpath=foo -O3 -o micro.s micro.go
%
```


## Building and running the unit tests

Here are instructions on building and running the unit tests for the middle layer:

```
// From within <workarea>/build.opt:

// Build unit test
% ninja GoBackendCoreTests

// Run unit test
% ./tools/gollvm/unittests/BackendCore/GoBackendCoreTests
[==========] Running 10 tests from 2 test cases.
[----------] Global test environment set-up.
[----------] 9 tests from BackendCoreTests
[ RUN      ] BackendCoreTests.MakeBackend
[       OK ] BackendCoreTests.MakeBackend (1 ms)
[ RUN      ] BackendCoreTests.ScalarTypes
[       OK ] BackendCoreTests.ScalarTypes (0 ms)
[ RUN      ] BackendCoreTests.StructTypes
[       OK ] BackendCoreTests.StructTypes (1 ms)
[ RUN      ] BackendCoreTests.ComplexTypes
[       OK ] BackendCoreTests.ComplexTypes (0 ms)
[ RUN      ] BackendCoreTests.FunctionTypes
[       OK ] BackendCoreTests.FunctionTypes (0 ms)
[ RUN      ] BackendCoreTests.PlaceholderTypes
[       OK ] BackendCoreTests.PlaceholderTypes (0 ms)
[ RUN      ] BackendCoreTests.ArrayTypes
[       OK ] BackendCoreTests.ArrayTypes (0 ms)
[ RUN      ] BackendCoreTests.NamedTypes
[       OK ] BackendCoreTests.NamedTypes (0 ms)
[ RUN      ] BackendCoreTests.TypeUtils

...

[  PASSED  ] 10 tests.
```

The unit tests currently work by instantiating an LLVM Backend instance and making backend method calls (to mimic what the frontend would do), then inspects the results to make sure they are as expected. Here is an example:

```
TEST(BackendCoreTests, ComplexTypes) {
  LLVMContext C;

  Type *ft = Type::getFloatTy(C);
  Type *dt = Type::getDoubleTy(C);

  std::unique_ptr<Backend> be(go_get_backend(C));
  Btype *c32 = be->complex_type(64);
  ASSERT_TRUE(c32 != NULL);
  ASSERT_EQ(c32->type(), mkTwoFieldLLvmStruct(C, ft, ft));
  Btype *c64 = be->complex_type(128);
  ASSERT_TRUE(c64 != NULL);
  ASSERT_EQ(c64->type(), mkTwoFieldLLvmStruct(C, dt, dt));
}
```

The test above makes sure that the LLVM type we get as a result of calling Backend::complex_type() is kosher and matches up to expectations.

## Building libgo (Go runtime and standard libraries)

To build the Go runtime and standard libraries, use the following:

```
// From within <workarea>/build.opt:

// Build Go runtime and standard libraries
% ninja libgo_all

```

This will compile static (*.a) and dynamic (*.so) versions of the library.


