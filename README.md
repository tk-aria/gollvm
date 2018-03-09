
# gollvm

Gollvm is an LLVM-based Go compiler. It incorporates "gofrontend" (a Go language front end written in C++ and shared with GCCGO), a bridge components (which translates from gofrontend IR to LLVM IR), and a driver that sends the resulting IR through the LLVM back end. Gollvm is still at an early stage of development.

Gollvm (name still not finalized) is set up to be a subprojects within the LLVM tools directory, similar to how things work for "clang" or "compiler-rt": you check out a copy of the LLVM source tree and then within that tree you check out additional git repos.

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

// Create a build directory and run cmake
% mkdir build.opt
% cd build.opt
% cmake -DCMAKE_BUILD_TYPE=Debug -DLLVM_USE_LINKER=gold -G Ninja ../llvm

// Build
ninja <gollvm target(s)>
```

## Source code structure

Within <workarea>/llvm/tools/gollvm, the following directories are of interest:

.../llvm/tools/gollvm:

 * contains rules to build third party libraries needed for gollvm,
   along with common definitions for subdirs.

.../llvm/tools/gollvm/driver:

 * contains build rules and source code for llvm-goc

.../llvm/tools/gollvm/gofrontend:

 * source code for gofrontend and libgo (note: no cmake files here)

.../llvm/tools/gollvm/bridge:

 * contains build rules for the libLLVMCppGoFrontEnd.a, a library that contains both the gofrontend code and the LLVM-specific middle layer (for example, the definition of the class Llvm_backend, which inherits from Backend).

.../llvm/tools/gollvm/unittests:

 * source code for the unit tests

## Building and running llvm-goc

The executable llvm-goc is the main compiler driver for gollvm; it functions as a compiler (consuming source for a Go package and producing an object file) and will eventually include any necessary invocations of the linker or assembler. This program not fully usable on its own at the moment; using it requries a companion gccgo installation.

```
// From within <workarea>/build.opt:

% ninja llvm-goc
% cat micro.go
package foo
func Bar() int {
	return 1
}
% ./bin/llvm-goc -fgo-pkgpath=foo -O3 -o micro.s micro.go
%
```

## Using llvm-goc in combination with a GCCGO installation

At the moment the CMake build/install support for libgo is not entirely on line, which makes it difficult/unwieldy to use for running actual Go programs. As an interim workaround, I've written a shim/wrapper script that allows you to use llvm-goc in combination with an existing GCCGO installation, using gccgo for the runtime/libraries and the linking step, but llvm-goc for any compilation.

The wrapper script can be found in the tools/ subdir. To use it, build a copy of GCCGO and run "make install" to copy the bits into an install directory. From the GCCGO install directory, you can insert the wrapper by running it with the "--install" option:

```
 % cd /my/gccgo/install
 % /my/gollvm/sandbox/tools/gollvm-wrap.py --install
 executing: mv bin/gccgo bin/gccgo.real
 executing: chmod 0755 bin/gccgo
 executing: cp /my/gollvm/sandbox/tools/gollvm-wrap.py bin
 executing: cp /my/gollvm/sandbox/tools/script_utils.py bin
 wrapper installed successfully
 %

```

At this point you can now run "go build", "go run", etc using GCCGO -- the compilation steps will be performed by llvm-goc, and the remainder (linking, incorporation of runtime) will be done by gccgo. Example:

```
% cd $GOPATH/src/himom
% go run himom.go
hi mom!
% go run -compiler gccgo himom.go
hi mom!
% GOLLVM_WRAP_OPTIONS=-t go run -compiler gccgo himom.go
# command-line-arguments
+ llvm-goc -I $WORK -c -g -m64 -fgo-relative-import-path=_/mygopath/src/himom -o $WORK/command-line-arguments/_obj/_go_.o.s ./himom.go -L /my/gccgo/install/lib64/go/8.0.0/x86_64-pc-linux-gnu
hi mom
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
