
# Copyright 2018 The Go Authors. All rights reserved.
# Use of this source code is governed by a BSD-style
# license that can be found in the LICENSE file.

# Notes:
#
# The code in gofrontend depends on functions from GMP, MPC, and MPFR;
# these libraries are currently handled via the cmake "ExternalProject"
# utility.
#

include(ExternalProject)
include(ProcessorCount)
include(LLVMExternalProjectUtils)

list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake/modules")
set(GOLLVM_USE_SPLIT_STACK ON CACHE BOOL "use split stack by default")
set(GOLLVM_DEFAULT_LINKER gold CACHE STRING "default linker for Go links")

include(CmakeUtils)
include(AddGollvm)

# So that we can issue "make -jN" cmds in externalproject_add
processorcount(PROCESSOR_COUNT)

set(EXTINSTALLDIR ${CMAKE_CURRENT_BINARY_DIR}/external/install)
set(EXTLIBDIR "${EXTINSTALLDIR}/lib")
set(EXTINCLUDEDIR "${EXTINSTALLDIR}/include")
set(EXTCPPFLAGS "CFLAGS=-I${EXTINCLUDEDIR}")
if(NOT "${CMAKE_SYSROOT}" STREQUAL "")
  string(APPEND EXTCPPFLAGS " --sysroot=${CMAKE_SYSROOT}")
endif()
if(LLVM_ENABLE_PIC)
  string(APPEND EXTCPPFLAGS " -fPIC -DPIC")
endif()
set(EXTLDFLAGS "LDFLAGS=-L${EXTLIBDIR}")
set(EXTCC "CC=${CMAKE_C_COMPILER}" "CXX=${CMAKE_CXX_COMPILER}")

set(gollvm_binroot "${CMAKE_CURRENT_BINARY_DIR}")

externalproject_add(libgmp
  URL URL https://gmplib.org/download/gmp/gmp-6.2.0.tar.bz2
  URL_MD5 c24161e0dd44cae78cd5f67193492a21
  DOWNLOAD_DIR ${CMAKE_CURRENT_BINARY_DIR}/external-downloads
  BINARY_DIR ${CMAKE_CURRENT_BINARY_DIR}/external/gmp-build
  SOURCE_DIR ${CMAKE_CURRENT_BINARY_DIR}/external/gmp
  CONFIGURE_COMMAND <SOURCE_DIR>/configure --prefix=${EXTINSTALLDIR} ${EXTCPPFLAGS} ${EXTLDFLAGS} ${EXTCC}
  BUILD_COMMAND make -j${PROCESSOR_COUNT} install
  LOG_CONFIGURE 1
  LOG_BUILD 1
  LOG_INSTALL 1
  )

externalproject_add(libmpfr
  DEPENDS libgmp
  URL https://www.mpfr.org/mpfr-current/mpfr-4.1.0.tar.bz2
  URL_MD5 44b892bc5a45bafb4294d134e13aad1d
  DOWNLOAD_DIR ${CMAKE_CURRENT_BINARY_DIR}/external-downloads
  BINARY_DIR ${CMAKE_CURRENT_BINARY_DIR}/external/mpfr
  SOURCE_DIR ${CMAKE_CURRENT_BINARY_DIR}/external/mpfr
  CONFIGURE_COMMAND <SOURCE_DIR>/configure --with-gmp=${CMAKE_CURRENT_BINARY_DIR}/external/gmp --prefix=${EXTINSTALLDIR} ${EXTCPPFLAGS} ${EXTLDFLAGS} ${EXTCC}
  BUILD_COMMAND make -j${PROCESSOR_COUNT} install
  LOG_CONFIGURE 1
  LOG_BUILD 1
  LOG_INSTALL 1
  )

externalproject_add(libmpc
  DEPENDS libgmp libmpfr
  URL https://ftp.gnu.org/gnu/mpc/mpc-1.2.0.tar.gz
  URL_MD5 2f1ce56ac775f2be090863f364931a03
  DOWNLOAD_DIR ${CMAKE_CURRENT_BINARY_DIR}/external-downloads
  BINARY_DIR ${CMAKE_CURRENT_BINARY_DIR}/external/mpc
  SOURCE_DIR ${CMAKE_CURRENT_BINARY_DIR}/external/mpc
  PREFIX ${EXTINSTALLDIR}
  CONFIGURE_COMMAND <SOURCE_DIR>/configure --with-gmp=${CMAKE_CURRENT_BINARY_DIR}/external/gmp --with-mpfr=${CMAKE_CURRENT_BINARY_DIR}/external/mpfr --prefix=${EXTINSTALLDIR} ${EXTCPPFLAGS} ${EXTLDFLAGS} ${EXTCC}
  BUILD_COMMAND make -j${PROCESSOR_COUNT} install
  LOG_CONFIGURE 1
  LOG_BUILD 1
  LOG_INSTALL 1
  )

# Top level targets for building, installing
add_custom_target(gollvm ALL)
add_custom_target(install-gollvm)

# In most use cases, we want to force a rebuild of all objects built
# from Go source if the compiler changes.
set(gocdep llvm-goc llvm-goc-token)

# For compiler developers, however, the dependence this can be a
# hassle-- when hacking on the compiler, it is annoying to have each
# tiny change force a library rebuild, so the DISABLE_LIBGO_GOC_DEP can
# be set as an (unsafe) escape hatch to break the dependence from golibs
# to compiler. In this case we still need to insure that the compiler
# exists, but we don't care whether it is up to date or not.
if (DISABLE_LIBGO_GOC_DEP)
  set(gocdep llvm-goc-token)
endif()

# Root of gollvm source code.
set(GOLLVM_SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR})

# Location of gofrontend source code.
set(GOFRONTEND_SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/gofrontend/go)

# Location of bridge source code.
set(BRIDGE_SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/bridge)

# Location of gollvm specific passes source code.
set(PASSES_SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/passes)

# Location of driver utilities source code.
set(DRIVER_UTILS_SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/driver)

# Subdirectory for the middle layer that translates Backend method
# calls into LLVM IR.
add_subdirectory(bridge)

# Subdirectory for gollvm specific backend passes.
add_subdirectory(passes)

# Subdirectory for compiler driver utilities library.
add_subdirectory(driver)

# Subdirectory for compiler driver executable.
add_subdirectory(driver-main)

# Go standard library
add_subdirectory(libgo)

# Go tools (go, gofmt, etc)
add_subdirectory(gotools)

# Subdir for unit tests
add_subdirectory(unittests)

# Top-level check target for gollvm
add_custom_target(check-gollvm DEPENDS check-libgo check-gotools)
