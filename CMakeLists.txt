
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
  string(APPEND EXTCPPFLAGS " -fPIC")
endif()
set(EXTLDFLAGS "LDFLAGS=-L${EXTLIBDIR}")
set(EXTCC "CC=${CMAKE_C_COMPILER}" "CXX=${CMAKE_CXX_COMPILER}")

set(gollvm_binroot "${CMAKE_CURRENT_BINARY_DIR}")

externalproject_add(libgmp
  URL ftp://ftp.gnu.org/gnu/gmp/gmp-6.1.0.tar.bz2
  URL_MD5 86ee6e54ebfc4a90b643a65e402c4048
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
  URL ftp://gcc.gnu.org/pub/gcc/infrastructure/mpfr-3.1.4.tar.bz2
  URL_MD5 b8a2f6b0e68bef46e53da2ac439e1cf4
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
  URL ftp://gcc.gnu.org/pub/gcc/infrastructure/mpc-1.0.3.tar.gz
  URL_MD5 d6a1d5f8ddea3abd2cc3e98f58352d26
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

# Location of driver utilities source code.
set(DRIVER_UTILS_SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/driver)

# Subdirectory for the middle layer that translates Backend method
# calls into LLVM IR.
add_subdirectory(bridge)

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

