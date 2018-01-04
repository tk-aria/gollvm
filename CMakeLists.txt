
# Notes:
#
# The code in gofrontend depends on functions from GMP, MPC, and MPFR;
# these libraries are currently handled via the cmake "ExternalProject"
# utility.
#

include(ExternalProject)
include(ProcessorCount)
include(LLVMExternalProjectUtils)

# So that we can issue "make -jN" cmds in externalproject_add
processorcount(PROCESSOR_COUNT)

# FIXME: something less hacky for MacOS
if(APPLE)
set(MCONF1 "CPPFLAGS=-I/usr/local/include")
set(MCONF2 "LDFLAGS=-L/usr/local/lib")
endif()

# FIXME: add in URL args for the projects below.
# URL ftp://ftp.gnu.org/gnu/gmp/gmp-6.1.0.tar.bz2
# URL ftp://gcc.gnu.org/pub/gcc/infrastructure/mpfr-3.1.4.tar.bz2
# URL ftp://gcc.gnu.org/pub/gcc/infrastructure/mpc-1.0.3.tar.gz

# Open issue: should the configure cmds below be run with
# "CC=${CMAKE_BINARY_DIR}/bin/clang"

set(MPCINSTALL ${CMAKE_CURRENT_BINARY_DIR}/external/build-mpc)
externalproject_add(libmpc
  URL ftp://gcc.gnu.org/pub/gcc/infrastructure/mpc-1.0.3.tar.gz
  DOWNLOAD_DIR ${CMAKE_CURRENT_BINARY_DIR}/external-downloads
  BINARY_DIR ${CMAKE_CURRENT_BINARY_DIR}/external/mpc
  CONFIGURE_COMMAND <SOURCE_DIR>/configure --prefix=${MPCINSTALL} ${MCONF1} ${MCONF2}
  BUILD_COMMAND make -j${PROCESSOR_COUNT}
  INSTALL_COMMAND make -j${PROCESSOR_COUNT} install
  LOG_CONFIGURE 1
  LOG_BUILD 1
  LOG_INSTALL 1
  )

set(MPFRINSTALL ${CMAKE_CURRENT_BINARY_DIR}/external/build-mpfr)
externalproject_add(libmpfr
  URL ftp://gcc.gnu.org/pub/gcc/infrastructure/mpfr-3.1.4.tar.bz2
  DOWNLOAD_DIR ${CMAKE_CURRENT_BINARY_DIR}/external-downloads
  BINARY_DIR ${CMAKE_CURRENT_BINARY_DIR}/external/mpfr
  CONFIGURE_COMMAND <SOURCE_DIR>/configure --prefix=${MPFRINSTALL} ${MCONF1} ${MCONF2}
  BUILD_COMMAND make -j${PROCESSOR_COUNT}
  INSTALL_COMMAND make -j${PROCESSOR_COUNT} install
  LOG_CONFIGURE 1
  LOG_BUILD 1
  LOG_INSTALL 1
  )

set(GMPINSTALL ${CMAKE_CURRENT_BINARY_DIR}/external/build-gmp)
externalproject_add(libgmp
  URL ftp://ftp.gnu.org/gnu/gmp/gmp-6.1.0.tar.bz2
  DOWNLOAD_DIR ${CMAKE_CURRENT_BINARY_DIR}/external-downloads
  BINARY_DIR ${CMAKE_CURRENT_BINARY_DIR}/external/gmp
  CONFIGURE_COMMAND <SOURCE_DIR>/configure --prefix=${GMPINSTALL} ${MCONF1} ${MCONF2}
  BUILD_COMMAND make -j${PROCESSOR_COUNT}
  INSTALL_COMMAND make -j${PROCESSOR_COUNT} install
  LOG_CONFIGURE 1
  LOG_BUILD 1
  LOG_INSTALL 1
  )

# Location of gofrontend source code.
set(GOFRONTEND_SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/gofrontend/go)

# Location of bridge source code.
set(BRIDGE_SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/bridge)

# Subdirectory for the middle layer that translates Backend method
# calls into LLVM IR.
add_subdirectory(bridge)

# Subdirectory for llvm-goparse executable.
add_subdirectory(driver)

# Subdir for unit tests that test the go middle layer
add_subdirectory(unittests)

