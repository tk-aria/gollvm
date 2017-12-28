
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

# Open issue: should the configure cmds below be run with
# "CC=${CMAKE_BINARY_DIR}/bin/clang"

set(MPCINSTALL ${CMAKE_CURRENT_BINARY_DIR}/external/build-mpc)
set(MPCLIBDIR "${MPCINSTALL}/lib")
externalproject_add(libmpc
  URL ftp://gcc.gnu.org/pub/gcc/infrastructure/mpc-1.0.3.tar.gz
  URL_MD5 d6a1d5f8ddea3abd2cc3e98f58352d26
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
set(MPFRLIBDIR "${MPFRINSTALL}/lib")
externalproject_add(libmpfr
  URL ftp://gcc.gnu.org/pub/gcc/infrastructure/mpfr-3.1.4.tar.bz2
  URL_MD5 b8a2f6b0e68bef46e53da2ac439e1cf4
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
set(GMPLIBDIR "${GMPINSTALL}/lib")
externalproject_add(libgmp
  URL ftp://ftp.gnu.org/gnu/gmp/gmp-6.1.0.tar.bz2
  URL_MD5 86ee6e54ebfc4a90b643a65e402c4048
  DOWNLOAD_DIR ${CMAKE_CURRENT_BINARY_DIR}/external-downloads
  BINARY_DIR ${CMAKE_CURRENT_BINARY_DIR}/external/gmp
  CONFIGURE_COMMAND <SOURCE_DIR>/configure --prefix=${GMPINSTALL} ${MCONF1} ${MCONF2}
  BUILD_COMMAND make -j${PROCESSOR_COUNT}
  INSTALL_COMMAND make -j${PROCESSOR_COUNT} install
  LOG_CONFIGURE 1
  LOG_BUILD 1
  LOG_INSTALL 1
  )

# Subdir for gofrontend itself and for the middle layer that translates
# Backend method calls into LLVM IR.
add_subdirectory(llvm-gofrontend)

# Subdir for unit tests that test the go middle layer
add_subdirectory(unittests)

# Things we need to link into llvm-goparse
set(LLVM_LINK_COMPONENTS
  ${LLVM_TARGETS_TO_BUILD}
  CppGoFrontEnd
  CodeGen
  Core
  IRReader
  MC
  Support
  Target
  Object
  Passes
  Support
  )

# Include directories needed by llvm-goparse
include_directories(${CMAKE_CURRENT_SOURCE_DIR}/llvm-gofrontend/gofrontend/go)
include_directories(${CMAKE_CURRENT_SOURCE_DIR}/llvm-gofrontend)

# For gofrontend headers
include_directories(${MPCINSTALL}/include)
include_directories(${MPFRINSTALL}/include)
include_directories(${GMPINSTALL}/include)

# The llvm-goparse executable itself
add_llvm_tool(llvm-goparse
  goparse-llvm.cpp
  )

# Record the fact that llvm-goparse depends on these libs
add_dependencies(llvm-goparse libmpfr libmpc libgmp)

# Add in llvm-goparse dependencies.
target_link_libraries(llvm-goparse
  "-L${MPCLIBDIR}" "-lmpc"
  "-L${MPFRLIBDIR}" "-lmpfr"
  "-L${GMPLIBDIR}" "-lgmp")


