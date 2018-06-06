
# Rules for building driver utils library

#----------------------------------------------------------------------

set(LLVM_TARGET_DEFINITIONS GollvmOptions.td)
tablegen(LLVM GollvmOptions.inc -gen-opt-parser-defs)
add_public_tablegen_target(GollvmDriverOptions)

configure_file(
    ${CMAKE_CURRENT_SOURCE_DIR}/GollvmConfig.h.cmake
    ${CMAKE_CURRENT_BINARY_DIR}/GollvmConfig.h)

# Include directories needed for this lib.
include_directories(${GOFRONTEND_SOURCE_DIR})
include_directories(${BRIDGE_SOURCE_DIR})

# Gofrontend headers use headers from these packages.
include_directories(${EXTINSTALLDIR}/include)

# A library containing driver utility code.
add_llvm_library(LLVMDriverUtils
  Action.cpp
  Artifact.cpp
  Command.cpp
  Compilation.cpp
  CompileGo.cpp
  Driver.cpp
  GccUtils.cpp
  GnuTools.cpp
  GollvmOptions.cpp
  LinuxToolChain.cpp
  ReadStdin.cpp
  Tool.cpp
  ToolChain.cpp
  DEPENDS
  GollvmDriverOptions
  )

# Add dependencies for these libs because it needs the
# headers to be installed.
add_dependencies(LLVMDriverUtils libmpfr libmpc libgmp)
