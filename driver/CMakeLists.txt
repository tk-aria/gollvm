
# Rules for building driver utils library

#----------------------------------------------------------------------

set(LLVM_TARGET_DEFINITIONS GollvmOptions.td)
tablegen(LLVM GollvmOptions.inc -gen-opt-parser-defs)
add_public_tablegen_target(GollvmDriverOptions)

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
  Tool.cpp
  ToolChain.cpp
  DEPENDS
  GollvmDriverOptions
  )

