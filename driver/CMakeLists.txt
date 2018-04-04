
# Rules for building llvm-goc executable.

#----------------------------------------------------------------------

set(LLVM_TARGET_DEFINITIONS GollvmOptions.td)
tablegen(LLVM GollvmOptions.inc -gen-opt-parser-defs)
add_public_tablegen_target(GollvmDriverOptions)

# Libraries that we need to link into llvm-goc
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
  Option
  Passes
  Support
  )

# Include directories needed by llvm-goc
include_directories(${GOFRONTEND_SOURCE_DIR})
include_directories(${BRIDGE_SOURCE_DIR})

# Gofrontend headers use headers from these packages.
include_directories(${EXTINSTALLDIR}/include)

# The llvm-goc executable itself
add_llvm_tool(llvm-goc
  llvm-goc.cpp
  GollvmOptions.cpp
  DEPENDS
  GollvmDriverOptions
  )

# Record the fact that llvm-goc depends on these libs
add_dependencies(llvm-goc libmpfr libmpc libgmp)

# Add in the libraries for the llvm-goc dependencies.
target_link_libraries(llvm-goc
  PRIVATE
  "-L${EXTLIBDIR}" "-lmpc" "-lmpfr" "-lgmp"
  )
