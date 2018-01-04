
# Rules for building llvm-goparse executable.

#----------------------------------------------------------------------

# Libraries that we need to link into llvm-goparse
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
include_directories(${GOFRONTEND_SOURCE_DIR})
include_directories(${BRIDGE_SOURCE_DIR})

# Gofrontend headers use headers from these packages.
include_directories(${MPCINSTALL}/include)
include_directories(${MPFRINSTALL}/include)
include_directories(${GMPINSTALL}/include)

# The llvm-goparse executable itself
add_llvm_tool(llvm-goparse
  goparse-llvm.cpp
  )

# Record the fact that llvm-goparse depends on these libs
add_dependencies(llvm-goparse libmpfr libmpc libgmp)

# Add in the libraries for the llvm-goparse dependencies.
target_link_libraries(llvm-goparse
  "-L${MPCLIBDIR}" "-lmpc"
  "-L${MPFRLIBDIR}" "-lmpfr"
  "-L${GMPLIBDIR}" "-lgmp")

