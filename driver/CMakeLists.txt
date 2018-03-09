
# Rules for building llvm-goc executable.

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

# The llvm-goc executable itself
add_llvm_tool(llvm-goc
  llvm-goc.cpp
  )

# Record the fact that llvm-goc depends on these libs
add_dependencies(llvm-goc libmpfr libmpc libgmp)

# Add in the libraries for the llvm-goc dependencies.
target_link_libraries(llvm-goc
  PRIVATE
  "-L${MPCLIBDIR}" "-lmpc"
  "-L${MPFRLIBDIR}" "-lmpfr"
  "-L${GMPLIBDIR}" "-lgmp"
  )
