
# Rules for building llvm-goc executable.

#----------------------------------------------------------------------

# Libraries that we need to link into llvm-goc
set(LLVM_LINK_COMPONENTS
  DriverUtils
  CppGoFrontEnd
  ${LLVM_TARGETS_TO_BUILD}
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
include_directories(${DRIVER_UTILS_SOURCE_DIR})
include_directories("${gollvm_binroot}/driver")

# Gofrontend headers use headers from these packages.
include_directories(${EXTINSTALLDIR}/include)

# The llvm-goc executable itself
add_llvm_tool(llvm-goc
  llvm-goc.cpp)

# Record the fact that llvm-goc depends on these libs
add_dependencies(llvm-goc libmpfr libmpc libgmp)

# Add in the libraries for the llvm-goc dependencies.
target_link_libraries(llvm-goc
  PRIVATE
  "-L${EXTLIBDIR}" "-lmpc" "-lmpfr" "-lgmp"
  )
