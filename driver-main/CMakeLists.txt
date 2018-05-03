
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
add_gollvm_tool(llvm-goc
  llvm-goc.cpp)

# Record the fact that llvm-goc depends on these libs
add_dependencies(llvm-goc libmpfr libmpc libgmp)

# Add in the libraries for the llvm-goc dependencies.
target_link_libraries(llvm-goc
  PRIVATE
  "-L${EXTLIBDIR}" "-lmpc" "-lmpfr" "-lgmp"
  )

# Create a "compiler built" file each time llvm-goc is built.
set(llvm_goc_build ${CMAKE_CURRENT_BINARY_DIR}/llvm-goc.built)
add_custom_command(
  OUTPUT ${llvm_goc_build}
  COMMAND  ${CMAKE_COMMAND} -E touch ${llvm_goc_build}
  DEPENDS llvm-goc 
  COMMENT "Create 'build-performed' file for llvm-goc"
  VERBATIM)

# Copy the build file to the token file if different. The intent here
# is that the token file gets created in the process of building
# llvm-goc, but does NOT get updated each time llvm-goc is rebuilt.
set(llvm_goc_token ${CMAKE_CURRENT_BINARY_DIR}/llvm-goc.token)
copy_if_different(${llvm_goc_build} ${llvm_goc_token})

# Token target.
add_custom_target(llvm-goc-token ALL DEPENDS ${llvm_goc_token})

# For compatibility, add a symlink to llvm-goc named "gccgo" in install dir.
install(CODE "execute_process(COMMAND ${CMAKE_COMMAND} -E create_symlink \"llvm-goc\" \"${CMAKE_INSTALL_PREFIX}/bin/gccgo\")"
  COMPONENT llvm-goc)




    



