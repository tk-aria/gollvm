
# Library subdir within installation.
# FIXME: select 32/64 based on default target triple
set(libsubdir "lib64")

# Set version number string (used by install rules)
set(libversion ${LLVM_VERSION_MAJOR})
if (NOT ${LLVM_VERSION_MINOR} EQUAL 0 AND NOT ${LLVM_VERSION_PATCH} EQUAL 0)
  set(libversion "${libversion}.${LLVM_VERSION_MINOR}")
endif()
if (NOT ${LLVM_VERSION_PATCH} EQUAL 0)
  set(libversion "${libversion}.${LLVM_VERSION_PATCH}")
endif()

# These are incorporated into GollvmConfig.h
set(GOLLVM_LIBVERSION "${libversion}")
set(GOLLVM_INSTALL_DIR "${CMAKE_INSTALL_PREFIX}")
set(GOLLVM_INSTALL_LIBDIR "${CMAKE_INSTALL_PREFIX}/${libsubdir}")

macro(add_gollvm_library name)
  llvm_add_library(${name} ${ARGN})

  # Configure for install.
  install(TARGETS ${name}
    COMPONENT ${name}
    LIBRARY DESTINATION ${libsubdir}
    ARCHIVE DESTINATION ${libsubdir}
    RUNTIME DESTINATION bin)

  # Add an install target.
  add_custom_target(install-${name}
    DEPENDS ${name}
    COMMAND "${CMAKE_COMMAND}"
    -DCMAKE_INSTALL_COMPONENT=${name}
    -P "${CMAKE_BINARY_DIR}/cmake_install.cmake")
  add_dependencies(install-gollvm install-${name})

  set_target_properties(${name} PROPERTIES FOLDER "Gollvm libraries")
endmacro(add_gollvm_library)

macro(add_gollvm_executable name)
  add_llvm_executable(${name} ${ARGN} )
  set_target_properties(${name} PROPERTIES FOLDER "Gollvm executables")
endmacro(add_gollvm_executable)

macro(add_gollvm_tool name)

  add_gollvm_executable(${name} ${ARGN})

  # Configure for install.
  install(TARGETS ${name}
    COMPONENT ${name}
    LIBRARY DESTINATION ${libsubdir}
    ARCHIVE DESTINATION ${libsubdir}
    RUNTIME DESTINATION bin)

  # Add an install target.
  add_custom_target(install-${name}
    DEPENDS ${name}
    COMMAND "${CMAKE_COMMAND}"
    -DCMAKE_INSTALL_COMPONENT=${name}
    -P "${CMAKE_BINARY_DIR}/cmake_install.cmake")
  add_dependencies(install-gollvm install-${name})

  set_target_properties(${name} PROPERTIES FOLDER "Gollvm tools")
endmacro()
