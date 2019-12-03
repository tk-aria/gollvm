
# Library subdir within installation.
# FIXME: select 32/64 based on default target triple
# NOTE: default target triple has not been set at this point,
#       we remain libsubdir unchanged with arm64 linux configuration
#       for now, so libgo libraries and go*begin.a are put
#       under 'lib64' as well, further fix required
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

# We need to check if '-fsplit-stack' is supported with 'USING_SPLIT_STACK'
# at compile time. So define this macro in GollvmConfig.h if it's supported.
if(GOLLVM_USE_SPLIT_STACK)
  # For amd64, as gcc, clang, ld and ld.gold all support -fsplit-stack, so all
  # going well. For arm64, the situation is very complicated, none of gcc, ld
  # and ld.gold support this option, but clang does. When using clang compiler
  # and ld linker, the test passes, but in fact ld does not support stack
  # splitting. So here we do this test with ld.gold linker.
  # FIXME: update here once one day there is a linker that supports '-fsplit-stack'
  # on arm64.
  SET(CMAKE_REQUIRED_FLAGS "-fuse-ld=gold -fsplit-stack")
  check_c_source_compiles("#include<stdio.h>\nint main(){printf(\"hello\");\nreturn 0;}" C_SUPPORTS_SPLIT_STACK)
  if(NOT C_SUPPORTS_SPLIT_STACK)
    message(SEND_ERROR "C compiler does not support -fsplit-stack")
  else()
    set(USING_SPLIT_STACK 1)
  endif()
endif()

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
