
add_custom_target(GoBackendUnitTests)
set_target_properties(GoBackendUnitTests PROPERTIES FOLDER "Tests")

if (APPLE)
  set(CMAKE_INSTALL_RPATH "@executable_path/../../lib")
else(UNIX)
  set(CMAKE_INSTALL_RPATH "\$ORIGIN/../../lib${LLVM_LIBDIR_SUFFIX}")
endif()

function(add_gobackend_unittest test_dirname)
  add_unittest(GoBackendUnitTests ${test_dirname} ${ARGN})
endfunction()

include_directories(${CMAKE_CURRENT_SOURCE_DIR}/../llvm-gofrontend)
include_directories(${CMAKE_CURRENT_SOURCE_DIR}/../llvm-gofrontend/gofrontend/go)

include_directories(${MPCINSTALL}/include)
include_directories(${MPFRINSTALL}/include)
include_directories(${GMPINSTALL}/include)

add_subdirectory(BackendCore)
