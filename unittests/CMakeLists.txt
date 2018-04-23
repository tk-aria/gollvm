
add_custom_target(GoBackendUnitTests)
set_target_properties(GoBackendUnitTests PROPERTIES FOLDER "Tests")

add_subdirectory(TestUtils)
set(unittest_testutils_src "${CMAKE_CURRENT_SOURCE_DIR}/TestUtils")

function(add_gobackend_unittest test_dirname)
  add_unittest(GoBackendUnitTests ${test_dirname} ${ARGN})
endfunction()

add_subdirectory(GoDumpSpec)

if (APPLE)
  set(CMAKE_INSTALL_RPATH "@executable_path/../../lib")
else(UNIX)
  set(CMAKE_INSTALL_RPATH "\$ORIGIN/../../lib${LLVM_LIBDIR_SUFFIX}")
endif()

function(add_gobackend_unittest test_dirname)
  add_unittest(GoBackendUnitTests ${test_dirname} ${ARGN})
endfunction()

include_directories(${EXTINSTALLDIR}/include)
include_directories(${BRIDGE_SOURCE_DIR})
include_directories(${GOFRONTEND_SOURCE_DIR})

add_subdirectory(DriverUtils)
add_subdirectory(BackendCore)
