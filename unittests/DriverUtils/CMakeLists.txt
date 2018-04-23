
set(LLVM_LINK_COMPONENTS
  DriverUtils
  CodeGen
  Core
  Option
  Support)

set(DriverUtilsTestSources
  DriverUtilsTests.cpp)

add_gobackend_unittest(DriverUtilsTests
  ${DriverUtilsTestSources})

set(driver_src_dir "${GOLLVM_SOURCE_DIR}/driver")

include_directories(${unittest_testutils_src})
include_directories(${driver_src_dir})
include_directories("${gollvm_binroot}/driver")

target_link_libraries(DriverUtilsTests
  PRIVATE
  GoUnitTestUtils)

