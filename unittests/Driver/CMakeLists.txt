
set(LLVM_LINK_COMPONENTS
  DriverUtils
  CppGoFrontEnd
  CppGoPasses
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
  Support)

set(DriverTestSources
  DriverTests.cpp)

add_gobackend_unittest(DriverTests
  ${DriverTestSources})

set(driver_src_dir "${GOLLVM_SOURCE_DIR}/driver")

include_directories(${unittest_testutils_src})
include_directories(${driver_src_dir})
include_directories("${gollvm_binroot}/driver")

# Record the fact that this unit test depends on these libs
add_dependencies(DriverTests libmpfr libmpc libgmp)

target_link_libraries(DriverTests
  PRIVATE
  GoUnitTestUtils
  "-L${EXTLIBDIR}" "-Wl,--push-state" "-Wl,-Bstatic" "-lmpc" "-lmpfr" "-lgmp" "-Wl,--pop-state"
  )
