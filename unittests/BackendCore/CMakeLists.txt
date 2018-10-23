
set(LLVM_LINK_COMPONENTS
  CppGoFrontEnd
  CodeGen
  Core
  Support
  )

set(BackendCoreSources
  BackendCoreTests.cpp
  BackendArrayStruct.cpp
  BackendCABIOracleTests.cpp
  BackendExprTests.cpp
  BackendPointerExprTests.cpp
  BackendFcnTests.cpp
  BackendCallTests.cpp
  BackendDebugEmit.cpp
  BackendStmtTests.cpp
  BackendVarTests.cpp
  BackendTreeIntegrity.cpp
  BackendNodeTests.cpp
  LinemapTests.cpp
  Sha1Tests.cpp
  TestUtilsTest.cpp
  TestUtils.cpp
  )

add_gobackend_unittest(GoBackendCoreTests
  ${BackendCoreSources}
  )

include_directories(${unittest_testutils_src})
include_directories(${MPCINSTALL}/include)
include_directories(${MPFRINSTALL}/include)
include_directories(${GMPINSTALL}/include)

# Record the fact that this unit test depends on these libs
add_dependencies(GoBackendCoreTests libmpfr libmpc libgmp)

# Add in the archives for the unit test dependencies.
target_link_libraries(GoBackendCoreTests
  PRIVATE
  GoUnitTestUtils
  "-L${EXTLIBDIR}" "-Wl,--push-state" "-Wl,-Bstatic" "-lmpc" "-lmpfr" "-lgmp" "-Wl,--pop-state"
  )
