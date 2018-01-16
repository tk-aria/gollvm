
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

include_directories(${MPCINSTALL}/include)
include_directories(${MPFRINSTALL}/include)
include_directories(${GMPINSTALL}/include)

# Record the fact that llvm-goparse depends on these libs
add_dependencies(GoBackendCoreTests libmpfr libmpc libgmp)

# Add in the archives for the llvm-goparse dependencies.
target_link_libraries(GoBackendCoreTests
  PRIVATE
  "-L${MPCLIBDIR}" "-lmpc"
  "-L${MPFRLIBDIR}" "-lmpfr"
  "-L${GMPLIBDIR}" "-lgmp"
  )
