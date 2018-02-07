
set(LLVM_LINK_COMPONENTS
  CodeGen
  Core
  Support
  )

set(GodumpSpecTestSources
  TokenizerTests.cpp
  )

add_gobackend_unittest(GoDumpSpecTests
  ${GodumpSpecTestSources}
  )

set(tokenizer_src_dir "${GOLLVM_SOURCE_DIR}/libgo/godumpspec/mtokenizer")

include_directories(${unittest_testutils_src})
include_directories(${tokenizer_src_dir})

target_link_libraries(GoDumpSpecTests
  PRIVATE
  GoDumpSpecMacroTokenizer
  GoUnitTestUtils
)

