
set(LLVM_LINK_COMPONENTS
  CodeGen
  Core
  Support)

set(GodumpSpecTestSources
  ParserTests.cpp
  TokenizerTests.cpp)

add_gobackend_unittest(GoDumpSpecTests
  ${GodumpSpecTestSources})

set(tokenizer_src_dir "${GOLLVM_SOURCE_DIR}/libgo/godumpspec/mtokenizer")
set(parser_src_dir "${GOLLVM_SOURCE_DIR}/libgo/godumpspec/mparser")

include_directories(${unittest_testutils_src})
include_directories(${tokenizer_src_dir})
include_directories(${parser_src_dir})

target_link_libraries(GoDumpSpecTests
  PRIVATE
  GoDumpSpecMacroParser
  GoDumpSpecMacroTokenizer
  GoUnitTestUtils)

