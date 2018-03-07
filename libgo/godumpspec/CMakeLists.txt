
include(GoVars)
include(ConfigSetup)

# Separate library subdirs here to enable unit testing.
add_subdirectory(mtokenizer)
include_directories("${CMAKE_CURRENT_SOURCE_DIR}/mtokenizer")

# Libraries that we need to link into 'llvm-godumpspec'
set(LLVM_LINK_COMPONENTS
  DebugInfoDWARF
  AllTargetsDescs
  AllTargetsInfos
  MC
  Object
  Support)

# The llvm-godumpspec executable. 
add_llvm_utility(llvm-godumpspec
  godumpspec.cpp)

target_link_libraries(llvm-godumpspec
  PRIVATE
  GoDumpSpecMacroTokenizer)

