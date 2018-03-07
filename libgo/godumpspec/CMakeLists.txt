
include(GoVars)
include(ConfigSetup)

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
  PRIVATE)

