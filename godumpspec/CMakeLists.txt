
# Libraries that we need to link into 'llvm-godumpspec'
set(LLVM_LINK_COMPONENTS
  DebugInfoDWARF
  AllTargetsDescs
  AllTargetsInfos
  MC
  Object
  Support
  )

# FIXME: this program is a tool for building libgo; we don't want
# to include it in the "all" build target or 
# want 

# The llvm-godumpspec executable.
add_llvm_tool(llvm-godumpspec
  godumpspec.cpp
  )
