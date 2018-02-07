
add_llvm_library(GoDumpSpecMacroParser
  macro-parser.cpp

  # This is a library meant only for the build tree.
  BUILDTREE_ONLY
)
