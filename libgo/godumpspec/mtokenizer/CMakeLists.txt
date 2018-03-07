
add_llvm_library(GoDumpSpecMacroTokenizer
  macro-tokenizer.cpp

  # This is a library meant only for the build tree.
  BUILDTREE_ONLY
)
