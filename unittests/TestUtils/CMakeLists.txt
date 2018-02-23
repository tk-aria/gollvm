
add_llvm_library(GoUnitTestUtils
  DiffUtils.cpp

  # This is a library meant only for the build tree.
  BUILDTREE_ONLY
)


