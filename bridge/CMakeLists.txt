
# Rules for building LLVMCppGoFrontEnd library, which incorporates
# both the Go frontend and the frontend => LLVM-IR bridge component.

set(LLVM_LINK_COMPONENTS
  CodeGen
  Core
  Support
  )

add_llvm_library(LLVMCppGoFrontEnd
  ${GOFRONTEND_SOURCE_DIR}/ast-dump.cc
  ${GOFRONTEND_SOURCE_DIR}/escape.cc
  ${GOFRONTEND_SOURCE_DIR}/export.cc
  ${GOFRONTEND_SOURCE_DIR}/expressions.cc
  ${GOFRONTEND_SOURCE_DIR}/go-diagnostics.cc
  ${GOFRONTEND_SOURCE_DIR}/go-encode-id.cc
  ${GOFRONTEND_SOURCE_DIR}/go-dump.cc
  ${GOFRONTEND_SOURCE_DIR}/go-optimize.cc
  ${GOFRONTEND_SOURCE_DIR}/go.cc
  ${GOFRONTEND_SOURCE_DIR}/gogo.cc
  ${GOFRONTEND_SOURCE_DIR}/import-archive.cc
  ${GOFRONTEND_SOURCE_DIR}/import.cc
  ${GOFRONTEND_SOURCE_DIR}/lex.cc
  ${GOFRONTEND_SOURCE_DIR}/names.cc
  ${GOFRONTEND_SOURCE_DIR}/parse.cc
  ${GOFRONTEND_SOURCE_DIR}/runtime.cc
  ${GOFRONTEND_SOURCE_DIR}/statements.cc
  ${GOFRONTEND_SOURCE_DIR}/types.cc
  ${GOFRONTEND_SOURCE_DIR}/unsafe.cc
  ${GOFRONTEND_SOURCE_DIR}/wb.cc
  go-backend.cpp
  go-llvm-bexpression.cpp
  go-llvm-bfunction.cpp
  go-llvm-bnode.cpp
  go-llvm-bstatement.cpp
  go-llvm-btype.cpp
  go-llvm-builtins.cpp
  go-llvm-bvariable.cpp
  go-llvm-cabi-oracle.cpp
  go-llvm-diagnostics.cpp
  go-llvm-dibuildhelper.cpp
  go-llvm-genblocks.cpp
  go-llvm-irbuilders.cpp
  go-llvm-linemap.cpp
  go-llvm-materialize.cpp
  go-llvm-tree-integrity.cpp
  go-llvm-typemanager.cpp
  go-llvm.cpp
  go-sha1.cpp
)

add_dependencies(LLVMCppGoFrontEnd intrinsics_gen)

include_directories(${MPCINSTALL}/include)
include_directories(${MPFRINSTALL}/include)
include_directories(${GMPINSTALL}/include)
include_directories(${GOFRONTEND_SOURCE_DIR})

