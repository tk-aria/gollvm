
include(GoVars)
include(ConfigSetup)

# Separate library subdirs here to enable unit testing.
add_subdirectory(mtokenizer)
include_directories("${CMAKE_CURRENT_SOURCE_DIR}/mtokenizer")
add_subdirectory(mparser)

# Create macro temp files. These are throwaways for the time being.
set(macrotmpscript "${CMAKE_CURRENT_SOURCE_DIR}/macgen.sh")
set(macrotmpgnu "${CMAKE_CURRENT_BINARY_DIR}/macros.gnu.txt")
set(macrotmpclang "${CMAKE_CURRENT_BINARY_DIR}/macros.clang.txt")
set(macrotmpinput "${GOLLVM_SOURCE_DIR}/gofrontend/libgo/sysinfo.c")
set(sysinfognudoto "${CMAKE_CURRENT_BINARY_DIR}/sysinfo.gnu.o")
set(sysinfoclangdoto "${CMAKE_CURRENT_BINARY_DIR}/sysinfo.gnu.o")
set(sysinfognudoti "${CMAKE_CURRENT_BINARY_DIR}/sysinfo.gnu.i")
set(sysinfoclangdoti "${CMAKE_CURRENT_BINARY_DIR}/sysinfo.clang.i")

execute_process(COMMAND
  "${shell}" "${macrotmpscript}" "g++"
  ${macrotmpinput} ${macrotmpgnu} ${sysinfognudoto} ${sysinfognudoti}
  "-fno-eliminate-unused-debug-types" "-g3" "-c"
  "-I${libgo_binroot}"  "-I${libgo_binroot}/runtime"
  OUTPUT_VARIABLE result
  ERROR_VARIABLE errmsg
  RESULT_VARIABLE exitstatus)
if(NOT ${exitstatus} MATCHES 0)
  message(FATAL_ERROR "${macrotmpscript} invocation failed: ${errmsg}")
endif()

execute_process(COMMAND
  "${shell}" "${macrotmpscript}" "clang-3.9"
  ${macrotmpinput} ${macrotmpclang} ${sysinfoclangdoto} ${sysinfoclangdoti}
  "-fno-eliminate-unused-debug-types" "-g3"
  "-I${libgo_binroot}"  "-I${libgo_binroot}/runtime"
  OUTPUT_VARIABLE result
  ERROR_VARIABLE errmsg
  RESULT_VARIABLE exitstatus)
if(NOT ${exitstatus} MATCHES 0)
  message(FATAL_ERROR "${macrotmpscript} invocation failed: ${errmsg}")
endif()

# Libraries that we need to link into 'llvm-godumpspec'
set(LLVM_LINK_COMPONENTS
  DebugInfoDWARF
  AllTargetsDescs
  AllTargetsInfos
  MC
  Object
  Support
  )

include_directories("${CMAKE_CURRENT_SOURCE_DIR}/mparser")

# FIXME: this program is a tool for building libgo; we don't want
# to include it in the "all" build target nor do we want to install it.

# The llvm-godumpspec executable.
add_llvm_tool(llvm-godumpspec
  godumpspec.cpp
  )

target_link_libraries(llvm-godumpspec
  PRIVATE
  GoDumpSpecMacroParser
  GoDumpSpecMacroTokenizer)




