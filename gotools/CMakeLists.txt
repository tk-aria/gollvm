
# Gotools build requires libgo; if libgo is stubbed out then don't
# try to build gotools either.

if(DISABLE_LIBGO_BUILD)
  return()
endif()

include(GoProgram)

message(STATUS "starting gotools configuration.")

# FIXME: common this up with code in libgo

# FIXME: write code to insure that this is set and that the shell
# in question behaves properly.
set(shell $ENV{SHELL})

# Set goarch/goos based on what we see in the LLVM triple.
string(REGEX REPLACE "-" " " lht_components ${LLVM_DEFAULT_TARGET_TRIPLE})
separate_arguments(lht_components)
list(GET lht_components 0 goarch)
list(GET lht_components 2 goos)

# LLVM's "x86_64" is the same as Go's "amd64".
if( ${goarch} STREQUAL "x86_64" )
  set(goarch "amd64")
endif()

# FIXME: rewrite this to locate llvm-goparse in a more official way
# set(driver "${bin}/../bin/llvm-goparse")
get_target_property(driverdir llvm-goparse RUNTIME_OUTPUT_DIRECTORY)
set(gollvm_driver "${driverdir}/llvm-goparse")
#set(gocompiler ${gollvm_driver})

# FIXME: still need to use wrapper for compilation, since
# llvm-goparse functionality not yet complete.
set(gocompiler "gccgo")

set(cmd_srcroot "${GOLLVM_SOURCE_DIR}/gofrontend/libgo/go/cmd")

set(gotools_binroot "${CMAKE_CURRENT_BINARY_DIR}")
set(libgo_binroot "${CMAKE_CURRENT_BINARY_DIR}/../libgo")

set(libgotool_archive "${libgo_binroot}/libgotool.a")

# Cgo needs a copy of the defaultCC function.
set(cgozdefaultccdotgo "${gotools_binroot}/zdefaultcc.go")
set(cgozdefaultcctmp "${gotools_binroot}/zdefaultcc.go.tmp")
mkzdefaultcc("main" ${cgozdefaultcctmp} ${gollvm_driver}
             ${CMAKE_C_COMPILER} ${CMAKE_CXX_COMPILER})
copy_if_different(${cgozdefaultcctmp} ${cgozdefaultccdotgo})

set(libgo_scriptroot "${GOLLVM_SOURCE_DIR}/gofrontend/libgo")
set(matchdotsh "${libgo_scriptroot}/match.sh")

set(cgo_extra_go_files "${cgozdefaultccdotgo}")

# Loop over each of the tools of interest.
set(tools "go" "gofmt" "cgo" "vet" "buildid" "test2json")
set(allgotools)
foreach(tool ${tools})

  # Check for tool dir
  if(NOT EXISTS "${cmd_srcroot}/${tool}")
    message(SEND_ERROR "Go tool directory ${tool} does not exist.")
  else()
    set(tool_target "gotools_cmd_${tool}")

    # Invoke match.sh to collect Go files of interest for this
    # tool, via shell script. Read result into variable.
    execute_process(COMMAND "${shell}" "${matchdotsh}"
      "--goarch=${goarch}" "--goos=${goos}"
      "--srcdir=${cmd_srcroot}/${tool}"
      OUTPUT_VARIABLE toolfiles
      ERROR_VARIABLE errmsg
      RESULT_VARIABLE exitstatus)
    if(NOT ${exitstatus} MATCHES 0)
      message(FATAL_ERROR "match.sh invocation failed: ${errmsg}")
    endif()
    string(STRIP ${toolfiles} toolfiles)
    separate_arguments(toolfiles)

    # Incorporate extras.
    if(NOT "${${tool}_extra_go_files}" STREQUAL "")
      list(APPEND toolfiles "${${tool}_extra_go_files}")
    endif()

    # Create target for program.
    add_go_program(${tool} ${tool_target}
      ${libgo_binroot} ${gotools_binroot}
      GOSRC ${toolfiles}
      GOLIB ${libgotool_archive}
      GODEP libgotool libgo_shared)
    list(APPEND allgotools ${tool_target})
  endif()
endforeach()

add_custom_target(gotools_all DEPENDS ${allgotools})

# FIXME: add install rules

message(STATUS "gotools configuration complete.")
