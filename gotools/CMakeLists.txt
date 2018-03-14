
# Gotools build requires libgo; if libgo is stubbed out then don't
# try to build gotools either.

if(DISABLE_LIBGO_BUILD)
  return()
endif()

# Pick up any extra Go compiler flags specified via
# "cmake -DGOLLVM_EXTRA_GOCFLAGS=..."
set(gotools_extra_gocflags ${GOLLVM_EXTRA_GOCFLAGS})

include(GoProgram)

message(STATUS "starting gotools configuration.")

include(GoVars)

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
      GOCFLAGS ${gotools_extra_gocflags}
      GODEP libgotool libgo_shared libgo_static)
    list(APPEND allgotools ${tool_target})
  endif()
endforeach()

add_custom_target(gotools_all DEPENDS ${allgotools})

# FIXME: add install rules

message(STATUS "gotools configuration complete.")
