
# Gotools build requires libgo; if libgo is stubbed out then don't
# try to build gotools either.

if(DISABLE_LIBGO_BUILD)
  return()
endif()

include(GoProgram)
include(GoVars)

message(STATUS "starting gotools configuration.")

# Pick up any extra Go compiler flags specified via
# "cmake -DGOLLVM_EXTRA_GOCFLAGS=..."
set(gotools_extra_gocflags ${GOLLVM_EXTRA_GOCFLAGS})

# Driver for compiling *.go files.
get_target_property(driverdir llvm-goc RUNTIME_OUTPUT_DIRECTORY)
set(gollvm_driver "${driverdir}/llvm-goc")
set(gocompiler ${gollvm_driver})

set(cmd_srcroot "${GOLLVM_SOURCE_DIR}/gofrontend/libgo/go/cmd")
set(libgo_srcroot "${GOLLVM_SOURCE_DIR}/gofrontend/libgo")

set(gotools_binroot "${CMAKE_CURRENT_BINARY_DIR}")
set(libgo_binroot "${gollvm_binroot}/libgo")

set(libgotool_archive "${libgo_binroot}/libgotool.a")

# Cgo needs a copy of the defaultCC function in the "main" package.
set(cgozdefaultccdotgo "${gotools_binroot}/zdefaultcc.go")
set(cgozdefaultcctmp "${gotools_binroot}/zdefaultcc.go.tmp")
mkzdefaultcc("main" ${cgozdefaultcctmp}
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

    set(isubdir "tools")
    if("${tool}" STREQUAL "go")
      set(isubdir "bin")
    endif()

    # Create target for program.
    add_go_program(${tool} ${tool_target}
      ${libgo_binroot} ${gotools_binroot}
      GOSRC ${toolfiles}
      GOLIB ${libgotool_archive}
      GOCFLAGS ${gotools_extra_gocflags}
      ISUBDIR ${isubdir}
      GODEP libgotool libgo_shared libgo_static libgobegin)
    list(APPEND allgotools ${tool_target})
  endif()
endforeach()

add_custom_target(gotools_all DEPENDS ${allgotools})
add_dependencies(gollvm gotools_all)

message(STATUS "gotools: generating check targets")

# Emit a small script that will be used to invoke the Go compiler
# In order to get the right permissions on this script we
# use a file copy command. The cmake file copy command allows you
# to specify only the target directory (not target file name), so
# as a hack, emit the original file into the parent binary dir and then
# copy down into the current binary dir.

# FIXME: one of the go package tests (TestPackageMainTestCompilerFlags)
# fails unless the driver name containg "gccgo".
set(rungoc "${gotools_binroot}/run-gccgo")
set(rungoctmp "${gotools_binroot}/../run-gccgo")
file(REMOVE ${rungoctmp})
file(WRITE ${rungoctmp} "#!/bin/sh\n")
file(APPEND ${rungoctmp} "exec ${gocompiler} \"$@\"")
file(APPEND ${rungoctmp} " -I ${libgo_binroot}")
file(APPEND ${rungoctmp} " -L ${libgo_binroot}")
file(APPEND ${rungoctmp} " ${gotools_extra_gocflags}\n")

# Copy with correct perms.
file(COPY ${rungoctmp}
  DESTINATION ${gotools_binroot}
  FILE_PERMISSIONS OWNER_WRITE OWNER_EXECUTE OWNER_READ
                   WORLD_EXECUTE WORLD_READ)

# Emit a small script that will be used to invoke the C compiler
# In order to get the right permissions on this script we
# use a file copy command (see notes above about permissions).
set(runcc "${gotools_binroot}/runcc")
set(runcctmp "${gotools_binroot}/../runcc")
file(REMOVE ${runcctmp})
file(WRITE ${runcctmp} "#!/bin/sh\n")
file(APPEND ${runcctmp} "exec ${CMAKE_C_COMPILER} ")
file(APPEND ${runcctmp} " -I ${libgo_binroot}")
file(APPEND ${runcctmp} " -L ${libgo_binroot} \"$@\"\n")

# Copy with correct perms.
file(COPY ${runcctmp}
  DESTINATION ${gotools_binroot}
  FILE_PERMISSIONS OWNER_WRITE OWNER_EXECUTE OWNER_READ
                   WORLD_EXECUTE WORLD_READ)

# Script to help with 'go test ...' runs
set(runner "${CMAKE_CURRENT_SOURCE_DIR}/gotestprogram.sh")

# List containing all the check targets.
set(checktargets)

# Timeouts for check targets. The cmd/go test is large and complex; needs
# a longer timeout than the other targets.
set(cmdgo_check_timeout 500)
set(default_check_timeout 250)

# This test target runs `go test cmd/go` within the build environment.
set(targetname "check_go_tool")
add_custom_target(
  ${targetname}
  COMMAND "${shell}" ${runner}
    "WORKDIR" "check-go-dir"
    "SUBDIR" "src/cmd/go"
    "LOGFILE" "${gotools_binroot}/cmd_go-testlog"
    "SETENV" "GOPATH=${gotools_binroot}/check-go-dir"
    "COPYGODIRS" "${cmd_srcroot}/go:src/cmd/go"
    "COPYDIRS"
       "${cmd_srcroot}/go/internal:src/cmd/go"
       "${cmd_srcroot}/internal:src/cmd"
       "${cmd_srcroot}/go/testdata:src/cmd/go"
    "COPYFILES"
       "${libgo_binroot}/zdefaultcc.go:src/cmd/go/internal/cfg/"
       "${libgo_binroot}/objabi.go:src/cmd/internal/objabi"
    "TIMEOUT" ${cmdgo_check_timeout}
    "GOC" "${rungoc}"
    "BINDIR" ${gotools_binroot}
    "LIBDIR" ${libgo_binroot}
  DEPENDS ${libgo_goxfiles} libgotool libgo_shared gotools_all libgobegin libgolibbegin
    ${libgo_binroot}/zstdpkglist.go ${libgo_binroot}/zdefaultcc.go
  COMMENT "Checking cmd/go tool"
  VERBATIM)
list(APPEND checktargets ${targetname})

# This test target runs `go test cmd/vet` within the build environment.
set(targetname "check_vet_tool")
add_custom_target(
  ${targetname}
  COMMAND "${shell}" ${runner}
    "WORKDIR" "check-vet-dir"
    "SUBDIR" "src/cmd/vet"
    "LOGFILE" "${gotools_binroot}/cmd_vet-testlog"
    "SETENV" "GOPATH=${gotools_binroot}/check-vet-dir"
    "COPYDIRS"
        "${cmd_srcroot}/vet:src/cmd"
        "${cmd_srcroot}/internal/objabi:src/cmd/internal"
    "COPYFILES" "${libgo_binroot}/objabi.go:src/cmd/internal/objabi"
    "TIMEOUT" ${default_check_timeout}
    "GOC" "${rungoc}"
    "BINDIR" ${gotools_binroot}
    "LIBDIR" ${libgo_binroot}
  DEPENDS ${libgo_goxfiles} libgotool libgo_shared gotools_all libgobegin
  COMMENT "Checking cmd/vet tool"
  VERBATIM)
list(APPEND checktargets ${targetname})

# This test target runs `go test misc/cgo/test` within the build environment.
set(targetname "check_cgo_tool")
add_custom_target(
  ${targetname}
  COMMAND "${shell}" ${runner}
    "WORKDIR" "check-cgo-dir"
    "SUBDIR" "misc/cgo/test"
    "LOGFILE" "${gotools_binroot}/cmd_cgo-testlog"
    "COPYDIRS" "${libgo_srcroot}/misc/cgo/test:misc/cgo"
    "TIMEOUT" ${default_check_timeout}
    "GOC" "${rungoc}"
    "SETENV" "GOTRACEBACK=2"
    "BINDIR" ${gotools_binroot}
    "LIBDIR" ${libgo_binroot}
  DEPENDS ${libgo_goxfiles} libgotool libgo_shared gotools_all libgobegin
  COMMENT "Checking cmd/cgo tool"
  VERBATIM)
list(APPEND checktargets ${targetname})

# Run `go test misc/cgo/testcarchive/carchive_test.go` in the build env.
set(targetname "check_carchive_tool")
add_custom_target(
  ${targetname}
  COMMAND "${shell}" ${runner}
    "WORKDIR" "check-carchive-dir"
    "SUBDIR" "misc/cgo/testcarchive"
    "LOGFILE" "${gotools_binroot}/cmd_carchive-testlog"
    "COPYDIRS" "${libgo_srcroot}/misc/cgo/testcarchive:misc/cgo"
    "TIMEOUT" ${default_check_timeout}
    "GOC" "${rungoc}"
    "CC" "${runcc}"
    "TESTARG" "carchive_test.go"
    "BINDIR" ${gotools_binroot}
    "LIBDIR" ${libgo_binroot}
  DEPENDS ${libgo_goxfiles} libgotool libgo_shared gotools_all libgobegin libgolibbegin
  COMMENT "Checking cmd/carchive tool"
  VERBATIM)
list(APPEND checktargets ${targetname})

# Finally, kick off the runtime package test using the 'go' tool
# from the build area.
set(gotestrunner "${GOLLVM_SOURCE_DIR}/libgo/checkpackage.sh")
file(STRINGS "${libgo_binroot}/runtime.gofiles" runtimefiles)
separate_arguments(runtimefiles)

set(targetname "check_runtime_with_gotool")
add_custom_target(
  ${targetname}
  COMMAND "${shell}" ${gotestrunner}
  "PACKAGE" "runtime"
  "FILES" ${runtimefiles}
  "GOOS" ${goos}
  "GOARCH" ${goarch}
  "ADDTOPATH" ${gotools_binroot}
  "GC" ${rungoc} -fgo-compiling-runtime
  "GOLIBS" ${packlibs} ${extralibs}
  "BINDIR" "${libgo_binroot}"
  "BASEDIR" "${libgo_srcroot}"
  DEPENDS libgotool libgo_shared libgo_static gotools_all libgobegin
  COMMENT "Checking runtime package with 'go' tool"
  VERBATIM)
list(APPEND checktargets ${targetname})

add_custom_target(check-gotools DEPENDS ${checktargets})

message(STATUS "gotools configuration complete.")
