
# Building libgo is time-consuming. Allow developers to stub it out
# via cmake flag if desired.

if(DISABLE_LIBGO_BUILD)
  return()
endif()

message(STATUS "starting libgo configuration.")

include(GoVars)
include(AutoGenGo)
include(ConfigSetup)
include(GenDeps)
include(GoPackage)
include(StructConfigUtils)
include(LibbacktraceUtils)
include(LibffiUtils)

# Root of libgo sources.
set(libgo_srcroot "${GOLLVM_SOURCE_DIR}/gofrontend/libgo")

# Directory from which we're going to pull libgo Go source code.
set(libgo_gosrcroot "${libgo_srcroot}/go")

# Directory from which we're going to pull libgo C source code.
set(libgo_csrcroot "${libgo_srcroot}")

# Directory from which we're going to pull libgo helper scripts (ex: match.sh)
set(libgo_scriptroot ${libgo_srcroot})

# Libbacktrace source code.
set(libbacktrace_srcroot "${CMAKE_CURRENT_SOURCE_DIR}/libbacktrace")

# Libffi source code.
set(libffi_srcroot "${CMAKE_CURRENT_SOURCE_DIR}/libffi")

# Binary root (top level of libgo build).
set(libgo_binroot "${CMAKE_CURRENT_BINARY_DIR}")

#........................................................................
#
# Call a helper to set up libbacktrace. This also creates the libbacktrace
# targets (libbacktrace_nonpiclib, libbacktrace_piclib).
setup_libbacktrace()

# Call a helper to set up libffi. This also creates the libffi
# targets (libffi_nonpiclib, libffi_piclib).
setup_libffi(${libffi_srcroot})

# Base set of defines for building C code
set(basedefines "-D_GNU_SOURCE" "-D_LARGEFILE_SOURCE" "-D_FILE_OFFSET_BITS=64")

# Set up various config.h files. Copy the LLVM-generated config.h and
# related files (the libgo config.h will include the llvm config.h).
file(MAKE_DIRECTORY "${libgo_binroot}/llvm/Config")
set(llvmconfigdir "${LLVM_BINARY_DIR}/include/llvm/Config")
file(COPY "${llvmconfigdir}/config.h" DESTINATION "llvm/Config")
file(COPY "${llvmconfigdir}/llvm-config.h" DESTINATION "llvm/Config")

# Generate config.h, included by various C files in libgo/go/runtime
# and also used to emit gen-sysinfo.go.
file(MAKE_DIRECTORY "${libgo_binroot}/runtime")
set(runtimeconfigdoth ${libgo_binroot}/runtime/config.h)
configure_file(
  ${CMAKE_CURRENT_SOURCE_DIR}/config.h.cmake
  ${runtimeconfigdoth})

# Subdirectory for godumpspec tool.
add_subdirectory(godumpspec)

#......................................................................

# Driver for compiling *.go files.
get_target_property(driverdir llvm-goc RUNTIME_OUTPUT_DIRECTORY)
set(gollvm_driver "${driverdir}/llvm-goc")
set(gocompiler ${gollvm_driver})

# Pick up any extra Go compiler flags specified via
# "cmake -DGOLLVM_EXTRA_GOCFLAGS=..."
set(tmp_libgo_extra_gocflags ${GOLLVM_EXTRA_GOCFLAGS})
if(GOLLVM_EXTRA_GOCFLAGS)
  string(REPLACE " " ";" libgo_extra_gocflags ${tmp_libgo_extra_gocflags})
endif()
if(NOT GOLLVM_USE_SPLIT_STACK)
  list(APPEND libgo_extra_gocflags "-fno-split-stack")
endif()

# Read in list of all libgo packages
file(STRINGS "${libgo_srcroot}/libgo-packages.txt" libpackages)
file(STRINGS "${libgo_srcroot}/gotool-packages.txt" toolpackages)
list(APPEND allpackages ${libpackages})
list(APPEND allpackages ${toolpackages})

# Script used to collect the Go source files that make up a package.
set(matchdotsh "${libgo_scriptroot}/match.sh")

# Set <pkg>_matchargs for packages that need additional match.sh args.
set(runtime_matchargs "--tag=libffi")

# Certain packages need extra Go source files. The convention here is
# that <pkg>_extra_go_files holds the additional sources for <pkg>.
set(runtime_extra_go_files "runtime_sysinfo.go" "sigtab.go")
set(cmd_internal_objabi_extra_go_files "objabi.go")
set(internal_goroot_extra_go_files "zstdpkglist.go")
set(internal_cpu_extra_go_files "cpugen.go")
set(golang_org_x_sys_cpu_extra_go_files "gcpugen.go")
set(cmd_go_internal_cfg_extra_go_files "zdefaultcc.go")
set(runtime_internal_sys_extra_go_files "version.go")
set(go_types_extra_go_files "gccgosizes.go")
set(syscall_extra_go_files "libcalls.go" "sysinfo.go" "syscall_arch.go")
if(${goos} STREQUAL "linux")
  list(APPEND syscall_extra_go_files "epoll.go")
endif()

# Collect the source files in each package, write to temp file.
foreach( pack ${allpackages})

  # The package directory should exist-- issue an error if it does not.
  if(NOT EXISTS ${libgo_gosrcroot}/${pack})
    message(SEND_ERROR "Package directory ${pack} does not exist.")
  else()

    string(REPLACE "/" "_" ptarget2 "${pack}")
    string(REPLACE "." "_" ptarget "${ptarget2}")
    set(packfilestmp "${libgo_binroot}/${ptarget}.gofiles")

    # Package-specific match args
    set(margs "${${ptarget}_matchargs}")

    # Invoke match.sh to collect Go files of interest for this
    # package, via shell script.
    execute_process(COMMAND "${shell}" "${matchdotsh}" ${margs}
      "--goarch=${goarch}" "--goos=${goos}"
      "--srcdir=${libgo_gosrcroot}/${pack}"
      OUTPUT_VARIABLE packfiles)
    string(STRIP ${packfiles} packfiles)

    # Incorporate any extra source files. These generated files are
    # expected to appear in the root of the bin dir.
    set(extrasrcs "${${ptarget}_extra_go_files}")
    foreach( esrc ${extrasrcs})
      string(STRIP ${esrc} esrcf)
      string(APPEND packfiles " ${libgo_binroot}/${esrcf}")
    endforeach()

    file(WRITE ${packfilestmp} "${packfiles}\n")
  endif()
endforeach()

#........................................................................

# This macro determines the final set of Go source files and Go compiler
# flags for a given package. Outputs are returned in the following
# variables:
#
#    basepacksrcs      Base Go source files (no autogenfiles)
#    packsrcs          All Go source files (including autogenfiles)
#    packopts          GOCFLAGS to use when building package
#
macro(collect_package_inputs pack)
  set(basepacksrcs)
  set(packsrcs)
  set(packopts)

  string(REPLACE "/" "_" ptarget2 "${pack}")
  string(REPLACE "." "_" ptarget "${ptarget2}")
  set(packfilestmp "${libgo_binroot}/${ptarget}.gofiles")
  file(STRINGS ${packfilestmp} matchoutput)
  separate_arguments(matchoutput)

  # Canonicalize paths. Not strictly needed, but makes output nicer.
  foreach( packsrc ${matchoutput})
    string(STRIP ${packsrc} spack)
    get_filename_component(canonsrc "${spack}" REALPATH)
    list(APPEND packsrcs "${canonsrc}")
    get_filename_component(canondir "${canonsrc}" DIRECTORY)
    if(NOT ${canondir} STREQUAL ${libgo_binroot})
      list(APPEND basepacksrcs "${canonsrc}")
    endif()
  endforeach()

  # Collect any package-specific Go command line flags
  set(packopts "${${ptarget}_gocflags}")
endmacro()

#........................................................................

# Certain packages need special compiler options. The convention here
# <pkg>_gocflags holds the command line options needed for <pkg>.

# FIXME: when 386 comes on line, use correct 386-specific options here.

set(math_gocflags "-ffp-contract=off" "-fno-math-errno" "-fno-trapping-math")
set(math_check_gocflags "-ffp-contract=off" "-fno-math-errno" "-fno-trapping-math")

set(runtime_gocflags "-fgo-c-header=runtime.inc.raw" "-fgo-compiling-runtime")
set(runtime_check_gocflags "-fgo-compiling-runtime")

set(runtime_internal_atomic_gocflags "-fgo-compiling-runtime")
set(runtime_internal_atomic_check_gocflags "-fgo-compiling-runtime")

set(runtime_internal_sys_gocflags "-fgo-compiling-runtime")
set(runtime_internal_sys_check_gocflags "-fgo-compiling-runtime")

set(runtime_pprof_check_gocflags "-fno-inline" "-static-libgo")

#........................................................................

# Rules for version.go
set(versiondotgo "${libgo_binroot}/version.go")
set(versiontmp "${libgo_binroot}/version.go.tmp")
mkversion(${goos} ${goarch} ${versiontmp} ${libgo_binroot}
          ${libgo_gosrcroot} ${libgo_scriptroot})
copy_if_different(${versiontmp} ${versiondotgo})

# Rules for gccgosizes.go
set(gccgosizesdotgo "${libgo_binroot}/gccgosizes.go")
set(gccgosizestmp "${libgo_binroot}/gccgosizes.go.tmp")
mkgccgosizes(${goarch} ${gccgosizestmp} ${libgo_scriptroot})
copy_if_different(${gccgosizestmp} ${gccgosizesdotgo})

# Rules for objabi.go
set(objabidotgo "${libgo_binroot}/objabi.go")
set(objabitmp "${libgo_binroot}/objabi.go.tmp")
mkobjabi(${objabitmp} ${libgo_binroot} ${libgo_gosrcroot})
copy_if_different(${objabitmp} ${objabidotgo})

# Rules for zstdpkglist.go
set(zstdpkglistdotgo "${libgo_binroot}/zstdpkglist.go")
set(zstdpkglisttmp "${libgo_binroot}/zstdpkglist.go.tmp")
mkzstdpkglist("goroot" ${zstdpkglisttmp} "${libpackages}")
copy_if_different(${zstdpkglisttmp} ${zstdpkglistdotgo})

# Rules for zdefaultcc.go
set(zdefaultccdotgo "${libgo_binroot}/zdefaultcc.go")
set(zdefaultcctmp "${libgo_binroot}/zdefaultcc.go.tmp")
mkzdefaultcc("cfg" ${zdefaultcctmp}
             ${CMAKE_C_COMPILER} ${CMAKE_CXX_COMPILER} EXPORT)
copy_if_different(${zdefaultcctmp} ${zdefaultccdotgo})

# Rules for gen-sysinfo.go
set(gensysinfodotgo "${libgo_binroot}/gen-sysinfo.go")
set(gensysinfotmp "${libgo_binroot}/gen-sysinfo.go.tmp")
set(gensysinfomacrotmp "${libgo_binroot}/sysinfo.macros.txt")
set(gensysinfoobject "${libgo_binroot}/sysinfo.o")
get_target_property(godumpspecdir llvm-godumpspec RUNTIME_OUTPUT_DIRECTORY)
set(godumpspecexec "${godumpspecdir}/llvm-godumpspec")
set(sysinfoc "${libgo_srcroot}/sysinfo.c")
set(sysinfoflags ${basedefines})
list(APPEND sysinfoflags "-I${libgo_binroot}" "-I${libgo_binroot}/runtime")
mkgensysinfo(${gensysinfotmp} ${gensysinfodotgo}
  ${gensysinfomacrotmp} ${gensysinfoobject} ${godumpspecexec} ${sysinfoc}
  CFLAGS ${sysinfoflags}
  DEPS llvm-godumpspec ${runtimeconfigdoth})

# Command to create runtime_sysinfo.go, via shell script.
set(mkrsysinfosh "${libgo_scriptroot}/mkrsysinfo.sh")
set(rsysinfodotgo "${libgo_binroot}/runtime_sysinfo.go")
generate_go_from_script(${rsysinfodotgo} ${mkrsysinfosh}
  ${goos} ${goarch} ${libgo_binroot}
  DEP ${gensysinfodotgo})

# Command to create sigtab.go, via shell script
set(sigtabdotgo "${libgo_binroot}/sigtab.go")
set(mksigtabsh "${libgo_scriptroot}/mksigtab.sh")
generate_go_from_script(${sigtabdotgo} ${mksigtabsh}
                        ${goos} ${goarch} ${libgo_binroot}
                        CAPTURE DEP ${gensysinfodotgo})

# Generation of libcalls.go
set(libcallsdotgo "${libgo_binroot}/libcalls.go")
set(libcallstmp "${libgo_binroot}/tmp-libcalls.go")
set(awkfile "${libgo_gosrcroot}/syscall/mksyscall.awk")
set(mklibcallssh "${CMAKE_CURRENT_SOURCE_DIR}/mklibcalls.sh")
collect_package_inputs("syscall")
set(syscallgofiles "${libgo_binroot}/syscall.basefiles")
string(REPLACE ";" " " basepacksrcs "${basepacksrcs}")
file(WRITE "${syscallgofiles}" "${basepacksrcs}\n")
generate_go_from_script(${libcallsdotgo} ${mklibcallssh} ${goos} ${goarch}
                        ${libgo_binroot} DEP ${awkfile} ${syscallgofiles}
                        SCRIPTARGS ${awk} ${awkfile} ${syscallgofiles}
			${libcallstmp})

# Generated file errno.i
set(errnoi "${libgo_binroot}/errno.i")
set(errnoitmp "${libgo_binroot}/tmp-errno.i")
set(mkerrnoish "${CMAKE_CURRENT_SOURCE_DIR}/mkerrnoi.sh")
set(cflags ${CMAKE_C_FLAGS})
if(NOT "${CMAKE_SYSROOT}" STREQUAL "")
  set(cflags "--sysroot=${CMAKE_SYSROOT}")
endif()
generate_go_from_script(${errnoi} ${mkerrnoish} ${goos} ${goarch}
                        ${libgo_binroot} SCRIPTARGS "${CMAKE_C_COMPILER}"
                        ${errnoitmp} ${cflags})

# Generated file sysinfo.go
set(sysinfodotgo "${libgo_binroot}/sysinfo.go")
set(mksysinfosh "${libgo_scriptroot}/mksysinfo.sh")
generate_go_from_script(${sysinfodotgo} ${mksysinfosh} ${goos} ${goarch}
                        ${libgo_binroot} DEP ${gensysinfodotgo} ${errnoi})

# Generated file syscall_arch.go
set(syscallarchdotgo "${libgo_binroot}/syscall_arch.go")
set(syscallarchtmp "${libgo_binroot}/syscall_arch.go.tmp")
mksyscallarch(${syscallarchtmp} ${goos} ${goarch})
copy_if_different(${syscallarchtmp} ${syscallarchdotgo})

# Compute epoll size/offset info
compute_struct_size_at_compile_time(SIZEOF_STRUCT_EPOLL_EVENT
  "struct epoll_event"
  "sys/epoll.h"
  "cmake-epoll-tmpfile.cpp")
compute_field_offset_at_compile_time(STRUCT_EPOLL_EVENT_FD_OFFSET
  "struct epoll_event"
  "data.fd"
  "stddef.h;sys/epoll.h"
  "cmake-epoll-tmpfile.cpp")

# Generated file epoll.go
set(epolldotgo "${libgo_binroot}/epoll.go")
set(epolltmp "${libgo_binroot}/epoll.go.tmp")
mkepoll(${epolltmp})
copy_if_different(${epolltmp} ${epolldotgo})

# Generated file cpugen.go
set(cpugendotgo "${libgo_binroot}/cpugen.go")
set(cpugentmp "${libgo_binroot}/cpugen.go.tmp")
mkcpugen(${goarch} ${cpugentmp} ${libgo_scriptroot})
copy_if_different(${cpugentmp} ${cpugendotgo})

# Generated file gcpugen.go
set(gcpugendotgo "${libgo_binroot}/gcpugen.go")
set(gcpugentmp "${libgo_binroot}/gcpugen.go.tmp")
mkgcpugen(${goarch} ${gcpugentmp} ${libgo_scriptroot})
copy_if_different(${gcpugentmp} ${gcpugendotgo})

#........................................................................

message(STATUS "Libgo: creating stdlib package targets")

set(libgo_go_picobjects)
set(libgo_go_nonpicobjects)
set(libgo_goxfiles)
set(libgotool_nonpicobjects)

# Process each package
foreach( pack ${allpackages})
  string(REPLACE "/" "_" ptarget2 "${pack}")
  string(REPLACE "." "_" ptarget "${ptarget2}")

  collect_package_inputs(${pack})

  # Generate dependencies.
  set(packdeps)
  godeps(${pack} ${libgo_scriptroot} SOURCES ${basepacksrcs})

  # If this is a gotool package, we don't need a pic version
  set(nopic)
  list(FIND toolpackages ${pack} found)
  if(NOT ${found} EQUAL -1)
    set(nopic "NOPIC")
  endif()

  # Call into helper to create rules for package.
  add_go_package("${pack}" "${libgo_binroot}" GOSRC ${packsrcs} GODEP ${packdeps} GOCFLAGS ${packopts} ${libgo_extra_gocflags} ${nopic})

  # Accumulate libgo objects.
  if(${found} EQUAL -1)
    list(APPEND libgo_go_picobjects ${package_picofile})
    list(APPEND libgo_go_nonpicobjects ${package_ofile})
    list(APPEND libgo_goxfiles ${package_goxfile})
  else()
    list(APPEND libgotool_nonpicobjects ${package_ofile})
  endif()
endforeach()

# Create object library for libgotool
add_library(libgotool STATIC EXCLUDE_FROM_ALL ${libgotool_nonpicobjects})
set_target_properties(libgotool PROPERTIES ARCHIVE_OUTPUT_DIRECTORY ${libgo_binroot})
set_target_properties(libgotool PROPERTIES OUTPUT_NAME "gotool")
set_target_properties(libgotool PROPERTIES LINKER_LANGUAGE C)

# This file contains the list of packages for which we want to run tests.
file(STRINGS "${libgo_srcroot}/check-packages.txt" checkpackages)

#......................................................................
#
# C portion of libgo

# Generate new runtime.inc from runtime.inc.raw if anything has changed.
set(runtimeinc "${libgo_binroot}/runtime.inc")
set(runtimeincraw "${libgo_binroot}/runtime.inc.raw")
set(runtimeinctmp "${libgo_binroot}/runtime.inc.tmp")
set(runtimeincgen "${libgo_binroot}/tmp-runtime.inc")
set(mkruntimeincsh "${libgo_srcroot}/mkruntimeinc.sh")
generate_go_from_script(${runtimeinc} ${mkruntimeincsh} "" "" ${libgo_binroot} SCRIPTARGS ${runtimeincraw} ${runtimeinctmp} ${runtimeincgen} DEP "libgo_runtime")

# Create a target for runtime.inc so that things can depend on it
add_custom_target(runtimeinctarg DEPENDS ${runtimeinc})

# Base C files from the runtime dir.
set(runtimecfiles
  "runtime/aeshash.c"
  "runtime/go-assert.c"
  "runtime/go-caller.c"
  "runtime/go-callers.c"
  "runtime/go-cdiv.c"
  "runtime/go-cgo.c"
  "runtime/go-construct-map.c"
  "runtime/go-ffi.c"
  "runtime/go-fieldtrack.c"
  "runtime/go-matherr.c"
  "runtime/go-memclr.c"
  "runtime/go-memequal.c"
  "runtime/go-nanotime.c"
  "runtime/go-now.c"
  "runtime/go-nosys.c"
  "runtime/go-reflect-call.c"
  "runtime/go-setenv.c"
  "runtime/go-signal.c"
  "runtime/go-unsafe-pointer.c"
  "runtime/go-unsetenv.c"
  "runtime/go-unwind.c"
  "runtime/go-varargs.c"
  "runtime/env_posix.c"
  "runtime/panic.c"
  "runtime/print.c"
  "runtime/proc.c"
  "runtime/runtime_c.c"
  "runtime/stack.c"
  "runtime/yield.c"
  "runtime/go-context.S")

# C files that happen to be living in other packages.
list(APPEND runtimecfiles
  "go/internal/bytealg/bytealg.c"
  "go/log/syslog/syslog_c.c"
  "go/reflect/makefunc_ffi_c.c"
  "go/runtime/internal/atomic/atomic.c"
  "go/internal/cpu/cpu_gccgo.c"
  "go/sync/atomic/atomic.c"
  "go/syscall/errno.c"
  "go/syscall/signame.c"
  "go/syscall/wait.c")

if (NOT ${goarch} STREQUAL "arm64")
  list(APPEND runtimecfiles
    "go/golang.org/x/sys/cpu/cpu_gccgo_x86.c")
endif()

# Linux-specific C files.
if(${goos} STREQUAL "linux")
  list(APPEND runtimecfiles
    "go/syscall/clone_linux.c")
endif()

# Form full paths.
set(runtimecpaths)
foreach(cfile ${runtimecfiles})
  list(APPEND runtimecpaths "${libgo_csrcroot}/${cfile}")
endforeach()

# go-wrapper.c is not in gofrontend/libgo
list(APPEND runtimecpaths "${GOLLVM_SOURCE_DIR}/libgo/runtime/go-wrappers.c")

# Compiler flags for C files in the runtime.
set(baseopts "-g -Wno-zero-length-array ")
if(GOLLVM_USE_SPLIT_STACK)
  string(APPEND baseopts "-fsplit-stack ")
endif()
foreach(def ${basedefines})
  string(APPEND baseopts "${def} ")
endforeach()
string(APPEND baseopts "${GOLLVM_EXTRA_CFLAGS} ")

# Special flags required for aeshash.c (functions in this file are called
# only when running on the proper architecture, so it is safe to ask for
# specific architectural features).
if (${goarch} STREQUAL "arm64")
  set_source_files_properties(${libgo_csrcroot}/runtime/aeshash.c PROPERTIES COMPILE_FLAGS "-march=armv8-a+crypto")
else()
  set_source_files_properties(${libgo_csrcroot}/runtime/aeshash.c PROPERTIES COMPILE_FLAGS "-maes -mssse3")
endif()

# Object library based on libgo C code, PIC-compiled
add_library(libgo_c_piclib OBJECT EXCLUDE_FROM_ALL ${runtimecpaths})
set_target_properties(libgo_c_piclib PROPERTIES COMPILE_FLAGS "-fPIC ${baseopts}")
target_include_directories(libgo_c_piclib PUBLIC
  "${libgo_csrcroot}/runtime"
  "${libgo_binroot}/runtime"
  ${libbacktrace_srcroot})
add_dependencies(libgo_c_piclib runtimeinctarg)

# Library with non-PIC-compiled objects from libgo C code
add_library(libgo_c_nonpiclib OBJECT EXCLUDE_FROM_ALL ${runtimecpaths})
set_target_properties(libgo_c_nonpiclib PROPERTIES COMPILE_FLAGS "-fno-PIC ${baseopts}")
target_include_directories(libgo_c_nonpiclib PUBLIC
  "${libgo_csrcroot}/runtime"
  "${libgo_binroot}/runtime"
  ${libbacktrace_srcroot})
add_dependencies(libgo_c_nonpiclib runtimeinctarg)

# Static libgo -- combines non-pic C objects and non-pic Go objects
add_gollvm_library(libgo_static STATIC
  $<TARGET_OBJECTS:libgo_c_nonpiclib>
  ${libgo_go_nonpicobjects}
  $<TARGET_OBJECTS:libbacktrace_nonpiclib>
  $<TARGET_OBJECTS:libffi_nonpiclib>)
set_target_properties(libgo_static PROPERTIES OUTPUT_NAME "go")
set_target_properties(libgo_static PROPERTIES
  ARCHIVE_OUTPUT_DIRECTORY ${libgo_binroot})

# HACK: undo LLVM default here.
string(REPLACE "-Wl,-z,defs" "" CMAKE_SHARED_LINKER_FLAGS
    "${CMAKE_SHARED_LINKER_FLAGS}")

# Shared libgo -- combines pic C objects and non-pic Go objects.
add_gollvm_library(libgo_shared SHARED
  $<TARGET_OBJECTS:libgo_c_piclib>
  ${libgo_go_picobjects}
  $<TARGET_OBJECTS:libbacktrace_piclib>
  $<TARGET_OBJECTS:libffi_piclib>)
set_target_properties(libgo_shared PROPERTIES
  LIBRARY_OUTPUT_DIRECTORY ${libgo_binroot})
set_target_properties(libgo_shared PROPERTIES OUTPUT_NAME "go")
set(linkopts "-lpthread;-lm")
if(GOLLVM_USE_SPLIT_STACK)
  string(APPEND linkopts ";-fsplit-stack")
endif()
target_link_libraries(libgo_shared PUBLIC "${linkopts}")

# Sources for libgobegin.a
set(libgobegincfiles
  "${libgo_csrcroot}/runtime/go-main.c")
# libgobegin.a (static library only)
add_gollvm_library(libgobegin STATIC
  ${libgobegincfiles})
add_dependencies(libgobegin runtimeinctarg)
target_include_directories(libgobegin PUBLIC
  "${libgo_csrcroot}/runtime"
  "${libgo_binroot}/runtime")
# Use -fPIC for libgobegin so that it can be put in a PIE.
set_target_properties(libgobegin PROPERTIES COMPILE_FLAGS "${baseopts} -fPIC")
set_target_properties(libgobegin PROPERTIES OUTPUT_NAME "gobegin")
set_target_properties(libgobegin PROPERTIES
  ARCHIVE_OUTPUT_DIRECTORY ${libgo_binroot})

# Sources for libgolibbegin.a
set(libgolibbegincfiles
  "${libgo_csrcroot}/runtime/go-libmain.c")
# libgolibbegin.a (static library only)
add_gollvm_library(libgolibbegin STATIC
  ${libgolibbegincfiles})
add_dependencies(libgolibbegin runtimeinctarg)
target_include_directories(libgolibbegin PUBLIC
  "${libgo_csrcroot}/runtime"
  "${libgo_binroot}/runtime")
# Use -fPIC for libgolibbegin so that it can be put in a PIE.
set_target_properties(libgolibbegin PROPERTIES
  COMPILE_FLAGS "${baseopts} -fPIC")
set_target_properties(libgolibbegin PROPERTIES OUTPUT_NAME "golibbegin")
set_target_properties(libgolibbegin PROPERTIES
  ARCHIVE_OUTPUT_DIRECTORY ${libgo_binroot})

# Pseudo-target for all libgo buildables.
add_custom_target(libgo_all DEPENDS
  llvm-goc libgo_static libgo_shared libgobegin libgolibbegin)
add_dependencies(gollvm libgo_all)

# Create a target-specific symlink to the Go library dir. This is
# an interim solution; ideally we want to key off of CMAKE_INSTALL_PREFIX.
execute_process(COMMAND ${CMAKE_COMMAND}
  -E make_directory "${LLVM_BINARY_DIR}/libgo")
execute_process(COMMAND ${CMAKE_COMMAND}
  -E create_symlink "${libgo_binroot}"
   "${LLVM_BINARY_DIR}/libgo/${LLVM_DEFAULT_TARGET_TRIPLE}")

#........................................................................
#
# Check target generation.
#
set(extra_check_libs_cmd_go_internal_cache "${libgo_binroot}/libgotool.a")
set(extra_check_libs_cmd_go_internal_generate "${libgo_binroot}/libgotool.a")
set(extra_check_libs_cmd_go_internal_get "${libgo_binroot}/libgotool.a")
set(extra_check_libs_cmd_go_internal_load "${libgo_binroot}/libgotool.a")
set(extra_check_libs_cmd_go_internal_lockedfile "${libgo_binroot}/libgotool.a")
set(extra_check_libs_cmd_go_internal_imports "${libgo_binroot}/libgotool.a")
set(extra_check_libs_cmd_go_internal_modconv "${libgo_binroot}/libgotool.a")
set(extra_check_libs_cmd_go_internal_modfetch "${libgo_binroot}/libgotool.a")
set(extra_check_libs_cmd_go_internal_modfetch_codehost "${libgo_binroot}/libgotool.a")
set(extra_check_libs_cmd_go_internal_modfile "${libgo_binroot}/libgotool.a")
set(extra_check_libs_cmd_go_internal_modload "${libgo_binroot}/libgotool.a")
set(extra_check_libs_cmd_go_internal_module "${libgo_binroot}/libgotool.a")
set(extra_check_libs_cmd_go_internal_mvs "${libgo_binroot}/libgotool.a")
set(extra_check_libs_cmd_go_internal_search "${libgo_binroot}/libgotool.a")
set(extra_check_libs_cmd_go_internal_web2 "${libgo_binroot}/libgotool.a")
set(extra_check_libs_cmd_go_internal_work "${libgo_binroot}/libgotool.a")
set(extra_check_libs_cmd_vet_internal_cfg "${libgo_binroot}/libgotool.a")

set(checktargets)

message(STATUS "Libgo: generating check targets")
foreach( pack ${checkpackages})
  string(REPLACE "/" "_" ptarget2 "${pack}")
  string(REPLACE "." "_" ptarget "${ptarget}")
  set(runner "${CMAKE_CURRENT_SOURCE_DIR}/checkpackage.sh")

  # This will set 'packsrcs' and 'packopts'
  collect_package_inputs(${pack})

  set(extralibs "${extra_check_libs_${ptarget}}")

  # In some cases we want a different set of flags for the check
  # target, notable the runtime package (where the main build uses
  # -fgo-c-header=... but the check build does not).
  set(checkgocflags "${${ptarget}_check_gocflags}")
  if(NOT "${checkgocflags}" STREQUAL "")
    set(packopts "${checkgocflags}")
  endif()

  # Test target for package x/y/z will be check_libgo_x_y_z
  set(targetname "check_libgo_${ptarget}")

  # Note: only a subset of package tests are dependent on libgotool.a,
  # but for simplicity's sake we'll just make them all dependent on it.
  add_custom_target(
    ${targetname}
    COMMAND "${shell}" ${runner}
    "PACKAGE" ${pack}
    "FILES" ${packsrcs}
    "GOOS" ${goos}
    "GOARCH" ${goarch}
    "GC" ${gocompiler} "-L" ${libgo_binroot} ${packopts} ${libgo_extra_gocflags}
    "GOLIBS" ${packlibs} ${extralibs}
    "BINDIR" "${libgo_binroot}"
    "BASEDIR" "${libgo_srcroot}"
    DEPENDS ${libgo_goxfiles} libgotool libgo_shared libgo_static libgobegin
    COMMENT "Checking Go package ${pack}"
    VERBATIM)
  list(APPEND checktargets ${targetname})

endforeach()

add_custom_target(check-libgo DEPENDS ${checktargets})

message(STATUS "libgo configuration complete.")
