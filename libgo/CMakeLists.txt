
# Building libgo is time-consuming. Allow developers to stub it out
# via cmake flag if desired.

if(DISABLE_LIBGO_BUILD)
  return()
endif()

message(STATUS "starting libgo configuration.")

list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake")

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

# Directory from which we're going to pull libgo helper scripts (ex: match.sh)
set(libgo_scriptroot ${libgo_srcroot})

# Libbacktrace source code.
set(libbacktrace_srcroot "${CMAKE_CURRENT_SOURCE_DIR}/libbacktrace")

# Libffi source code.
set(libffi_srcroot "${CMAKE_CURRENT_SOURCE_DIR}/libffi")

# Binary root (top level of libgo build). 
set(libgo_binroot "${CMAKE_CURRENT_BINARY_DIR}")

# FIXME: rewrite this to locate llvm-goparse in a more official way
# set(driver "${bin}/../bin/llvm-goparse")
get_target_property(driverdir llvm-goparse RUNTIME_OUTPUT_DIRECTORY)
set(gollvm_driver "${driverdir}/llvm-goparse")
#set(gocompiler ${gollvm_driver})

# FIXME: still need to use wrapper for compilation, since
# llvm-goparse functionality not yet complete.
set(gocompiler "gccgo")

# Read in list of all libgo packages
file(STRINGS "${CMAKE_CURRENT_SOURCE_DIR}/libgo-packages.txt" libpackages)
file(STRINGS "${CMAKE_CURRENT_SOURCE_DIR}/gotool-packages.txt" toolpackages)
list(APPEND allpackages ${libpackages})
list(APPEND allpackages ${toolpackages})

# Script used to collect the Go source files that make up a package. 
set(matchdotsh "${libgo_scriptroot}/match.sh")

# Package-specific match script arguments 
set(runtime_matchargs "--tag=libffi")

# Collect the source files in each package, write to temp file.
foreach( pack ${allpackages})

  # The package directory should exist-- issue an error if it does not.
  if(NOT EXISTS ${libgo_gosrcroot}/${pack})
    message(SEND_ERROR "Package directory ${pack} does not exist.")
  else()

    string(REPLACE "/" "_" ptarget "${pack}")
    set(packfilestmp "${libgo_binroot}/${ptarget}.gofiles")

    # Package-specific match args
    set(margs "${${ptarget}_matchargs}")

    # Invoke match.sh to collect Go files of interest for this
    # package, via shell script. Read result into variable
    execute_process(COMMAND "${shell}" "${matchdotsh}" ${margs}
      "--goarch=${goarch}" "--goos=${goos}"
      "--srcdir=${libgo_gosrcroot}/${pack}"
      OUTPUT_FILE ${packfilestmp})
  endif()
endforeach()

#........................................................................

# Certain packages need special compiler options and extra Go source files.
# The convention here is that <package>_extra_go_files holds the
# additional sources, and <package>_gocflags holds the command line options,
# and <package>_matchargs holds additional match.sh arguments.

# FIXME: when 386 comes on line, use correct 386-specific options here.

# FIXME: correct gcc-compatible option should be -ffp-contract=off,
# however at the moment for llm-goparse it is -fp-contract=off
set(math_gocflags "-fp-contract=off")

set(runtime_gocflags "-fgo-c-header=runtime.inc.raw" "-fgo-compiling-runtime")
set(runtime_internal_atomic_gocflags "-fgo-compiling-runtime")
set(runtime_internal_atomic_check_gocflags "-fgo-compiling-runtime")
set(runtime_internal_sys_gocflags "-fgo-compiling-runtime")
set(runtime_internal_sys_check_gocflags "-fgo-compiling-runtime")

# Incorporate auto-generated Go files.
set(runtime_extra_go_files "runtime_sysinfo.go" "sigtab.go")
set(cmd_internal_objabi_extra_go_files "objabi.go")
set(cmd_go_internal_load_extra_go_files "zstdpkglist.go")
set(cmd_go_internal_cfg_extra_go_files "zdefaultcc.go")
set(runtime_internal_sys_extra_go_files "version.go")
set(go_types_extra_go_files "gccgosizes.go")
set(syscall_extra_go_files "libcalls.go" "sysinfo.go" "syscall_arch.go")
if(${goos} STREQUAL "linux")
  list(APPEND syscall_extra_go_files "epoll.go")
endif()

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
mkzstdpkglist(${zstdpkglisttmp} ${libpackages})
copy_if_different(${zstdpkglisttmp} ${zstdpkglistdotgo})

# Rules for zdefaultcc.go
set(zdefaultccdotgo "${libgo_binroot}/zdefaultcc.go")
set(zdefaultcctmp "${libgo_binroot}/zdefaultcc.go.tmp")
mkzdefaultcc(${zdefaultcctmp} ${gollvm_driver}
             ${CMAKE_C_COMPILER} ${CMAKE_CXX_COMPILER})
copy_if_different(${zdefaultcctmp} ${zdefaultccdotgo})


# FIXME: Use a rule that copies in an archived copy of this source
# file, since for the moment there is no clang support for the
# -fdump-go-spec option (better to implement that as a bitcode pass,
# to be submitted in a forthcoming patch).
set(gensysinfo "${libgo_binroot}/gen-sysinfo.go")
generate_go_from_archived(${gensysinfo} ${goos} ${goarch})

# Command to create runtime_sysinfo.go, via shell script.
set(mkrsysinfosh "${libgo_scriptroot}/mkrsysinfo.sh")
set(rsysinfodotgo "${libgo_binroot}/runtime_sysinfo.go")
generate_go_from_script(${rsysinfodotgo} ${mkrsysinfosh}
                        ${goos} ${goarch} ${libgo_binroot} DEP ${gensysinfo})

# Command to create sigtab.go, via shell script
set(sigtabdotgo "${libgo_binroot}/sigtab.go")
set(mksigtabsh "${libgo_scriptroot}/mksigtab.sh")
generate_go_from_script(${sigtabdotgo} ${mksigtabsh}
                        ${goos} ${goarch} ${libgo_binroot}
                        CAPTURE DEP ${gensysinfo})

# Generation of libcalls.go
set(libcallsdotgo "${libgo_binroot}/libcalls.go")
set(libcallstmp "${libgo_binroot}/tmp-libcalls.go")
set(awkfile "${libgo_gosrcroot}/syscall/mksyscall.awk")
set(mklibcallssh "${CMAKE_CURRENT_SOURCE_DIR}/mklibcalls.sh")
set(syscallgofiles "${libgo_binroot}/syscall.gofiles")
generate_go_from_script(${libcallsdotgo} ${mklibcallssh} ${goos} ${goarch}
                        ${libgo_binroot} DEP ${awkfile} ${syscallgofiles}
                        SCRIPTARGS ${awk} ${awkfile} ${syscallgofiles}
			${libcallstmp})

# Generated file errno.i
set(errnoi "${libgo_binroot}/errno.i")
set(errnoitmp "${libgo_binroot}/tmp-errno.i")
set(mkerrnoish "${CMAKE_CURRENT_SOURCE_DIR}/mkerrnoi.sh") 
generate_go_from_script(${errnoi} ${mkerrnoish} ${goos} ${goarch}
                        ${libgo_binroot} SCRIPTARGS "${CMAKE_C_COMPILER}"
                        ${errnoitmp} ${CMAKE_C_FLAGS})

# Generated file sysinfo.go
set(sysinfodotgo "${libgo_binroot}/sysinfo.go")
set(mksysinfosh "${libgo_scriptroot}/mksysinfo.sh")

generate_go_from_script(${sysinfodotgo} ${mksysinfosh} ${goos} ${goarch}
                        ${libgo_binroot} DEP ${gensysinfo} ${errnoi})

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
  set(packsrcs)
  set(packopts)
  
  string(REPLACE "/" "_" ptarget "${pack}")
  set(packfilestmp "${libgo_binroot}/${ptarget}.gofiles")
  file(STRINGS ${packfilestmp} matchoutput)
  separate_arguments(matchoutput)

  # Canonicalize paths. Not strictly needed, but makes output nicer.
  set(packsrcs)
  foreach( packsrc ${matchoutput})
    string(STRIP ${packsrc} spack)
    get_filename_component(canonsrc "${spack}" REALPATH)
    list(APPEND packsrcs "${canonsrc}")
  endforeach()
  set(basepacksrcs ${packsrcs})
  
  # Incorporate any extra source files. These generated files are
  # expected to appear in the root of the bin dir.
  set(extrasrcs "${${ptarget}_extra_go_files}")
  foreach( esrc ${extrasrcs})
    string(STRIP ${esrc} esrcf)
    list(APPEND packsrcs "${libgo_binroot}/${esrcf}")
  endforeach()

  # Collect any package-specific Go command line flags
  set(packopts "${${ptarget}_gocflags}")
endmacro()

#........................................................................

message(STATUS "Libgo: creating stdlib package targets")

set(libgo_go_picobjects)
set(libgo_go_nonpicobjects)
set(libgo_goxfiles)
set(libgotool_nonpicobjects)

# Process each package
foreach( pack ${allpackages})
  string(REPLACE "/" "_" ptarget "${pack}")

  collect_package_inputs(${pack})
  
  # Generate dependencies.
  set(packdeps)
  gendeps(${pack} ${libgo_scriptroot} SOURCES ${basepacksrcs})

  # If this is a gotool package, we don't need a pic version
  set(nopic)
  list(FIND toolpackages ${pack} found)
  if(NOT ${found} EQUAL -1)
    set(nopic "NOPIC")
  endif()

  # Call into helper to create rules for package.
  add_go_package("${pack}" "${libgo_binroot}" GOSRC ${packsrcs} GODEP ${packdeps} GOCFLAGS ${packopts} ${nopic})

  # Accumulate libgo objects. 
  if(${found} EQUAL -1)
    list(APPEND libgo_go_picobjects ${package_picofile})
    list(APPEND libgo_go_nonpicobjects ${package_ofile})
    list(APPEND libgo_goxfiles ${package_goxfile})
  else()
    list(APPEND libgotool_nonpicobjects ${package_ofile})
  endif()
endforeach()

#........................................................................
#
# Libbacktrace

# Call a helper to set up libbacktrace.
setup_libbacktrace()

# libbacktrace sources. FIXME: break out into a separate dir or file.
set(libbacktracecfiles
  "${libbacktrace_srcroot}/atomic.c"
  "${libbacktrace_srcroot}/backtrace.c"
  "${libbacktrace_srcroot}/dwarf.c"
  "${libbacktrace_srcroot}/elf.c"
  "${libbacktrace_srcroot}/fileline.c"
  "${libbacktrace_srcroot}/mmapio.c"
  "${libbacktrace_srcroot}/mmap.c"
  "${libbacktrace_srcroot}/posix.c"
  "${libbacktrace_srcroot}/print.c"
  "${libbacktrace_srcroot}/simple.c"
  "${libbacktrace_srcroot}/sort.c"
  "${libbacktrace_srcroot}/state.c")

set(libbacktraceflags "-fsplit-stack")

# Object libraries built from libbacktrace sources.
# Note: this build uses -fsplit-stack, whereas in the gccgo
# libgo it does not. In theory this should be a good thing.

# nonpic
add_library(libbacktrace_nonpiclib
  OBJECT EXCLUDE_FROM_ALL ${libbacktracecfiles})
get_target_property(btflags libbacktrace_nonpiclib COMPILE_FLAGS)
message(STATUS "Prior bt flags: ${btflags}")
set_target_properties(libbacktrace_nonpiclib
  PROPERTIES COMPILE_FLAGS "${libbacktraceflags}")
target_include_directories(libbacktrace_nonpiclib
  BEFORE PRIVATE "${libgo_binroot}/libbacktrace")

# pic
string(APPEND libbacktraceflags " -fPIC")
add_library(libbacktrace_piclib
  OBJECT EXCLUDE_FROM_ALL ${libbacktracecfiles})
target_include_directories(libbacktrace_piclib
  BEFORE PRIVATE "${libgo_binroot}/libbacktrace")
set_target_properties(libbacktrace_piclib
  PROPERTIES COMPILE_FLAGS "${libbacktraceflags}")

# Call a helper to set up libffi. This also creates the libffi
# targets (libffi_nonpiclib, libffi_piclib).
setup_libffi(${libffi_srcroot})

message(STATUS "libgo configuration complete.")

