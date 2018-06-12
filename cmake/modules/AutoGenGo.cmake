
#----------------------------------------------------------------------
# Emit 'version.go'. This file contains a series of Go constants that
# capture the target configuration (arch, OS, etc), emitted directly
# to a file (as opposed to generation via script). Based on the
# libgo Makefile recipe.
#
# Unnamed parameters:
#
#   * GOOS setting (target OS)
#   * GOARCH setting (target architecture)
#   * output file
#   * cmake binary directory for libfo
#   * root of src containing libgo script files
#
function(mkversion goos goarch outfile bindir srcroot scriptroot)

  file(REMOVE ${outfile})
  file(WRITE ${outfile} "package sys\n")

  # FIXME: typical LLVM usage allows for performing a build without
  # CMAKE_INSTALL_PREFIX set, then doing an install from the build
  # using a newly chosen CMAKE_INSTALL_PREFIX value. This mode is
  # not currently supported -- the install prefix has to be set properly
  # as part of the original build.

  # Tools subdir within the install.
  file(APPEND ${outfile} "func init() { DefaultGoroot = \"${CMAKE_INSTALL_PREFIX}\" }\n")

  # Compiler version
  file(STRINGS "${srcroot}/../VERSION" rawver)
  string(STRIP ${rawver} ver)
  file(APPEND ${outfile} "const TheVersion = ")
  emitversionstring(${outfile} ${srcroot})
  file(APPEND ${outfile} "\n")

  # FIXME:
  # GccgoToolDir is set to the gccgo installation directory that contains
  # (among other things) "go1", "cgo", "cc1", and other auxiliary
  # executables. Where things like "cgo" will live hasn't been ironed
  # out yet, so just pick a spot in the bin directory for now. See also
  # 'DefaultGoRoot' above.
  file(APPEND ${outfile} "const GccgoToolDir = \"${CMAKE_INSTALL_PREFIX}/tools\"\n")

  # FIXME: add a real switch base on configuration params here.

  if( NOT ${goarch} STREQUAL "amd64")
    message(FATAL_ERROR "unsupported arch ${goarch}: currently only amd64 support")
  else()
    set(archfamily "AMD64")
    set(bigendian "false")
    set(cachelinesize "64")
    set(physpagesize "4096")
    set(pcquantum "1")
    set(int64align "8")
    set(hugepagesize "1 << 21")
    set(minframesize 0)
  endif()

  file(APPEND ${outfile} "const Goexperiment = ``\n")
  file(APPEND ${outfile} "const GOARCH = \"${goarch}\"\n")
  file(APPEND ${outfile} "const GOOS = \"${goos}\"\n")
  file(APPEND ${outfile} "\n")
  file(APPEND ${outfile} "type ArchFamilyType int\n\n")
  file(APPEND ${outfile} "const (\n")
  file(APPEND ${outfile} "\tUNKNOWN ArchFamilyType = iota\n")

  foreach (af ${allgoarchfamily})
    file(APPEND ${outfile} "\t${af}\n")
  endforeach()
  file(APPEND ${outfile} ")\n\n")

  foreach (arch ${allgoarch})
    set(val "0")
    if( ${arch} STREQUAL ${goarch})
      set(val "1")
    endif()
    upperfirst(${arch} "uparch")
    file(APPEND ${outfile} "const Goarch${uparch} = ${val}\n")
  endforeach()
  file(APPEND ${outfile} "\n")

  set(constants "ArchFamily:family" "BigEndian:bigendian" "CacheLineSize:cachelinesize" "PhysPageSize:defaultphyspagesize" "PCQuantum:pcquantum" "Int64Align:int64align" "HugePageSize:hugepagesize" "MinFrameSize:minframesize")

  file(APPEND ${outfile} "const (\n")
  foreach(item ${constants})

    # Split item into Go constant name and
    string(REGEX REPLACE ":" " " sitem ${item})
    separate_arguments(sitem)
    list(GET sitem 0 constname)
    list(GET sitem 1 scriptarg)

    # Invoke goarch.sh
    execute_process(COMMAND ${shell} "${scriptroot}/goarch.sh"
      ${goarch} ${scriptarg}
      OUTPUT_VARIABLE result
      ERROR_VARIABLE errmsg
      RESULT_VARIABLE exitstatus)
    if(${exitstatus} MATCHES 0)
      file(APPEND ${outfile} "\t${constname} = ${result}")
    else()
      message(FATAL_ERROR "goarch.sh invocation failed: ${errmsg}")
    endif()

  endforeach()

  file(APPEND ${outfile} ")\n\n")

  foreach (os ${allgoos})
    set(val "0")
    if( ${os} STREQUAL ${goos})
      set(val "1")
    endif()
    upperfirst(${os} "upos")
    file(APPEND ${outfile} "const Goos${upos} = ${val}\n")
  endforeach()

  file(APPEND ${outfile} "\n")
  file(APPEND ${outfile} "type Uintreg uintptr\n")
endfunction()

macro(upperfirst name result)
  string(SUBSTRING ${name} 0 1 c1)
  string(SUBSTRING ${name} 1 -1 crem)
  string(TOUPPER ${c1} upc1)
  set(${result} "${upc1}${crem}")
endmacro()

#----------------------------------------------------------------------
# Emit compiler version string to specified output file. This is extracted
# out into a separate function since it is needed in a couple of different
# places.
#
# Unnamed parameters:
#
#   * output file to target
#   * root of libgo source

function(emitversionstring outfile srcroot)
  file(STRINGS "${srcroot}/../VERSION" rawver)
  string(STRIP ${rawver} ver)
  file(APPEND ${outfile} "\"${ver} gollvm LLVM ${LLVM_VERSION_MAJOR}.${LLVM_VERSION_MINOR}.${LLVM_VERSION_PATCH}${LLVM_VERSION_SUFFIX}\"")
endfunction()

#----------------------------------------------------------------------
# Emit 'gccgosizes.go', which includes size and alignment constants
# by architecture.
#
# Unnamed parameters:
#
#   * GOARCH setting for target
#   * output file to target
#   * directory containing libgo scripts
#
function(mkgccgosizes goarch outfile scriptroot)

  file(REMOVE ${outfile})
  file(WRITE ${outfile} "package types\n\n")
  file(APPEND ${outfile} "var gccgoArchSizes = map[string]*StdSizes{\n")
  foreach(arch ${allgoarch})

    execute_process(COMMAND ${shell} "${scriptroot}/goarch.sh"
      ${arch} "ptrsize"
      OUTPUT_VARIABLE presult
      ERROR_VARIABLE errmsg
      RESULT_VARIABLE exitstatus)
    if(NOT ${exitstatus} MATCHES 0)
      message(FATAL_ERROR "goarch.sh invocation failed: ${errmsg}")
    endif()

    execute_process(COMMAND ${shell} "${scriptroot}/goarch.sh"
      ${arch} "maxalign"
      OUTPUT_VARIABLE aresult
      ERROR_VARIABLE errmsg
      RESULT_VARIABLE exitstatus)
    if(NOT ${exitstatus} MATCHES 0)
      message(FATAL_ERROR "goarch.sh invocation failed: ${errmsg}")
    endif()

    string(STRIP ${presult} presult)
    string(STRIP ${aresult} aresult)

    file(APPEND ${outfile} "\"${arch}\": {${presult}, ${aresult}},\n")
  endforeach()
  file(APPEND ${outfile} "}\n")
endfunction()

#----------------------------------------------------------------------
# Emit 'objabi.go', containing default settings for various GO* vars.
#
# Unnamed parameters:
#
#   * output file to target
#   * libgo cmake binary directory
#   * libgo source code root directory
#
function(mkobjabi outfile binroot srcroot)

  file(REMOVE ${outfile})
  file(WRITE ${outfile} "package objabi\n\n")
  file(APPEND ${outfile} "import \"runtime\"\n")

  file(APPEND ${outfile} "func init() { defaultGOROOT = \"${GOLLVM_INSTALL_DIR}\" }\n")

  file(APPEND ${outfile} "const defaultGO386 = `sse2`\n")
  file(APPEND ${outfile} "const defaultGOARM = `5`\n")
  file(APPEND ${outfile} "const defaultGOMIPS = `hardfloat`\n")
  file(APPEND ${outfile} "const defaultGOOS = runtime.GOOS\n")
  file(APPEND ${outfile} "const defaultGOARCH = runtime.GOARCH\n")
  file(APPEND ${outfile} "const defaultGO_EXTLINK_ENABLED = ``\n")
  file(APPEND ${outfile} "const version = ")
  emitversionstring(${outfile} ${srcroot})
  file(APPEND ${outfile} "\n")
  file(APPEND ${outfile} "const stackGuardMultiplier = 1\n")
  file(APPEND ${outfile} "const goexperiment = ``\n")
endfunction()

#----------------------------------------------------------------------
# Emit 'zstdpkglist.go', containing a map with all of the std Go packages.
#
# Unnamed parameters:
#
#   * package to use for generated Go code
#   * output file to target
#   * list of go std library packages (does not include libgotool packages)
#
function(mkzstdpkglist package outfile libpackages)
  file(REMOVE ${outfile})
  file(WRITE ${outfile} "package ${package}\n\n")
  file(APPEND ${outfile} "var stdpkg = map[string]bool{")
  foreach(pack ${libpackages})
    file(APPEND ${outfile} "\"${pack}\": true,\n")
  endforeach()
  file(APPEND ${outfile} "\"unsafe\": true,\n")
  file(APPEND ${outfile} "\"runtime/cgo\": true,\n")
  file(APPEND ${outfile} "}\n")
endfunction()

#----------------------------------------------------------------------
# Emit 'zdefaultcc.go', containing info on where to find C/C++ compilers.
#
# Unnamed parameters:
#
#   * package to use for generated Go code
#   * output file to target
#   * C compiler path
#   * C++ compiler path
#
# Named parameters:
#
# EXPORT    Generated public functions (ex: DefaultCC not defaultCC).
#
function(mkzdefaultcc package outfile ccpath cxxpath)
  CMAKE_PARSE_ARGUMENTS(ARG "EXPORT" "" "" ${ARGN})

  # Construct default driver path
  set(driverpath "${GOLLVM_INSTALL_DIR}/bin/llvm-goc")

  file(REMOVE ${outfile})
  file(WRITE ${outfile} "package ${package}\n\n")

  set(f1 "defaultGCCGO")
  set(f2 "defaultCC")
  set(f3 "defaultCXX")
  set(f4 "defaultPkgConfig")
  set(v1 "oSArchSupportsCgo")
  if( ${ARG_EXPORT} )
    upperfirst(${f1} "f1")
    upperfirst(${f2} "f2")
    upperfirst(${f3} "f3")
    upperfirst(${f4} "f4")
    upperfirst(${v1} "v1")
  endif()

  file(APPEND ${outfile} "func ${f1}(goos, goarch string) string { return \"${driverpath}\" }\n")
  file(APPEND ${outfile} "func ${f2}(goos, goarch string) string { return \"${ccpath}\" }\n")
  file(APPEND ${outfile} "func ${f3}(goos, goarch string) string { return \"${cxxpath}\" }\n")
  file(APPEND ${outfile} "const ${f4} = \"pkg-config\"\n")
  file(APPEND ${outfile} "var ${v1} = map[string]bool{}\n")
endfunction()

#----------------------------------------------------------------------
# Emit 'epoll.go', containing info on the epoll syscall.  Reads
# variables SIZEOF_STRUCT_EPOLL_EVENT and STRUCT_EPOLL_EVENT_FD_OFFSET
# previously set by config process.
#
# Unnamed parameters:
#
#   * output file to target
#
function(mkepoll outfile)
  file(REMOVE ${outfile})
  file(WRITE ${outfile} "package syscall\n\n")

  set(s ${SIZEOF_STRUCT_EPOLL_EVENT})
  set(o ${STRUCT_EPOLL_EVENT_FD_OFFSET})

  file(APPEND ${outfile} "type EpollEvent struct {")
  file(APPEND ${outfile} "  Events uint32\n")
  if(${s} EQUAL 0 AND ${o} EQUAL 0)
    message(SEND_ERROR "*** struct epoll_event data.fd offset unknown")
  elseif(${s} EQUAL 8 AND ${o} EQUAL 4)
    file(APPEND ${outfile} "  Fd int32\n")
  elseif(${s} EQUAL 12 AND ${o} EQUAL 4)
    file(APPEND ${outfile} "  Fd int32\n  Pad [4]byte\n")
  elseif(${s} EQUAL 12 AND ${o} EQUAL 8)
    file(APPEND ${outfile} "  Pad [4]byte\n  Fd int32\n")
  elseif(${s} EQUAL 16 AND ${o} EQUAL 8)
    file(APPEND ${outfile} "  Pad [4]byte\n  Fd int32\n  Pad [4]byte\n  ")
  else()
    message(SEND_ERROR "*** struct epoll_event data.fd offset unknown")
  endif()
  file(APPEND ${outfile} "}\n")
endfunction()

#----------------------------------------------------------------------
# Emit 'syscall_arch.go', containing arch and os constants.
#
# Unnamed parameters:
#
#   * output file to target
#   * GOOS setting (target OS)
#   * GOARCH setting (target architecture)
#
function(mksyscallarch outfile goos goarch)
  file(REMOVE ${outfile})
  file(WRITE ${outfile} "package syscall\n\n")
  file(APPEND ${outfile} "const ARCH = \"${goarch}\"\n")
  file(APPEND ${outfile} "const OS = \"${goos}\"\n")
endfunction()

# This helper function provides a general mechanism for running a
# shell script to emit a Go code to a temp file, then adds a cmake
# target that copies the temp to a specific *.go file if the temp
# is different.
#
# Example usage:
#
# generate_go_from_script(outfile script goos goarch workdir DEP <deps>)
#
# Unnamed parameters:
#
#   * generated Go source file (this should be a full path)
#   * script to run
#   * GOOS setting (target OS)
#   * GOARCH setting (target architecture)
#   * working bin directory
#
# Named parameters:
#
# CAPTURE      Capture stdout from script into output file.
# SCRIPTARGS   Additional args to pass to script.
# DEP          Things that the generated file should depend on.
#

function(generate_go_from_script outpath script goos goarch workdir)
  CMAKE_PARSE_ARGUMENTS(ARG "CAPTURE" "" "DEP;SCRIPTARGS" ${ARGN})

  get_filename_component(outfile "${outpath}" NAME)
  get_filename_component(outdir "${outpath}" DIRECTORY)
  set(tmpfile "${outdir}/tmp-${outfile}")
  set(shell $ENV{SHELL})

  # Create a rule that runs the script into a temporary file.
  if( NOT ARG_CAPTURE )
    add_custom_command(
      # For this variant, the assumption is that the script writes it
      # output to a specified file.
      OUTPUT ${tmpfile}
      COMMAND "GOARCH=${goarch}" "GOOS=${goos}"
              "${shell}" ${script} ${ARG_SCRIPTARGS}
      COMMENT "Creating ${tmpfile}"
      DEPENDS ${script} ${ARG_DEP}
      WORKING_DIRECTORY ${workdir}
      VERBATIM)
  else()
    # For this variant, the script is emitting output to stdout,
    # which we need to capture and redirect to a file.
    set(capturesh "${CMAKE_CURRENT_SOURCE_DIR}/capturescript.sh")
    add_custom_command(
      OUTPUT ${tmpfile}
      COMMAND "GOARCH=${goarch}" "GOOS=${goos}"
              "${shell}" ${capturesh} ${script} ${tmpfile} ${ARG_SCRIPTARGS}
      COMMENT "Creating ${tmpfile}"
      DEPENDS ${script} ${ARG_DEP}
      WORKING_DIRECTORY ${workdir}
      VERBATIM)
  endif()

  # Create a rule that copies tmp file to output file if different.
  copy_if_different(${tmpfile} ${outpath})
endfunction()

# This function manages the cmake rules for the auto-generated
# 'gen-sysinfo.go' file, which contains type information derived from
# compiling a C program that includes various system headers.
#
# Unnamed parameters:
#
#   * name of tmpfile into which to generate provisional Go code
#   * name of generated Go source file for final Go code
#   * name of macro temp file to generate
#   * name of temp object file to write
#   * path to godumpspec tool
#   * path to sysinfo.c
#
# Names params:
#
#   * DEPS -- things gen-sysinfo.go is dependent on (ex: config.h)
#   * CFLAGS -- additional compile options (ex: -I ...)
#
function(mkgensysinfo tmpfile outfile macrofile objfile godumpspec sysinfoc)
  CMAKE_PARSE_ARGUMENTS(ARG "" "" "DEPS;CFLAGS" ${ARGN})

  set(ccomp ${CMAKE_C_COMPILER})
  set(cflags ${ARG_CFLAGS})
  if(NOT "${CMAKE_SYSROOT}" STREQUAL "")
    set(cflags ${cflags} "--sysroot=${CMAKE_SYSROOT}")
  endif()

  # Run the host C compiler to generate the object. NB: clang will
  # accept -fno-eliminate-unused-debug-types but does not actually
  # implement this functionality.
  add_custom_command(
    OUTPUT ${objfile}
    COMMAND ${ccomp} "-g3" "-c" "-fno-eliminate-unused-debug-types"
            ${sysinfoc} -o ${objfile} ${cflags}
    DEPENDS ${sysinfoc} ${ARG_DEPS}
    COMMENT "Building sysinfo.o "
    VERBATIM)

  # Another compile to build the macro temp file.
  add_custom_command(
    OUTPUT ${macrofile}
    COMMAND ${ccomp} "-E" "-dM"
            ${sysinfoc} -o ${macrofile} ${cflags}
    DEPENDS ${sysinfoc} ${ARG_DEPS}
    COMMENT "Building sysinfo.c macro temp file"
    VERBATIM)

  # Next step is to run the llvm-godumpspec utility, which consumes
  # the two files produced above and generates the Go file in question.
  add_custom_command(
    OUTPUT ${tmpfile}
    COMMAND ${godumpspec}
    "--macrotmp=${macrofile}" "--object=${objfile}" "--output=${tmpfile}"
    DEPENDS ${objfile} ${macrofile} ${ARG_DEPS}
    COMMENT "Generating ${outfile}"
    VERBATIM)

  # Create a rule that copies tmpfile to output file if different.
  copy_if_different(${tmpfile} ${outfile})

endfunction()
