
# Building libgo is time-consuming. Allow developers to stub it out
# via cmake flag if desired.

if(DISABLE_LIBGO_BUILD)
  return()
endif()

message(STATUS "starting libgo configuration.")

list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake")

include(ConfigSetup)
include(GoPackage)
include(GenDeps)

# Root of libgo sources.
set(libgo_srcroot "${GOLLVM_SOURCE_DIR}/gofrontend/libgo")

# Directory from which we're going to pull libgo Go source code.
set(libgo_gosrcroot "${libgo_srcroot}/go")

# Directory from which we're going to pull libgo helper scripts (ex: match.sh)
set(libgo_scriptroot ${libgo_srcroot})

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
file(STRINGS "${libgo_srcroot}/libgo-packages.txt" libpackages)
file(STRINGS "${libgo_srcroot}/gotool-packages.txt" toolpackages)
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
# however at the moment for llvm-goparse it is -fp-contract=off
set(math_gocflags "-fp-contract=off")

set(runtime_gocflags "-fgo-c-header=runtime.inc.raw" "-fgo-compiling-runtime")
set(runtime_internal_atomic_gocflags "-fgo-compiling-runtime")
set(runtime_internal_atomic_check_gocflags "-fgo-compiling-runtime")
set(runtime_internal_sys_gocflags "-fgo-compiling-runtime")
set(runtime_internal_sys_check_gocflags "-fgo-compiling-runtime")

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
  godeps(${pack} ${libgo_scriptroot} SOURCES ${basepacksrcs})

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

message(STATUS "libgo configuration complete.")
