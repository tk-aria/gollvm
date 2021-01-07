
# Set goarch/goos based on what we see in the LLVM triple.
string(REGEX REPLACE "-" " " lht_components ${LLVM_DEFAULT_TARGET_TRIPLE})
separate_arguments(lht_components)
list(GET lht_components 0 llarch)
list(GET lht_components 2 goos)

# LLVM's "x86_64" is the same as Go's "amd64".
if( ${llarch} STREQUAL "x86_64" )
  set(goarch "amd64")
# LLVM's "aarch64" is the same as Go's "arm64".
elseif( ${llarch} STREQUAL "aarch64" )
  set(goarch "arm64")
else()
  message(SEND_ERROR "Arch ${llarch} not yet supported")
endif()

# List of all architectures, families, os flavors.
set(allgoarch "386" "alpha" "amd64" "amd64p32" "arm" "armbe" "arm64" "arm64be" "ia64" "m68k" "mips" "mipsle" "mips64" "mips64le" "mips64p32" "mips64p32le" "nios2" "ppc" "ppc64" "ppc64le" "riscv" "riscv64" "s390" "s390x" "sh" "shbe" "sparc" "sparc64" "wasm")
set(allgoarchfamily "I386" "ALPHA" "AMD64" "ARM" "ARM64" "IA64" "M68K" "MIPS" "MIPS64" "PPC" "PPC64" "RISCV" "RISCV64" "S390" "S390X" "SH" "SPARC" "SPARC64" "WASM")
set(allgoos "aix" "android" "darwin" "dragonfly" "freebsd" "ios" "irix" "linux" "netbsd" "openbsd" "plan9" "rtems" "solaris" "windows" "zos")

# Set library suffix based on target triple
if( ${llarch} STREQUAL "x86_64" )
  set(library_suffix "64")
elseif( ${llarch} STREQUAL "aarch64" )
# Driver::installedLibDir honors ./lib64 only
# Future change needed (along with those in AddGollvm.cmake)
  set(library_suffix "64")
else()
  message(SEND_ERROR "Arch ${llarch} not yet supported")
endif()

# We need a working shell. 
if(DEFINED ENV{SHELL})
  set(shell $ENV{SHELL})
else()
  set(shell "/bin/bash")
endif()
execute_process(COMMAND "${shell}" "-c" "echo foo" OUTPUT_VARIABLE echofoo)
if(echofoo STREQUAL "")
message(FATAL_ERROR "fatal: shell ${shell} missing or not functional")
endif()
string(STRIP "${echofoo}" stripechofoo)
if(NOT stripechofoo STREQUAL "foo")
message(FATAL_ERROR "fatal: shell ${shell} missing or not functional")
endif()

# FIXME: write cmake to discover awk, test to make sure it works
set(awk "/usr/bin/awk")

