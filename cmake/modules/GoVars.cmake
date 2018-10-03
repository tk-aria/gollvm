
# Set goarch/goos based on what we see in the LLVM triple.
string(REGEX REPLACE "-" " " lht_components ${LLVM_DEFAULT_TARGET_TRIPLE})
separate_arguments(lht_components)
list(GET lht_components 0 llarch)
list(GET lht_components 2 goos)

# LLVM's "x86_64" is the same as Go's "amd64".
if( ${llarch} STREQUAL "x86_64" )
  set(goarch "amd64")
else()
  message(SEND_ERROR "Arch ${llarch} not yet supported")
endif()

# List of all architectures, families, os flavors.
set(allgoarch "386" "alpha" "amd64" "amd64p32" "arm" "armbe" "arm64" "arm64be" "ia64" "m68k" "mips" "mipsle" "mips64" "mips64le" "mips64p32" "mips64p32le" "nios2" "ppc" "ppc64" "ppc64le" "riscv" "riscv64" "s390" "s390x" "sh" "shbe" "sparc" "sparc64" "wasm")
set(allgoarchfamily "I386" "ALPHA" "AMD64" "ARM" "ARM64" "IA64" "M68K" "MIPS" "MIPS64" "PPC" "PPC64" "RISCV" "RISCV64" "S390" "S390X" "SH" "SPARC" "SPARC64" "WASM")
set(allgoos "aix" "android" "darwin" "dragonfly" "freebsd" "irix" "linux" "netbsd" "openbsd" "plan9" "rtems" "solaris" "windows")

# Set library suffix based on target triple
if( ${llarch} STREQUAL "x86_64" )
  set(library_suffix "64")
else()
  message(SEND_ERROR "Arch ${llarch} not yet supported")
endif()

# FIXME: write code to insure that this is set and that the shell
# in question behaves properly.
set(shell $ENV{SHELL})

# FIXME: write cmake to discover awk, test to make sure it works
set(awk "/usr/bin/awk")

