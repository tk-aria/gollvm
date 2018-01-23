
# Set goarch/goos based on what we see in the LLVM triple.
string(REGEX REPLACE "-" " " lht_components ${LLVM_DEFAULT_TARGET_TRIPLE})
separate_arguments(lht_components)
list(GET lht_components 0 goarch)
list(GET lht_components 2 goos)

# LLVM's "x86_64" is the same as Go's "amd64".
if( ${goarch} STREQUAL "x86_64" )
  set(goarch "amd64")
endif()

# List of all architectures, families, os flavors.
set(allgoarch "386" "alpha" "amd64" "amd64p32" "arm" "armbe" "arm64" "arm64be" "ia64" "m68k" "mips" "mipsle" "mips64" "mips64le" "mips64p32" "mips64p32le" "ppc" "ppc64" "ppc64le" "s390" "s390x" "sparc" "sparc64")
set(allgoarchfamily "I386" "ALPHA" "AMD64" "ARM" "ARM64" "IA64" "M68K" "MIPS" "MIPS64" "PPC" "PPC64" "S390" "S390X" "SPARC" "SPARC64")
set(allgoos "aix" "android" "darwin" "dragonfly" "freebsd" "irix" "linux" "netbsd" "openbsd" "plan9" "rtems" "solaris" "windows")

# FIXME: write code to insure that this is set and that the shell
# in question behaves properly.
set(shell $ENV{SHELL})

# FIXME: write cmake to discover awk, test to make sure it works
set(awk "/usr/bin/awk")

set(atomicstuff "
int i;
int main () {
  __atomic_load_n (&i, __ATOMIC_ACQUIRE);
  __atomic_store_n (&i, 1, __ATOMIC_RELEASE);
 return 0;
}\n")

# Check for atomics
check_c_source_compiles("${atomicstuff}" HAVE_ATOMIC_FUNCTIONS)

# Assorted things needed by libbacktrace
check_symbol_exists(clock_gettime "time.h" HAVE_CLOCK_GETTIME)
check_symbol_exists(strnlen "string.h" HAVE_DECL_STRNLEN)
list(APPEND CMAKE_REQUIRED_DEFINITIONS "-D_GNU_SOURCE")
check_symbol_exists(dl_iterate_phdr "link.h" HAVE_DL_ITERATE_PHDR)
check_symbol_exists(fcntl "unistd.h;fcntl.h" HAVE_FCNTL)
check_symbol_exists(getexecname "stdlib.h" HAVE_GETEXECNAME)
check_symbol_exists(lstat "sys/types.h;sys/stat.h;unistd.h" HAVE_LSTAT)
check_symbol_exists(readlink "unistd.h" HAVE_READLINK)
check_symbol_exists(mmap "sys/mman.h" HAVE_MMAP)

check_include_file(inttypes.h HAVE_INTTYPES_H)
check_include_file(dlfcn.h HAVE_DLFCN_H)
check_include_file(link.h HAVE_LINK_H)
check_include_file(memory.h HAVE_MEMORY_H)
check_include_file(strings.h HAVE_STRINGS_H)
check_include_file(stdlib.h HAVE_STDLIB_H)
check_include_file(stdint.h HAVE_STDINT_H)

check_library_exists(z compress "" HAS_LIBZ)

# To generate libffi headers we need to know the sizes of various types.
check_type_size("double" SIZEOF_DOUBLE)
check_type_size("long double" HAVE_LONG_DOUBLE)
check_type_size("long double" SIZEOF_LONG_DOUBLE)
check_type_size("void*" SIZEOF_VOID_P)

# Check for various include files
check_include_file(alloca.h HAVE_ALLOCA_H)
check_include_file(inttypes.h HAVE_INTTYPES_H)
check_include_file(stdint.h HAVE_STDINT_H)

# Check for various functions
check_function_exists(alloca HAVE_ALLOCA)
check_function_exists(memcpy HAVE_MEMCPY)

