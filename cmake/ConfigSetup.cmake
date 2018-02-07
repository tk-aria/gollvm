
# Small atomic test case.
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

# Tests for things that libgo needs.
check_symbol_exists(setenv "stdlib.h" HAVE_SETENV)
check_symbol_exists(splice "fcntl.h" HAVE_SPLICE)
check_symbol_exists(strerror_r "string.h" HAVE_STRERROR_R)
check_symbol_exists(syscall "unistd.h" HAVE_SYSCALL)
check_symbol_exists(tee "fcntl.h" HAVE_TEE)
check_symbol_exists(openat "fcntl.h" HAVE_OPENAT)
list(APPEND CMAKE_REQUIRED_DEFINITIONS "-D_LARGEFILE64_SOURCE=1")
check_symbol_exists(open64 "fcntl.h" HAVE_OPEN64)
check_symbol_exists(mknodat "sys/types.h;sys/stat.h;fcntl.h;unistd.h" HAVE_MKNODAT)
check_symbol_exists(pipe2 "unistd.h" HAVE_PIPE2)

# Checks for include files
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

check_c_source_compiles("int main(int argc, char **argv) { return __sync_bool_compare_and_swap(&argc, argc, -argc) ? 1 : 0; }\n" HAVE_SYNC_BOOL_COMPARE_AND_SWAP_4)
check_c_source_compiles("#include <stdint.h>\nstatic uint64_t s;\nint main(int argc, char **argv) { return __sync_bool_compare_and_swap(&s, argc, -argc) ? 0 : 1; }\n" HAVE_SYNC_BOOL_COMPARE_AND_SWAP_8)

check_c_source_compiles("int main(int argc, char **argv) { return __sync_fetch_and_add(&argc, 1); }\n" HAVE_SYNC_FETCH_AND_ADD_4)
check_c_source_compiles("#include <stdint.h>\nstatic uint64_t s;\nint main(int argc, char **argv) { return __sync_add_and_fetch(&s, 1) == s ? 0 : 1; }\n" HAVE_SYNC_ADD_AND_FETCH_8)

# Issue an error if the C compiler doesn't support -fsplit-stack
# (in theory you can build libgo without it, so I suppose this could
# be changed to a warning).
check_c_compiler_flag("-fsplit-stack" C_SUPPORTS_SPLIT_STACK)
if(NOT C_SUPPORTS_SPLIT_STACK)
  message(SEND_ERROR "C compiler does not support -fsplit-stack")
endif()
set(USING_SPLIT_STACK 1)
set(USE_LIBFFI 1)
