/* config.h.cmake */

/* ELF size: 32 or 64 */
#define BACKTRACE_ELF_SIZE ${BACKTRACE_ELF_SIZE}

/* XCOFF size: 32 or 64 */
#cmakedefine BACKTRACE_XCOFF_SIZE

/* Define to 1 if you have the __atomic functions */
#cmakedefine01 HAVE_ATOMIC_FUNCTIONS

/* Define to 1 if you have the `clock_gettime' function. */
#cmakedefine01 HAVE_CLOCK_GETTIME

/* Define to 1 if you have the declaration of `strnlen', and to 0 if you
   don't. */
#cmakedefine01 HAVE_DECL_STRNLEN

/* Define to 1 if you have the <dlfcn.h> header file. */
#cmakedefine01 HAVE_DLFCN_H

/* Define if dl_iterate_phdr is available. */
#cmakedefine01 HAVE_DL_ITERATE_PHDR

/* Define to 1 if you have the fcntl function */
#cmakedefine01 HAVE_FCNTL

/* Define if getexecname is available. */
#cmakedefine01 HAVE_GETEXECNAME

/* Define if _Unwind_GetIPInfo is available. */
#cmakedefine01 HAVE_GETIPINFO

/* Define to 1 if you have the <inttypes.h> header file. */
#cmakedefine01 HAVE_INTTYPES_H

/* Define to 1 if you have the `z' library (-lz). */
#cmakedefine01 HAVE_LIBZ

/* Define to 1 if you have the <link.h> header file. */
#cmakedefine01 HAVE_LINK_H

/* Define if AIX loadquery is available. */
#cmakedefine01 HAVE_LOADQUERY

/* Define to 1 if you have the `lstat' function. */
#cmakedefine01 HAVE_LSTAT

/* Define to 1 if you have the <memory.h> header file. */
#cmakedefine01 HAVE_MEMORY_H

/* Define to 1 if you have the `readlink' function. */
#cmakedefine01 HAVE_READLINK

/* Define to 1 if you have the <stdint.h> header file. */
#cmakedefine01 HAVE_STDINT_H

/* Define to 1 if you have the <stdlib.h> header file. */
#cmakedefine01 HAVE_STDLIB_H

/* Define to 1 if you have the <strings.h> header file. */
#cmakedefine01 HAVE_STRINGS_H

/* Define to 1 if you have the <string.h> header file. */
#cmakedefine01 HAVE_STRING_H

/* Define to 1 if you have the __sync functions */
#cmakedefine01 HAVE_SYNC_FUNCTIONS

/* Define to 1 if you have the <sys/ldr.h> header file. */
#cmakedefine01 HAVE_SYS_LDR_H

/* Define to 1 if you have the <sys/mman.h> header file. */
#cmakedefine01 HAVE_SYS_MMAN_H

/* Define to 1 if you have the <sys/stat.h> header file. */
#cmakedefine01 HAVE_SYS_STAT_H

/* Define to 1 if you have the <sys/types.h> header file. */
#cmakedefine01 HAVE_SYS_TYPES_H

/* Define to 1 if you have the <unistd.h> header file. */
#cmakedefine01 HAVE_UNISTD_H

/* Define if -lz is available. */
#cmakedefine01 HAVE_ZLIB

