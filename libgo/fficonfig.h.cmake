/* fficonfig.h.in.  Generated from configure.ac by autoheader.  */

/* Define if building universal (internal helper macro) */
#cmakedefine AC_APPLE_UNIVERSAL_BUILD

/* Define to one of `_getb67', `GETB67', `getb67' for Cray-2 and Cray-YMP
   systems. This function is required for `alloca.c' support on those systems.
   */
#cmakedefine CRAY_STACKSEG_END

/* Define to 1 if using `alloca.c'. */
#cmakedefine C_ALLOCA

/* Define to the flags needed for the .section .eh_frame directive. */
#cmakedefine EH_FRAME_FLAGS "@EH_FRAME_FLAGS@"

/* Define this if you want extra debugging. */
#cmakedefine FFI_DEBUG

/* Cannot use malloc on this target, so, we revert to alternative means */
#cmakedefine FFI_MMAP_EXEC_WRIT 1

/* Define this is you do not want support for the raw API. */
#cmakedefine FFI_NO_RAW_API

/* Define this is you do not want support for aggregate types. */
#cmakedefine FFI_NO_STRUCTS

/* Define to 1 if you have `alloca', as a function or macro. */
#cmakedefine HAVE_ALLOCA 1

/* Define to 1 if you have <alloca.h> and it should be used (not on Ultrix). */
#cmakedefine HAVE_ALLOCA_H 1

/* Define if your assembler supports .cfi_* directives. */
#cmakedefine HAVE_AS_CFI_PSEUDO_OP 1

/* Define if your assembler supports .register. */
#cmakedefine HAVE_AS_REGISTER_PSEUDO_OP 1

/* Define if your assembler and linker support unaligned PC relative relocs. */
#cmakedefine HAVE_AS_SPARC_UA_PCREL 1

/* Define if your assembler supports PC relative relocs. */
#cmakedefine HAVE_AS_X86_PCREL 1

/* Define to 1 if you have the <dlfcn.h> header file. */
#cmakedefine HAVE_DLFCN_H 1

/* Define if __attribute__((visibility("hidden"))) is supported. */
#cmakedefine HAVE_HIDDEN_VISIBILITY_ATTRIBUTE 1

/* Define to 1 if you have the <inttypes.h> header file. */
#cmakedefine HAVE_INTTYPES_H 1

/* Define if you have the long double type and it is bigger than a double */
#cmakedefine HAVE_LONG_DOUBLE 1

/* Define to 1 if you have the `memcpy' function. */
#cmakedefine HAVE_MEMCPY 1

/* Define to 1 if you have the <memory.h> header file. */
#cmakedefine HAVE_MEMORY_H 1

/* Define to 1 if you have the `mmap' function. */
#cmakedefine HAVE_MMAP 1

/* Define if mmap with MAP_ANON(YMOUS) works. */
#cmakedefine HAVE_MMAP_ANON 1

/* Define if mmap of /dev/zero works. */
#cmakedefine HAVE_MMAP_DEV_ZERO 1

/* Define if read-only mmap of a plain file works. */
#cmakedefine HAVE_MMAP_FILE 1

/* Define if .eh_frame sections should be read-only. */
#cmakedefine HAVE_RO_EH_FRAME 1

/* Define to 1 if you have the <stdint.h> header file. */
#cmakedefine HAVE_STDINT_H 1

/* Define to 1 if you have the <stdlib.h> header file. */
#cmakedefine HAVE_STDLIB_H 1

/* Define to 1 if you have the <strings.h> header file. */
#cmakedefine HAVE_STRINGS_H 1

/* Define to 1 if you have the <string.h> header file. */
#cmakedefine HAVE_STRING_H 1

/* Define to 1 if you have the <sys/mman.h> header file. */
#cmakedefine HAVE_SYS_MMAN_H 1
 
/* Define to 1 if you have the <sys/stat.h> header file. */
#cmakedefine HAVE_SYS_STAT_H 1

/* Define to 1 if you have the <sys/types.h> header file. */
#cmakedefine HAVE_SYS_TYPES_H 1

/* Define to 1 if you have the <unistd.h> header file. */
#cmakedefine HAVE_UNISTD_H 1

/* Define to the sub-directory in which libtool stores uninstalled libraries.
   */
#define LT_OBJDIR ".libs/"

/* Define to 1 if your C compiler doesn't accept -c and -o together. */
#cmakedefine NO_MINUS_C_MINUS_O

/* Name of package */
#define PACKAGE "libffi"

/* Define to the address where bug reports for this package should be sent. */
#define PACKAGE_BUGREPORT "http://gcc.gnu.org/bugs.html"

/* Define to the full name of this package. */
#define PACKAGE_NAME "libffi"

/* Define to the full name and version of this package. */
#define PACKAGE_STRING "libffi 3.0.9"

/* Define to the one symbol short name of this package. */
#define PACKAGE_TARNAME "libffi"

/* Define to the version of this package. */
#define PACKAGE_VERSION "@VERSION@"

/* The size of `double', as computed by sizeof. */
#cmakedefine SIZEOF_DOUBLE @SIZEOF_DOUBLE@

/* The size of `long double', as computed by sizeof. */
#cmakedefine SIZEOF_LONG_DOUBLE @SIZEOF_LONG_DOUBLE@

/* If using the C implementation of alloca, define if you know the
   direction of stack growth for your system; otherwise it will be
   automatically deduced at runtime.
	STACK_DIRECTION > 0 => grows toward higher addresses
	STACK_DIRECTION < 0 => grows toward lower addresses
	STACK_DIRECTION = 0 => direction of growth unknown */
#cmakedefine STACK_DIRECTION

/* Define to 1 if you have the ANSI C header files. */
#cmakedefine STDC_HEADERS 1

/* Define this if you are using Purify and want to suppress spurious messages.
   */
#cmakedefine USING_PURIFY

/* Version number of package */
#define VERSION "3.0.9"

/* Define WORDS_BIGENDIAN to 1 if your processor stores words with the most
   significant byte first (like Motorola and SPARC, unlike Intel). */
#if defined AC_APPLE_UNIVERSAL_BUILD
# if defined __BIG_ENDIAN__
#  define WORDS_BIGENDIAN 1
# endif
#else
# ifndef WORDS_BIGENDIAN
#  undef WORDS_BIGENDIAN
# endif
#endif


#ifdef HAVE_HIDDEN_VISIBILITY_ATTRIBUTE
#ifdef LIBFFI_ASM
#define FFI_HIDDEN(name) .hidden name
#else
#define FFI_HIDDEN __attribute__ ((visibility ("hidden")))
#endif
#else
#ifdef LIBFFI_ASM
#define FFI_HIDDEN(name)
#else
#define FFI_HIDDEN
#endif
#endif
