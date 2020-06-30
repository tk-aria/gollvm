# CMake support related to building libbacktrace.

function(setup_libbacktrace)

  set(BACKTRACE_SUPPORTED 1)

  # Libbacktrace needs to be told ELF-32 or ELF-64.
  if( ${goarch} STREQUAL "amd64")
    set(BACKTRACE_ELF_SIZE 64)
    set(HAVE_GETIPINFO 1)
  elseif( ${goarch} STREQUAL "arm64")
    set(BACKTRACE_ELF_SIZE 64)
    set(HAVE_GETIPINFO 1)
  else()
    message(SEND_ERROR "Libbacktrace config setup not implemented for ${goarch}")
  endif()

  if(NOT HAVE_MMAP)
    message(SEND_ERROR "Support for mmap() is required -- setup failed.")
  endif()
  set(BACKTRACE_USES_MALLOC 0)

  # Support for pthreads is similarly required. HAVE_LIBPTHREAD is normally
  # set up by the main LLVM cmake config support.
  if(NOT HAVE_LIBPTHREAD)
    message(SEND_ERROR "Support for pthreads is required -- setup failed.")
  endif()
  set(BACKTRACE_SUPPORTS_THREADS 1)

  # Assume ELF/DWARF, meaning that BACKTRACE_SUPPORTS_DATA is hard-coded on.
  set(BACKTRACE_SUPPORTS_DATA 1)

  # We already tested for syncs at the parent level
  if(HAVE_SYNC_BOOL_COMPARE_AND_SWAP_4)
    if(HAVE_SYNC_BOOL_COMPARE_AND_SWAP_8)
      set(HAVE_SYNC_FUNCTIONS 1)
    endif()
  endif()

  # Generate backtrace-supported.h based on the above.
  configure_file(
    ${CMAKE_CURRENT_SOURCE_DIR}/backtrace-supported.h.cmake
    ${libgo_binroot}/backtrace-supported.h)

  file(MAKE_DIRECTORY "${libgo_binroot}/libbacktrace")

  # Generate libbacktrace-specific config.h based on the above.
  configure_file(
    ${CMAKE_CURRENT_SOURCE_DIR}/backtrace-config.h.cmake
    ${libgo_binroot}/libbacktrace/config.h)

  # libbacktrace sources. 
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

  set(libbacktraceflags "-g")
  if(GOLLVM_USE_SPLIT_STACK)
    string(APPEND libbacktraceflags " -fsplit-stack ${CFPROTECTION_WORKAROUND}")
  endif()
  string(APPEND libbacktraceflags " ${GOLLVM_EXTRA_CFLAGS}")

  # Object libraries built from libbacktrace sources.
  # Note: this build uses -fsplit-stack, whereas in the gccgo
  # libgo it does not. In theory this should be a good thing.

  # nonpic
  add_library(libbacktrace_nonpiclib
    OBJECT EXCLUDE_FROM_ALL ${libbacktracecfiles})
  set_target_properties(libbacktrace_nonpiclib
    PROPERTIES COMPILE_FLAGS "-fno-PIC ${libbacktraceflags}")
  target_include_directories(libbacktrace_nonpiclib
    BEFORE PRIVATE "${libgo_binroot}/libbacktrace")

  # pic
  add_library(libbacktrace_piclib
    OBJECT EXCLUDE_FROM_ALL ${libbacktracecfiles})
  target_include_directories(libbacktrace_piclib
    BEFORE PRIVATE "${libgo_binroot}/libbacktrace")
  set_target_properties(libbacktrace_piclib
    PROPERTIES COMPILE_FLAGS "-fPIC ${libbacktraceflags}")

endfunction()
