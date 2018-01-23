# CMake support related to building libbacktrace. 

function(setup_libbacktrace)

  set(BACKTRACE_SUPPORTED 1)

  # Libbacktrace needs to be told ELF-32 or ELF-64.
  if( ${goarch} STREQUAL "amd64")
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

endfunction()
