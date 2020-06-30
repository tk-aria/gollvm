# CMake support related to building libffi.

include(CheckTypeSize)
include(CheckFunctionExists)

function(setup_libffi libffi_srcroot)

  if(${llarch} STREQUAL "aarch64")
    set(arch_dir "aarch64")
  elseif(${llarch} STREQUAL "x86_64")
    set(arch_dir "x86")
  else()
    message(SEND_ERROR "Arch ${llarch} not yet supported")
  endif()


  # FIXME: currently hard-coded
  set(VERSION "3.99999")

  set(STDC_HEADERS 1)

  # FIXME: hoist this up into a common config-checking section (combine
  # with similar check in libbacktrace cmake).
  # For the moment, require that the target system supports mmap.
  check_symbol_exists(mmap "sys/mman.h" HAVE_MMAP)
  if(NOT HAVE_MMAP)
    message(SEND_ERROR "Support for mmap() is required -- setup failed.")
  else()
    set(HAVE_MMAP_ANON 1)
    set(HAVE_MMAP_FILE 1)
  endif()

  # Is this 32-bit or 64-bit x86?
  if(SIZEOF_VOID_P STREQUAL 4)
    set(HAVE_64BIT 0)
  else()
    set(HAVE_64BIT 1)
    if(NOT ${llarch} STREQUAL "aarch64")
      set(HAVE_AS_X86_64_UNWIND_SECTION_TYPE 1)
    endif()
  endif()

  # Pick up correct sources based on arch.
  set(c_srcs
    "${libffi_srcroot}/src/closures.c"
    "${libffi_srcroot}/src/prep_cif.c"
    "${libffi_srcroot}/src/types.c"
    "${libffi_srcroot}/src/raw_api.c"
    "${libffi_srcroot}/src/java_raw_api.c"
    "${libffi_srcroot}/src/${arch_dir}/ffi.c")

    set(asm_srcs "${libffi_srcroot}/src/${arch_dir}/sysv.S")

  if(HAVE_64BIT AND ${llarch} STREQUAL "x86_64")
    list(APPEND c_srcs "${libffi_srcroot}/src/x86/ffi64.c" "${libffi_srcroot}/src/x86/ffiw64.c")
    list(APPEND asm_srcs "${libffi_srcroot}/src/x86/unix64.S")
    list(APPEND asm_srcs "${libffi_srcroot}/src/x86/win64.S")
  endif()
  set_source_files_properties(${asm_srcs} PROPERTIES LANGUAGE C)

  # Set target based on arch.
  if(HAVE_64BIT AND ${llarch} STREQUAL "aarch64")
    set(TARGET AARCH64)
  elseif(HAVE_64BIT)
    set(TARGET X86_64)
  else()
    set(TARGET X86)
  endif()

  # Misc x86 and llvm setup.
  set(HAVE_AS_REGISTER_PSEUDO_OP 0)
  set(HAVE_AS_CFI_PSEUDO_OP 1)
  set(HAVE_AS_X86_PCREL 1)
  set(HAVE_RO_EH_FRAME 1)
  set(HAVE_HIDDEN_VISIBILITY_ATTRIBUTE 1)

  # These are options for FFI that we want to hard-wire on
  set(FFI_CLOSURES 1)
  set(FFI_GO_CLOSURES 1)
  set(FFI_DEBUG off)
  set(FFI_EXEC_TRAMPOLINE_TABLE 0)
  set(FFI_MMAP_EXEC_WRIT 0)

  set(libffiflags "-g")
  if(GOLLVM_USE_SPLIT_STACK)
    string(APPEND libffiflags " -fsplit-stack ${CFPROTECTION_WORKAROUND}")
  endif()
  string(APPEND libffiflags " ${GOLLVM_EXTRA_CFLAGS}")

  # Copy correct version of ffitarget.h to libgo binary root.
  file(COPY "${libffi_srcroot}/src/${arch_dir}/ffitarget.h" DESTINATION ${CMAKE_CURRENT_BINARY_DIR})

  # At this point we can generate the necessary headers.
  configure_file("${CMAKE_CURRENT_SOURCE_DIR}/fficonfig.h.cmake" "${CMAKE_CURRENT_BINARY_DIR}/fficonfig.h")
  configure_file ("${libffi_srcroot}/include/ffi.h.in" "${CMAKE_CURRENT_BINARY_DIR}/ffi.h")

  # Create libffi object libraries
  add_library(libffi_nonpiclib OBJECT EXCLUDE_FROM_ALL ${c_srcs} ${asm_srcs})
  set_target_properties(libffi_nonpiclib PROPERTIES COMPILE_FLAGS "-fno-PIC ${libffiflags}")
  target_include_directories(libffi_nonpiclib PUBLIC "${libffi_srcroot}/include")

  add_library(libffi_piclib OBJECT EXCLUDE_FROM_ALL ${c_srcs} ${asm_srcs})
  set_target_properties(libffi_piclib PROPERTIES COMPILE_FLAGS "-fPIC ${libffiflags}")
  target_include_directories(libffi_piclib PUBLIC "${libffi_srcroot}/include")

endfunction()
