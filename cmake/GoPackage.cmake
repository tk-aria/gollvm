
# This function adds targets for a specified package within the
# Go standard library. This will include the *.gox file for the package,
# as well as targets for PIC and non-PIC objects. Example usage:
#
# Example usage:
#
# add_go_package(pkgpath <dest>
#                GOSRC .../x.go .../y.go ... GODEP a.gox b.gox ...)
#
# Unnamed parameters:
#
#   * Package path, e.g. "bufio" or "container/heap".
#   * Destination directory for libgo build artifacts
#
# Named parameters:
#
# GOSRC     Full paths of go source files to build.
# GODEP     Names of export files for packages this package depends on.
# GOCFLAGS  Additional arguments passed to Go compiler.
# NOPIC     Don't bother to produce a PIC object, create only non-PIC.

function(add_go_package pkgpath dest)
  CMAKE_PARSE_ARGUMENTS(ARG "NOPIC" "" "GOSRC;GODEP;GOCFLAGS" ${ARGN})

  # Split package path into name and containing_dir
  get_filename_component(pdir "${pkgpath}" DIRECTORY)
  get_filename_component(name "${pkgpath}" NAME)
  if( "${pdir}" STREQUAL "")
    set(pdir ".")
  endif()

  # Note: currently no 32-bit support, need to revisit once we have -m32

  # Dependent packages (*.gox files). The fully qualified paths are
  # needed primarily for correct dependency generation; the actual
  # export data for packages is read from *.o files picked up
  # implicitly via the "-I ." passed to the driver.
  set(godeps)
  foreach( godep ${ARG_GODEP} )
    list(APPEND godeps "${dest}/${godep}")
  endforeach()

  # Non-PIC object file, PIC object file
  set(package_ofile "${dest}/${pdir}/${name}.o")
  set(package_picofile "${dest}/${pdir}/.pic/${name}.o")

  # Export file, export file temp.
  set(package_goxfile "${dest}/${pdir}/${name}.gox")
  set(package_goxtmp "${dest}/${pdir}/${name}.gox.tmp")

  # All outputs
  set(pkg_outputs)

  # Mangle packagepath (convert golang_org => vendor/golang_org).
  string(REPLACE "golang_org" "vendor/golang_org" pkgpath "${pkgpath}")

  # Command to build non-PIC object file
  add_custom_command(
    OUTPUT ${package_ofile}
    COMMAND ${CMAKE_COMMAND} -E make_directory "./${pdir}"
    COMMAND "${gocompiler}" "-c" "-o" ${package_ofile} "-fgo-pkgpath=${pkgpath}" ${ARG_GOCFLAGS} -I . ${ARG_GOSRC}
    DEPENDS ${ARG_GOSRC} ${godeps}
    COMMENT "Building Go package '${pkgpath}' (non-PIC)"
    VERBATIM)
  list(APPEND pkg_outputs "${package_ofile}")

  # Command to build PIC object file
  if(NOT ARG_NOPIC)
    add_custom_command(
      OUTPUT "${package_picofile}"
      COMMAND ${CMAKE_COMMAND} -E make_directory "./${pdir}/.pic"
      COMMAND "${gocompiler}" "-c" "-o" ${package_picofile} -fPIC "-fgo-pkgpath=${pkgpath}" ${ARG_GOCFLAGS} -I . ${ARG_GOSRC}
      DEPENDS ${ARG_GOSRC} ${godeps}
      COMMENT "Building Go package '${pkgpath}' (PIC)"
      VERBATIM)
    list(APPEND pkg_outputs "${package_picofile}")
  else()
    set(package_picofile)
  endif()

  # Command to build *.gox.tmp
  add_custom_command(
    OUTPUT "${package_goxtmp}"
    COMMAND objcopy -j .go_export "${package_ofile}" "${package_goxtmp}"
    DEPENDS ${package_ofile} ${package_picofile}
    COMMENT "Building Go exports file for package '${pkgpath}'"
    VERBATIM)

  # Command to update *.gox if different from *.gox.tmp
  add_custom_command(
    OUTPUT "${package_goxfile}"
    COMMAND  ${CMAKE_COMMAND} -E copy_if_different
       "${package_goxtmp}" "${package_goxfile}"
    DEPENDS "${package_goxtmp}"
    COMMENT "Updating Go exports file for package '${pkgpath}'"
    VERBATIM)
  list(APPEND pkg_outputs "${package_goxfile}")

  # Target name, using underscores, e.g. "container/heap"
  # creates a target of "libgo_container_heap"
  string(REPLACE "/" "_" ptarget "${pkgpath}")
  set(pkgtarget "libgo_${ptarget}")

  # Create target
  add_custom_target(${pkgtarget} ALL DEPENDS ${pkg_outputs})
  set_target_properties(${pkgtarget} PROPERTIES FOLDER "Object Libraries")

  # Caller needs to know these.
  set(package_ofile ${package_ofile} PARENT_SCOPE)
  set(package_picofile ${package_picofile} PARENT_SCOPE)
  set(package_goxfile ${package_goxfile} PARENT_SCOPE)

  # TODO: add install rules

endfunction()
