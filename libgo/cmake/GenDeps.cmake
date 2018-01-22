
# This function is a reimplementation of gendeps.sh in cmake.
# 
# TODO: remove this, it is no longer used.  In practice this winds up
# creating a giant amount of output/clutter when using cmake --trace,
# and doesn't seem to really offer a lot of other advantages. 

function(gendeps_in_cmake pack)
  set(deps)
  foreach( src ${ARGV})
    set(impblock 0)
    file(STRINGS ${src} lines)
    foreach( line ${lines})
      # Within an import block:
      if(impblock)
	string(REGEX MATCH "^\\)" endblock ${line})
	if(endblock)
  	  set(impblock 0)
	  break()
	endif()
	string(REGEX MATCH "^[ \t]*[A-Za-z0-9_\.]*[ \t]*\"([^\"]+)\".*"
	       imp ${line})
	if(imp)
	  list(APPEND deps "${CMAKE_MATCH_1}.gox")
	endif()
	continue()
      endif()

      # Single import outside of an import block.
      string(REGEX MATCH "^import[ \t]*[A-Za-z0-9_\.]*[ \t]*\"([^\"]+)\"" imp ${line})
      if(imp)
	list(APPEND deps "${CMAKE_MATCH_1}.gox")
	continue()
      endif()

      # Start of import block
      string(REGEX MATCH "^import[ \t]+\\\(" beginblock ${line})
      if(beginblock)
	set(impblock 1)
	continue()
      endif()
      
    endforeach()
    if(deps)
      list(SORT deps)
      list(REMOVE_DUPLICATES deps)
    endif()
  endforeach()

  # Prune out "unsafe"
  list(FIND deps "unsafe.gox" hasun)
  if( hasun GREATER -1)
    list(REMOVE_ITEM deps "unsafe.gox")
  endif()

  # Done
  set(packdeps ${deps} PARENT_SCOPE)
endfunction()

# This version runs the godeps.sh shell script and captures the
# resulting output. Return value is in the variable 'packdeps'
# in the parent scope.

# Unnamed parameters:
#
#   * Package path, e.g. "bufio" or "container/heap".
#   * Script root (where to find godeps.sh)
#
# Named parameters:
#
# SOURCES   Full paths of go source files in this package.

function(gendeps pack libgo_scriptroot)
  CMAKE_PARSE_ARGUMENTS(ARG "" "" "SOURCES" ${ARGN})

  set(godepsdotsh "${libgo_scriptroot}/godeps.sh")
  set(capturesh "${CMAKE_CURRENT_SOURCE_DIR}/capturescript.sh")

  string(REPLACE "/" "_" ptarget "${pack}")
  set(depouttmp "${libgo_binroot}/${ptarget}.dep")
  string(REPLACE ";" " " packfiles "${ARG_SOURCES}")
  set(packfiles "unused ${packfiles}")
  set(shell $ENV{SHELL})

  # Kick off the script.
  execute_process(COMMAND "/bin/sh" ${capturesh} ${godepsdotsh} ${depouttmp} ${packfiles})

  # Read the result.
  file(STRINGS ${depouttmp} depoutput)
  separate_arguments(depoutput)

  # Gendeps.sh produces output of the form:
  # <pack>.dep: file.go file2.go ... pack1.gox pack2.gox ...
  # We're interested in the gox files.
  set(packdepstmp)
  foreach( item ${depoutput})
    string(REGEX MATCH "^.+\.gox$" gox ${item})
    if(gox)
      list(APPEND packdepstmp ${item})
    endif()
  endforeach()

  #message(STATUS "deps for ${pack}: ${packdepstmp}")

  set(packdeps ${packdepstmp} PARENT_SCOPE)
endfunction()
