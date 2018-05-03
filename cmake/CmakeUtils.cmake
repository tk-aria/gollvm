
# Helper function to copy file X to file Y if X and Y are different.
#
# Unnamed parameters:
#
#   * src file path
#   * target file path
#
# Named parameters:
#
# COMMENT  comment string to use other than the default
function(copy_if_different inpath outpath)
  CMAKE_PARSE_ARGUMENTS(ARG "" "COMMENT" "" ${ARGN})
  get_filename_component(infile "${inpath}" NAME)
  get_filename_component(outfile "${outpath}" NAME)
  set(comment "Updating ${outfile} from ${infile}")
  if(NOT ARG_COMMENT)
    set(comment "${ARG_COMMENT}")
  endif()
  add_custom_command(
    OUTPUT "${outpath}"
    COMMAND  ${CMAKE_COMMAND} -E copy_if_different
       ${inpath} ${outpath}
    DEPENDS ${inpath}
    COMMENT "${comment}"
    VERBATIM)
endfunction()

