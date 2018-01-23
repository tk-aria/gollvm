
# compute_int_expr_at_compile_time
#
# Try to compute the value of an integer expression at compile
# time (usually a sizeof or offsetof). Uses a couple of different
# strategies. 
#
# Unnamed parameters:
#
#   * return variable name (set in parent scope)
#   * expr we want to compute
#   * headers to include
#   * temporary *.c file to emit
#
function(compute_expr_compile_time outvar expr hdrs tmpfile)
  set(${outvar} -1 PARENT_SCOPE)

  # Emit a C++ source file that will generate an error when compiled;
  # crafted such that the desired bit of info is part of the error
  # message. The duplicate case construct works with clang; the
  # template instantiation trick works with g++.
  set(tmpfile "${CMAKE_CURRENT_BINARY_DIR}/${tmpfile}")
  file(REMOVE ${tmpfile})
  file(WRITE ${tmpfile} "// auto-generated throwaway\n")
  foreach(inc ${hdrs})
    file(APPEND ${tmpfile} "#include <${inc}>\n")
  endforeach()
  file(APPEND ${tmpfile} "unsigned variant1(unsigned x) {\n")
  file(APPEND ${tmpfile} "  switch(x) {\n")
  file(APPEND ${tmpfile} "    case ${expr}: return 0;\n")
  file(APPEND ${tmpfile} "    case ${expr}: return 1;\n")
  file(APPEND ${tmpfile} "    default: return 2;\n")
  file(APPEND ${tmpfile} "  }\n")
  file(APPEND ${tmpfile} "}\n")
  file(APPEND ${tmpfile} "template<int s> struct Blat;\n")
  file(APPEND ${tmpfile} "Blat<${expr}> variant2;\n")
  file(APPEND ${tmpfile} "int main(int argc, char **argv) {\n")
  file(APPEND ${tmpfile} "  return variant1((unsigned)argc) != 0 ? 1 : 0;\n")
  file(APPEND ${tmpfile} "}\n")

  # Compile it.
  try_compile(unusedvar
    ${CMAKE_CURRENT_BINARY_DIR}
    SOURCES ${tmpfile}
    OUTPUT_VARIABLE errout)

  # Look for duplicate case error message (first variant)
  string(REGEX MATCH "^.*duplicate case value '([0-9]+)'.*$" matched ${errout})
  if(matched)
    set(${outvar} "${CMAKE_MATCH_1}" PARENT_SCOPE)
  else()

    # Look for template instantiation error message (second variant)
    string(REGEX MATCH "^.*Blat<([0-9]+)>.*incomplete type.*$"
      matched ${errout})
    if(matched)
      set(${outvar} "${CMAKE_MATCH_1}" PARENT_SCOPE)
    endif()
  endif()
endfunction()

# compute_struct_size_at_compile_time
#
# Computes the size of a struct at compile time and returns it in the
# specified variable. Return is -1 if size could not be determined.
#
# Unnamed parameters:
#
#   * return variable name (set in parent scope)
#   * struct whose size we want to determine
#   * headers to include
#   * temporary *.cpp file to emit
#
function(compute_struct_size_at_compile_time outvar struct hdrs tmpfile)
  set(expr "sizeof(${struct})")
  compute_expr_compile_time(result "${expr}" "${hdrs}" ${tmpfile})
  set(${outvar} ${result} PARENT_SCOPE)
endfunction()

# compute_field_offset_at_compile_time
#
# Computes the offset of a field within a struct at compile time and
# returns it in the specified variable. Return is -1 if size could not
# be determined.
#
# Unnamed parameters:
#
#   * return variable name (set in parent scope)
#   * struct whose size we want to determine
#   * headers to include
#   * temporary *.cpp file to emit
#
function (compute_field_offset_at_compile_time outvar struct field hdrs tmpfile)
  set(expr "offsetof(${struct},${field})")
  compute_expr_compile_time(result "${expr}" "${hdrs}" ${tmpfile})
  set(${outvar} ${result} PARENT_SCOPE)
endfunction()


