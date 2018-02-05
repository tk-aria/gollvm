
# This function adds a target for a specific Go program (executable),
# for example "gofmt" or "cgo". 
#
# Example usage:
#
# add_go_program(name target libgodir destdir
#                GOSRC .../x.go .../y.go ... GOLIB abc.a foo.so ...)
#
# Unnamed parameters:
#
#   * Program name, e.g. "gofmt"
#   * Target name, e.g. "gotools_cmd_go"
#   * Directory containing libgo build artifacts (ex: runtime.o)
#   * Destination directory for program build artifacts (e.g. executable)
#
# Named parameters:
#
# GOSRC     Full paths of go source files to build.
# GOLIB     Libraries to link against. 
# GODEP     Targets on which this program should be dependent.
# GOCFLAGS  Additional arguments passed to Go compiler.

function(add_go_program progname target libgodir destdir)
  CMAKE_PARSE_ARGUMENTS(ARG "" "" "GOSRC;GOLIB;GODEP;GOCFLAGS" ${ARGN})

  # Target of build
  set(program_exe "${destdir}/${progname}")

  # Deps
  set(deps ${ARG_GOSRC})
  list(APPEND deps ${ARG_GODEP})

  # Command to build executable.
  add_custom_command(
    OUTPUT ${program_exe}
    COMMAND "${gocompiler}" "-o" ${program_exe} ${ARG_GOSRC} ${ARG_GOCFLAGS}
            -I ${libgodir} -L ${libgodir} ${ARG_GOLIB}
    DEPENDS ${deps}
    COMMENT "Building go program ${progname}"
    VERBATIM)

  # Create target
  add_custom_target(${target} ALL DEPENDS ${program_exe})
  set_target_properties(${target} PROPERTIES FOLDER "Tools")

  # TODO: add install rules

endfunction()
