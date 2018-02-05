
# Gotools build requires libgo; if libgo is stubbed out then don't
# try to build gotools either.

if(DISABLE_LIBGO_BUILD)
  return()
endif()

include(GoProgram)

message(STATUS "starting gotools configuration.")

# FIXME: rewrite this to locate llvm-goparse in a more official way
# set(driver "${bin}/../bin/llvm-goparse")
get_target_property(driverdir llvm-goparse RUNTIME_OUTPUT_DIRECTORY)
set(gollvm_driver "${driverdir}/llvm-goparse")
#set(gocompiler ${gollvm_driver})

# FIXME: still need to use wrapper for compilation, since
# llvm-goparse functionality not yet complete.
set(gocompiler "gccgo")

set(cmd_srcroot "${GOLLVM_SOURCE_DIR}/gofrontend/libgo/go/cmd")

set(gotools_binroot "${CMAKE_CURRENT_BINARY_DIR}")
set(libgo_binroot "${CMAKE_CURRENT_BINARY_DIR}/../libgo")

set(libgotool_archive "${libgo_binroot}/libgotool.a")

# Cgo needs a copy of the defaultCC function.
set(cgozdefaultccdotgo "${gotools_binroot}/zdefaultcc.go")
set(cgozdefaultcctmp "${gotools_binroot}/zdefaultcc.go.tmp")
mkzdefaultcc("main" ${cgozdefaultcctmp} ${gollvm_driver}
             ${CMAKE_C_COMPILER} ${CMAKE_CXX_COMPILER})
copy_if_different(${cgozdefaultcctmp} ${cgozdefaultccdotgo})

set(go_cmd_go_files
  "${cmd_srcroot}/go/alldocs.go"
  "${cmd_srcroot}/go/go11.go"
  "${cmd_srcroot}/go/main.go"
  )

set(go_cmd_gofmt_files
  "${cmd_srcroot}/gofmt/doc.go" 
  "${cmd_srcroot}/gofmt/gofmt.go"
  "${cmd_srcroot}/gofmt/internal.go"
  "${cmd_srcroot}/gofmt/rewrite.go"
  "${cmd_srcroot}/gofmt/simplify.go"
  )

set(go_cmd_cgo_files
  "${cmd_srcroot}/cgo/ast.go"
  "${cmd_srcroot}/cgo/doc.go"
  "${cmd_srcroot}/cgo/gcc.go"
  "${cmd_srcroot}/cgo/godefs.go"
  "${cmd_srcroot}/cgo/main.go"
  "${cmd_srcroot}/cgo/out.go"
  "${cmd_srcroot}/cgo/util.go"
  "${cgozdefaultccdotgo}"
  )

set(go_cmd_vet_files 
  "${cmd_srcroot}/vet/asmdecl.go"
  "${cmd_srcroot}/vet/assign.go"
  "${cmd_srcroot}/vet/atomic.go"
  "${cmd_srcroot}/vet/bool.go"
  "${cmd_srcroot}/vet/buildtag.go"
  "${cmd_srcroot}/vet/cgo.go"
  "${cmd_srcroot}/vet/composite.go"
  "${cmd_srcroot}/vet/copylock.go"
  "${cmd_srcroot}/vet/deadcode.go"
  "${cmd_srcroot}/vet/dead.go"
  "${cmd_srcroot}/vet/doc.go"
  "${cmd_srcroot}/vet/httpresponse.go"
  "${cmd_srcroot}/vet/lostcancel.go"
  "${cmd_srcroot}/vet/main.go"
  "${cmd_srcroot}/vet/method.go"
  "${cmd_srcroot}/vet/nilfunc.go"
  "${cmd_srcroot}/vet/print.go"
  "${cmd_srcroot}/vet/rangeloop.go"
  "${cmd_srcroot}/vet/shadow.go"
  "${cmd_srcroot}/vet/shift.go"
  "${cmd_srcroot}/vet/structtag.go"
  "${cmd_srcroot}/vet/tests.go"
  "${cmd_srcroot}/vet/types.go"
  "${cmd_srcroot}/vet/unsafeptr.go"
  "${cmd_srcroot}/vet/unused.go"
)

set(go_cmd_buildid_files 
  "${cmd_srcroot}/buildid/buildid.go"
  "${cmd_srcroot}/buildid/doc.go"
  )

set(go_cmd_test2json_files
  "${cmd_srcroot}/test2json/main.go"
  )

set(allgotools)

add_go_program("go" gotools_cmd_go
  ${libgo_binroot} ${gotools_binroot}
  GOSRC "${go_cmd_go_files}"
  GOLIB ${libgotool_archive}
  GODEP libgotool libgo_shared
  )
list(APPEND allgotools gotools_cmd_go)

add_go_program("gofmt" gotools_cmd_gofmt
  ${libgo_binroot} ${gotools_binroot}
  GOSRC "${go_cmd_gofmt_files}"
  GOLIB ${libgotool_archive}
  GODEP libgotool libgo_shared
  )
list(APPEND allgotools gotools_cmd_gofmt)

add_go_program("cgo" gotools_cmd_cgo ${libgo_binroot} ${gotools_binroot}
  GOSRC "${go_cmd_cgo_files}"
  GOLIB ${libgotool_archive}
  GODEP libgotool libgo_shared
  )
list(APPEND allgotools gotools_cmd_cgo)

add_go_program("vet" gotools_cmd_vet
  ${libgo_binroot} ${gotools_binroot}
  GOSRC "${go_cmd_vet_files}"
  GOLIB ${libgotool_archive}
  GODEP libgotool libgo_shared
  )
list(APPEND allgotools gotools_cmd_vet)

add_go_program("buildid" gotools_cmd_buildid
  ${libgo_binroot} ${gotools_binroot}
  GOSRC "${go_cmd_buildid_files}"
  GOLIB ${libgotool_archive}
  GODEP libgotool libgo_shared
  )
list(APPEND allgotools gotools_cmd_buildid)

add_go_program("test2json" gotools_cmd_test2json
  ${libgo_binroot} ${gotools_binroot}
  GOSRC "${go_cmd_test2json_files}"
  GOLIB ${libgotool_archive}
  GODEP libgotool libgo_shared
  )
list(APPEND allgotools gotools_cmd_test2json)

add_custom_target(gotools_all DEPENDS ${allgotools})

# FIXME: add install rules

message(STATUS "gotools configuration complete.")

