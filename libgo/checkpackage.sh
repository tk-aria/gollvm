#!/bin/sh
#
# Invoke the 'gotest' script to run tests for a given Go package. This
# is expected to be run from the subdirectory within the libgo build
# corresponding to the package in question. Command line should look
# like:
#
#  checkpackage.sh \
#     PACKAGE <value> \
#     FILES file1 file2 ... fileN \
#     GOOS <value> \
#     GOARCH <value> \
#     GC <arg> ... \
#     GOLIBS <arg> ... \
#     BASEDIR <dir> \
#     BINDIR <dir> \
#
# Arguments to gotest (ex: --trace --keep) can be passed via the
# GOTESTFLAGS environment variable.
#
CUR=""
for ARG in $*
do
  case "$ARG" in
    ADDTOPATH) CUR=ADDTOPATH ;;
    BASEDIR) CUR=BASEDIR ;;
    BINDIR) CUR=BINDIR ;;
    FILES) CUR=FILES ;;
    GC) CUR=GC ;;
    GOARCH) CUR=GOARCH ;;
    GOLIBS) CUR=GOLIBS ;;
    GOOS) CUR=GOOS ;;
    PACKAGE) CUR=PACKAGE ;;
    *) if [ -z "${CUR}" ]; then
         echo "unexpected stray argument $ARG"
         exit 1
       fi
       eval "CV=\$$CUR"
       if [ -z "$CV" ]; then
         eval "$CUR=\$ARG"
       else
         eval "$CUR=\"$CV $ARG\""
       fi
       ;;
  esac
done
REQUIRED="GOOS GOARCH GC BASEDIR PACKAGE BINDIR"
for R in $REQUIRED
do
  eval "V=\$$R"
  if [ -z "$V" ]; then
    echo "error: no setting for \"$R\" supplied on command line"
    exit 1
  fi
done
export GC
export GOLIBS
export LD_LIBRARY_PATH="${BINDIR}:${LD_LIBRARY_PATH}"
if [ ! -z "${ADDTOPATH}" ]; then
  export PATH=${ADDTOPATH}:${PATH}
fi
#
exec ${SHELL} ${BASEDIR}/testsuite/gotest --goarch=${GOARCH} --goos=${GOOS} --basedir=${BASEDIR} --srcdir=${BASEDIR}/go/${PACKAGE} --pkgpath=${PACKAGE} --pkgfiles="${FILES}" ${GOTESTFLAGS}

