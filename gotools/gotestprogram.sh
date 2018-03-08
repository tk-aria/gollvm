#!/bin/sh
#
# Invoke the 'go test' to run test for a given Go program (such as
# the 'go' tool, or 'vet'). Setup for the test creates a small
# GOPATH-like environment in which to run.  Command line is expected
# to look like
#
#  gotestprogram.sh \
#     WORKDIR <value> \
#     SUBDIR <value> \
#     [COPYFILES file1:dest1 file2:dest2 ... fileN:destN] \
#     [COPYGODIRS dir1:dest1 dir2:dest2 ... dirN:destN] \
#     [COPYDIRS dir1:dest1 dir2:dest2 ... dirN:destN] \
#     [ENV var1=value1 var2=value2 .... varN=valueN] \
#     [TESTARG] <values> \
#     TIMEOUT <value> \
#     GOC <arg> ... \
#     CC <arg> ... \
#     LOGFILE <file> \
#     LIBDIR <dir> \
#     BINDIR <dir>
#
# where:
#
#   WORKDIR    names the work directory in which the test should be run
#   COPYFILES  is a list of F:D tags where F is a file path and D is
#              a subdir of the work dir into which F should be copied
#   COPYGODIRS is a list of G:D tags where G is a directory containing *.go
#              source files and D is a subdir of the work dir into which
#              the files should be copied
#   COPYDIRS   is a list of E:D tags where E is a directory and D is a
#              D is a subdir of the work dir into which D should be copied
#   ENV        is a list of var=value entries corresponding to environment
#              variable settings to make before running the test
#   TIMEOUT    is a numeric value (seconds) to use as the test timeout
#   TESTARG    arguments to append to 'go test' command line
#   GOC        is the path to the Go compiler driver or wrapper script
#   CC         is the path to the C compiler driver or wrapper script
#   LOGFILE    is a file into which test stdout/stderr should be written
#   LIBDIR     is the root of the libgo installation to test
#   BINDIR     is the directory containing tools to test (e.g. 'go')
#   SUBDIR     subdir within workdir in which test is run
#

CUR=""
for ARG in $*
do
  case "$ARG" in
    BINDIR) CUR=BINDIR ;;
    COPYDIRS) CUR=COPYDIRS ;;
    COPYFILES) CUR=COPYFILES ;;
    COPYGODIRS) CUR=COPYGODIRS ;;
    ENV) CUR=ENV ;;
    GOC) CUR=GOC ;;
    CC) CUR=CC ;;
    LIBDIR) CUR=LIBDIR ;;
    DEBUGCMD) CUR=DEBUGCMD ;;
    LOGFILE) CUR=LOGFILE ;;
    SUBDIR) CUR=SUBDIR ;;
    TESTARG) CUR=TESTARG ;;
    TIMEOUT) CUR=TIMEOUT ;;
    WORKDIR) CUR=WORKDIR ;;
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
REQUIRED="LOGFILE GOC LIBDIR BINDIR TIMEOUT WORKDIR SUBDIR"
for R in $REQUIRED
do
  eval "V=\$$R"
  if [ -z "$V" ]; then
    echo "error: no setting for \"$R\" supplied on command line"
    exit 1
  fi
done
export LD_LIBRARY_PATH="${LIBDIR}:${LD_LIBRARY_PATH}"
export PATH="${BINDIR}:${PATH}"
#
#------------------------------------------------------------------------
#
# Create directory in which we'll do the build
#
cd $BINDIR
if [ $? != 0 ]; then
  echo "can't change to $BINDIR"
  exit 1
fi
rm -rf $WORKDIR
mkdir $WORKDIR
#
# Copy in files cited in 'COPYGODIRS'. Each item here is a tuple X:Y
# where X is a source directory containing Go files and Y is a destination
# directory within the WORKDIR we're creating.
#
for CGD in $COPYGODIRS
do
  SRC=`echo $CGD | cut -f1 -d:`
  DST=`echo $CGD | cut -f2 -d:`
  mkdir -p ${WORKDIR}/$DST
  cp ${SRC}/*.go ${WORKDIR}/${DST}
  if [ $? != 0 ]; then
    echo "processing COPYGODIRS: command failed: cp ${SRC}/*.go ${WORKDIR}/${DST}"
    exit 1
  fi
done
#
# Copy in files cited in 'COPYFILES'. Each item here is a tuple X:Y
# where X is a specific file (typically a Go source file) and Y is a
# destination directory within the WORKDIR we're creating.
#
for CGF in $COPYFILES
do
  SRC=`echo $CGF | cut -f1 -d:`
  DST=`echo $CGF | cut -f2 -d:`
  mkdir -p ${WORKDIR}/$DST
  cp ${SRC} ${WORKDIR}/${DST}
  if [ $? != 0 ]; then
    echo "processing COPYFILES: command failed: cp ${SRC} ${WORKDIR}/${DST}"
    exit 1
  fi
done
#
# Copy in dirs cited in 'COPYDIRS'. Each item here is a tuple X:Y
# where X is a directory and Y is a destination directory within the
# WORKDIR we're creating.
#
for CD in $COPYDIRS
do
  SRC=`echo $CD | cut -f1 -d:`
  DST=`echo $CD | cut -f2 -d:`
  mkdir -p ${WORKDIR}/$DST
  cp -r ${SRC} ${WORKDIR}/${DST}
  if [ $? != 0 ]; then
    echo "processing COPYDIRS: command failed: cp -r ${SRC} ${WORKDIR}/${DST}"
    exit 1
  fi
done
#
# More setup
#
export GCCGOTOOLDIR=${BINDIR}
export GO_TESTING_GOTOOLS=yes
export GCCGO=${GOC}
if [ ! -z "${CC}" ]; then
  export CC
fi
export GOROOT=${LIBDIR}
HERE=`pwd`
cd $WORKDIR
#
# Change to the proper subdir and run the test.
#
cd ${SUBDIR}
if [ $? != 0 ]; then
  echo "can't change to ${SUBDIR}"
  exit 1
fi
CMD="go test -compiler gccgo ${FLAGS} -test.short -test.timeout=${TIMEOUT}s -test.v ${TESTARG}"
#
# Capture command for posterity
#
echo $CMD > ${LOGFILE}
#
# Execute the test, capturing output to log file. Emit log file highlights
# to stdout, then exit with appropriate status.
#
${CMD} 1>> ${LOGFILE} 2>&1
RC=$?
#
# For debugging failing tests, it can be handy to set "DEBUGCMD" to an
# editor or shell here, so as to poke around in the set up environment
#
if [ ! -z ${DEBUGCMD} ]; then
  ${DEBUGCMD} ${LOGFILE}
fi
#
# Emit log file highlights to stdout, then exit with appropriate status.
#
grep '^--- ' < $LOGFILE | sed -e 's/^--- \(.*\) ([^)]*)$/\1/' -e 's/SKIP/UNTESTED/' | sort -k 2
exit $RC
