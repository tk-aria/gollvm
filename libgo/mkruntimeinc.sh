#!/bin/sh
RAWFILE=$1
shift
TMPFILE1=$1
shift
OUTFILE=$1
shift
if [ -z "$OUTFILE" ]; then
  echo "error: no outfile specified"
  exit 1
fi
if [ -z "$RAWFILE" ]; then
  echo "error: no raw input file specified"
  exit 1
fi
if [ -z "$TMPFILE1" ]; then
  echo "error: no tmp 1 input file specified"
  exit 1
fi
TMPFILE2=${TMPFILE1}.2
#
# _Complex_lock and _Reader_lock are Go translations of some AIX
# system types and should not be exported back to C semt is a Go
# translation of the C type sem_t; it fails to convert on some systems
# and need not be exported back to C. sigset conflicts with system
# type sigset on AIX, so we need to rename it
#
grep -v "#define _" ${RAWFILE} | grep -v "#define [cm][01234] " | grep -v "#define empty " | grep -v "#define \\$" > ${TMPFILE1}
for pattern in '_[GP][a-z]' _Max _Lock _Sig _Trace _MHeap _Num
do
  grep "#define $pattern" ${RAWFILE} >> ${TMPFILE1};
done
for TYPE in _Complex_lock _Reader_lock semt
do
  sed -e '/struct '${TYPE}' {/,/^}/s/^.*$//' ${TMPFILE1} > ${TMPFILE2}
  mv ${TMPFILE2} ${TMPFILE1}
  if [ $? != 0 ]; then
    echo "error: cmd failed: mv ${TMPFILE2} ${TMPFILE1}"
    exit 1
  fi
done
exec sed -e 's/sigset/sigset_go/' ${TMPFILE1} > ${OUTFILE}
