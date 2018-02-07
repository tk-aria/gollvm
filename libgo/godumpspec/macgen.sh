#!/bin/sh
COMPILER=$1
shift
INPUT=$1
shift
OUTPUT=$1
shift
OFILE=$1
shift
IFILE=$1
shift
${COMPILER} $* -c ${INPUT} -o ${OFILE}
if [ $? != 0 ]; then
  echo "** regular comp failed"
  exit 1
fi
${COMPILER} $* -c -E ${INPUT} > ${IFILE}
if [ $? != 0 ]; then
  echo "** -E comp failed"
  exit 1
fi
exec ${COMPILER} $* -E -dM ${INPUT} > ${OUTPUT}
