#!/bin/sh
CC=$1
shift
OUTFILE=$1
shift
FLAGS=$*
set -e
if [ -z "$CC" ]; then
  echo "error: no CC version specified"
  exit 1
fi
if [ ! -x "$CC" ]; then
  echo "error: CC compiler $CC not executable"
  exit 1
fi
if [ -z "$OUTFILE" ]; then
  echo "error: no outfile specified"
  exit 1
fi
echo '#include <errno.h>' | ${CC} ${FLAGS} -x c - -E -dM > ${OUTFILE}
exit $?
