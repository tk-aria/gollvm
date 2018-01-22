#!/bin/sh
TORUN=$1
if [ -z "$TORUN" ]; then
  echo "error: no script specified"
  exit 1
fi
shift
OUTFILE=$1
if [ -z "$OUTFILE" ]; then
  echo "error: no outfile specified"
  exit 1
fi
shift
ARGS=$*
if [ -z "$SHELL" ]; then
  echo "error: no SHELL setting"
  exit 1
fi
exec ${SHELL} -e ${TORUN} ${ARGS} > ${OUTFILE}
