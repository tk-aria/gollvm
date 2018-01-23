#!/bin/sh
set -e
AWK=$1
AWKFILE=$2
FILESLIST=$3
OUTFILE=$4
if [ -z "$AWK" ]; then
  echo "error: no awk version specified"
  exit 1
fi
if [ ! -x "$AWK" ]; then
  echo "error: awk $AWK not executable"
  exit 1
fi
if [ -z "$AWKFILE" ]; then
  echo "error: no awkfile specified"
  exit 1
fi
if [ -z "$FILESLIST" ]; then
  echo "error: no filelist specified"
  exit 1
fi
if [ ! -r "$FILESLIST" ]; then
  echo "error: fileslist file $FILESLIST not readable"
  exit 1
fi
if [ -z "$OUTFILE" ]; then
  echo "error: no outfile specified"
  exit 1
fi
INFILES=`cat $FILESLIST`
exec ${AWK} -f ${AWKFILE} ${INFILES} > ${OUTFILE}
