#!/bin/sh
set -eu

HERE="$(dirname "$0")"
cd "$HERE"

CC='clang-22'
CFLAGS='-Wall -fdefer-ts -g -O1 -I.'

# -Df_TRACE adds tracing
# CFLAGS="$CFLAGS -Df_TRACE"

$CC $CFLAGS -c forth_rt.c forth.c

ec=0
thisEc=0
for f in testcases/test_*.c; do
  base="${f%.c}"
  $CC $CFLAGS -c "$base.c" -o "$base.o"
  $CC forth.o "$base.o" forth_rt.o -o "$base.exe"
  {
    set +e
    "./$base.exe"
    thisEc="$?"
    set -e
    if [ "$thisEc" = 0 ]; then
      echo >&2 "ok   $(basename "$base")"
    else
      ec=1
      echo >&2 "FAIL ($thisEc): $(basename "$base")"
    fi
  }
done
exit $ec
