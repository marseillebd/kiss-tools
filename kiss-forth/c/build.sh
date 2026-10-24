#!/bin/sh
set -eu

# -Df_TRACE adds tracing
clang-22 -Wall -fdefer-ts -g -O1 -c forth_rt.c forth.c

ec=0
thisEc=0
for f in test_*.c; do
  base="${f%.c}"
  clang-22 -Wall -g -O1 -c "$base.c" -o "$base.o"
  clang-22 forth.o "$base.o" forth_rt.o -o "$base.exe"
  {
    set +e
    "./$base.exe"
    thisEc="$?"
    set -e
    if [ "$thisEc" = 0 ]; then
      echo >&2 "$base ok"
    else
      ec=1
      echo >&2 "FAIL ($thisEc): $base"
    fi
  }
done
exit $ec
