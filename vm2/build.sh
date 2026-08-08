#!/bin/sh
set -eu
set -x

tmpdir="$(mktemp -d .build.XXXXXX)"
mkdir -p "$tmpdir"
trap 'rm -rf "$tmpdir"' EXIT

LLASM="${LLASM:-llvm-as-10}"
LLC="${LLC:-llc-10}"
CC="${CC:-gcc}"

"$LLASM" vm.ll -o "$tmpdir/vm.bc"
"$LLC" "$tmpdir/vm.bc" -o "$tmpdir/vm.s"
"$CC" vm.c "$tmpdir/vm.s" -o vm
