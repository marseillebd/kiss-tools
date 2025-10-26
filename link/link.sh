#!/usr/bin/env sh
set -e

runghc link.hs test.hex
runghc link.hs test.hex -b -o test.bin
xxd test.bin
