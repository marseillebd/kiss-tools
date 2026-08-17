#!/bin/sh
set -e

printf "#!/usr/bin/env  "
printf "kiss-vm        \n"
printf "kiss vm\0"
printf "\0\0\0\0\0\0\0\0"

sed -e 's/@.*$//' \
    -e 's/#.*$//' \
  | tr 'a-z' 'A-Z' \
  | tr -dC '0-9A-F' \
  | sed -E -e 's/.{4}/\0 /g' \
           -e 's/(.{5}){8}/\0\n/g' \
  | awk '{ printf "%.8x: %s\n", 16*(NR-1), $0 }' \
  | xxd -r
