#!/bin/bash
set -e

gcc -fdiagnostics-color=always -std=c11 -o smoketest \
  kiss-alloc.c kiss-alloc_malloc.c smoketest.c

valgrind ./smoketest
