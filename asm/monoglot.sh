#!/usr/bin/env bash
set -euo pipefail

comment='//'
while [[ $# -gt 0 ]]; do
  case "$1" in
    -c)
      case "$2" in
        c|C) comment='//' ;;
        hash) comment='#' ;;
        '')
          echo >&2 "missing argument to -c (must be one of: c, hash)"
          exit 1
          ;;
        *)
          echo >&2 "unrecognized argument to -c '$2' (must be one of: c, hash)"
          exit 1
          ;;
      esac
      shift; shift;;
    *) break ;;
  esac
done
case $# in
  0)
    echo >&2 "Waiting for input..."
    ext=''
    ;;
  1)
    exec 0<"$1"
    ext="${1##*.}"
    ;;
  *)
    cat >&2 << END
Usage: $0 [-c <c|hash>] [FILE]
  Converts input into a markdown file: comments are used as markdown, and code is encased in triple-tick blocks.
  If no file given, read from stdin.
  If the input file has an extension, it is used to tag code blocks.

  -c <comment type> Sets the comment marker. Default: C
    c, C  two slashes (like C++ and recent C single-line comments)
    hash  a single hash (like shell, python, and the like)
END
    exit 1
    ;;
esac

mode=init
blank=0
while IFS='' read line; do
  cleanLine="$(sed 's&^ *&&' <<<"$line")"
  case "$cleanLine" in
    "${comment} "*)
      case $mode in
        text)
          if [[ $blank != 0 ]]; then
            echo ''
            blank=0
          fi
          ;;
        code)
          mode=text
          blank=0
          echo '```'
          echo ''
          ;;
        init) mode=text ;;
        *) exit 1 ;;
      esac
      echo "$(sed 's&\s*'"${comment}"' &&' <<<"$line")"
      ;;
    ""|"${comment}")
      blank=1
      ;;
    *)
      case $mode in
        code)
          if [[ $blank != 0 ]]; then
            echo ''
            blank=0
          fi
          ;;
        text)
          mode=code
          blank=0
          echo ''
          echo '```'"$ext"
          ;;
        init)
          mode=code
          echo '```'"$ext"
          ;;
        *) exit 1 ;;
      esac
      echo "$line"
      ;;
  esac
done

case $mode in
  code)
    echo '```'
    ;;
esac
