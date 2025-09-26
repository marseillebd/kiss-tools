#!/usr/bin/env bash
set -euo pipefail

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
Usage: $0 [FILE]
  Converts input into a markdown file: comments are used as markdown, and code is encased in triple-tick blocks.
  If no file given, read from stdin.
  If the input file has an extension, it is used to tag code blocks.
END
    exit 1
    ;;
esac

mode=init
blank=0
while IFS='' read line; do
  cleanLine="$(sed 's;^ *;;' <<<"$line")"
  case "$cleanLine" in
    "// "*)
      case $mode in
        text)
          if [[ $blank != 0 ]]; then
            echo '$'
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
      echo "$(sed 's;\s*// ;;' <<<"$line")"
      ;;
    "")
      blank=1
      ;;
    *)
      case $mode in
        code)
          if [[ $blank != 0 ]]; then
            echo '$'
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
