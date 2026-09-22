#!/usr/bin/env bash
set +euo pipefail

kcget() {
  # parse arguments
  local fname section key
  case "$#" in
  2)
    fname="$1"
    key="$2"
  ;;
  3)
    fname="$1"
    section="$2"
    key="$3"
  ;;
  *)
  cat >&2 << 'END'
usage: kcget <config file> [ <section> ] <key>
END
  return 1
  ;;
  esac

  # read through file to find the correct key-value settings
  # prints out all pairs; used `head` or `tail` to get exactly one,
  # or handle the ambiguity however you want
  local line cursection
  grep -P '^'"$key"'\s+|^\[[a-zA-Z0-9.]+\]\s*$' <"$fname" | \
  while IFS='' read -r line; do
    echo >&2 "LINE: $line"
    case "$line" in
    '['*) # a section header
      cursection="${line#\[}"
      cursection="${cursection%\]*}"
      echo >&2 "SECTION: $cursection"
    ;;
    *)
      echo >&2 "'$section' =? '$cursection'"
      if [[ "$section" = "$cursection" ]]; then
        echo >&2 "FOUND: $line"
        echo "$line" | sed -E 's/^\S+\s+//'
      fi
    ;;
    esac
  done
}

kcf_main() {
  :
}

if [[ "$0" = "${BASH_SOURCE[0]}" ]]; then
  kcget "$@"
fi
