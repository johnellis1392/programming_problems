#!/usr/bin/env bash
set -u
watch() {
  if [[ $# -lt 1 ]]; then
    echo "Invalid # of arguments supplied"
    return 1
  fi
  filename="$1"
  shift
  if [[ ! -e "$filename" ]]; then
    echo "File '$filename' does not exist."
    return 1
  fi

  clear
  last_modified="$(date -r $filename)"
  python $filename $*
  while [ 1 ]; do
    m="$(date -r $filename)"
    if [[ $last_modified != $m ]]; then
      last_modified=$m
      clear
      python $filename $*
    else
      sleep 1
    fi
  done
}
watch $*