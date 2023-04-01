#!/usr/bin/env bash

# set -e
set -u
filename="bfs.py"

watch() {
  clear;
  last_modified="$(date -r $filename)"
  python $filename
  while [ 1 ]; do
    modified="$(date -r $filename)"
    if [ "$last_modified" != "$modified" ]; then
      last_modified="$modified"
      clear;
      python $filename
    else
      sleep 1
    fi
  done
}

watch