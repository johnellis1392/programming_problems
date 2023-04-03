#!/usr/bin/env bash
set -u -e
if [ $# -ne 1 ]; then
  echo "Usage: $0 <path to config file>"
  exit 0;
elif [ ! -e "$1" ]; then
  echo "ERROR: File '$1' does not exist"
  exit 1;
fi

filename="$1"
last_modified="$(date -r $filename)"
clear && ./run.sh

while [ 1 ]; do
  modified="$(date -r $filename)"
  if [[ "$last_modified" != "$modified" ]]; then
    last_modified=$modified
    clear
    ./run.sh
  else
    sleep 1
  fi
done