#!/usr/bin/env bash
filename="main.py"
last_modified="$(date -r $filename)"
clear && python $filename
while [ 1 ]; do
  modified="$(date -r $filename)"
  if [[ $last_modified !=  $modified ]]; then
    last_modified=$modified
    clear && python $filename
  else
    sleep 1
  fi
done