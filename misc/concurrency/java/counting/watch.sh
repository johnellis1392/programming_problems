#!/usr/bin/env bash
set -u

clear
javac Main.java && java Main

filename="Main.java"
last_modified="$(date -r $filename)"
while [ 1 ]; do
  modified="$(date -r $filename)"
  if [[ $last_modified != $modified ]]; then
    last_modified=$modified
    clear
    javac Main.java && java Main
  else
    sleep 1
  fi
done