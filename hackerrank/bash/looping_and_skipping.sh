#!/usr/bin/env bash

for i in {1..99}
do
  if [[ $(bc <<<"$i % 2" ) -eq 1 ]]; then
    echo $i
  fi
done