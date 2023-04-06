#!/usr/bin/env bash
find . -name \*.class | xargs -I{} rm {}