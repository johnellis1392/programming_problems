#!/usr/bin/env bash

set -e
javac --enable-preview --release 19 Main.java
java --enable-preview Main