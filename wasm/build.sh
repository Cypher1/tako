#!/bin/bash

function realt() {
  >&2 echo "timing '$@'"
  START=$(date +%s.%N)
  >&2 $@
  END=$(date +%s.%N)
  >&2 echo -e "\trealtime: $(bc <<< "$END - $START")"
}

function require() {
  PKG="$1"
  FILE="$2"
  if [ ! -f "$FILE" ]; then
      echo "Dependency $FILE is missing, fetching $PKG"
      return 1
  fi
  return 0
}

function initWabt() {
  require "wabt" "wabt/build/wat2wasm" && \
    require "wabt" "wabt/build/wasm2c" && \
    require "wabt" "wabt/wasm2c/wasm-rt.h" && \
    require "wabt" "wabt/wasm2c/wasm-rt-impl.c" && \
    return 1

  git submodule update --init --recursive
  mkdir -p wabt/build
  (
    # Use a subshell to maintain the current path.
    cd wabt/build
    cmake ..
    cmake --build .
  )
}

function wat2c() {
  # Use a subshell to maintain the current path.
  (
    FILE="$1"
    cd gen

    # Convert readable wat to wasm byte code.
    ../wabt/build/wat2wasm "${FILE}.wat" -o "${FILE}.wasm"

    # Generate C impl and header from wasm.
    ../wabt/build/wasm2c "${FILE}.wasm" -o "${FILE}.c"
  )
}

IKA_PATH="../target/debug/ika"

function initIka() {
  cargo build
}

function ika() {
  $IKA_PATH "$1" > gen/addTwo.wat
}

# Make sure our output dirs are available.
mkdir -p build
mkdir -p gen

# Fetch and build Wabt if it's not already.
initWabt

# Build Ika if it's not already.
realt initIka

# Copy the runtime header file so we don't have to rewrite C include lines.
require "header from wabt" "gen/wasm-rt.h" || \
  cp wabt/wasm2c/wasm-rt.h gen/wasm-rt.h

# Run ika's core.
realt ika "$1"
# Generate our C files ('compile to C')
realt wat2c "addTwo"

# Build with the bootstrap main.c
realt cc -o build/addTwo main.c gen/addTwo.c wabt/wasm2c/wasm-rt-impl.c
