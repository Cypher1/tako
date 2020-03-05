#!/bin/bash

for file in ../compiled_examples_wasm/*.tk
do
  ./build.sh "$file" || exit 1
done
