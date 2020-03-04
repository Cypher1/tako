#!/bin/bash

for file in ../compiled_examples/*.tk
do
  ./build.sh "$file" || exit 1
done
