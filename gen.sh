#!/bin/bash

for i in tests/*;
do
    name=$(echo "$i" | sed "s/tests\///")
    result=$(head -n 1 "$i")
    args=$(grep -v "$result" "$i" | tr '\n' '|' | sed "s/|$/\"/" | sed "s/^/\"/" | sed "s/|/\", \"/g")
    echo "\n#[test]\nfn $name() {\n  test_with_expectation($result, vec![$args]);\n}\n" >> "./tests/goldens.rs"
done