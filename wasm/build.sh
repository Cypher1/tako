./wabt/build/wasm2c addTwo.wasm -o addTwo.c

cc -o addTwo main.c addTwo.c wabt/wasm2c/wasm-rt-impl.c
