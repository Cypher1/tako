./wabt/build/wat2wasm myAddTwo.wat -o myAddTwo.wasm
./wabt/build/wasm2c myAddTwo.wasm -o myAddTwo.c

cc -o myAddTwo main.c myAddTwo.c wabt/wasm2c/wasm-rt-impl.c
