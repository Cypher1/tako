mkdir -p build
mkdir -p gen

cp ./wabt/wasm2c/wasm-rt.h ./gen/wasm-rt.h
./wabt/build/wat2wasm myAddTwo.wat -o gen/myAddTwo.wasm
./wabt/build/wasm2c gen/myAddTwo.wasm -o myAddTwo.c
mv myAddTwo.c gen/
mv myAddTwo.h gen/

cc -o build/myAddTwo myMain.c gen/myAddTwo.c wabt/wasm2c/wasm-rt-impl.c
