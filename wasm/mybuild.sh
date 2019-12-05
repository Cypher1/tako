mkdir -p build
mkdir -p gen

cargo run "$1" > gen/myAddTwo.wat
cp ./wabt/wasm2c/wasm-rt.h ./gen/wasm-rt.h
./wabt/build/wat2wasm gen/myAddTwo.wat -o gen/myAddTwo.wasm
./wabt/build/wasm2c gen/myAddTwo.wasm -o myAddTwo.c
mv myAddTwo.c gen/
mv myAddTwo.h gen/

cc -o build/myAddTwo myMain.c gen/myAddTwo.c wabt/wasm2c/wasm-rt-impl.c
