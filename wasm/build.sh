mkdir -p build
mkdir -p gen

cp ./wabt/wasm2c/wasm-rt.h ./gen/wasm-rt.h
./wabt/build/wasm2c addTwo.wasm -o addTwo.c
mv addTwo.c gen/
mv addTwo.h gen/

cc -o build/addTwo main.c gen/addTwo.c wabt/wasm2c/wasm-rt-impl.c
