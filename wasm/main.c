#include <stdio.h>
#include <stdlib.h>

#include "wabt/wasm2c/wasm-rt.h"
#include "addTwo.h"

int main(int argc, char* argv[]) {

  if(argc < 3) {
    return 1;
  }

  int x = atoi(argv[1]);
  int y = atoi(argv[2]);

  init();
  int z = Z_addTwoZ_iii(x, y);
  printf("%d + %d = %d\n", x, y, z);
  return 0;
}
