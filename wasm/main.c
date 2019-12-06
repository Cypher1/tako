#include <stdio.h>
#include <stdlib.h>

#include "wabt/wasm2c/wasm-rt.h"
#include "gen/addTwo.h"

int main(int argc, char* argv[]) {

  if(argc < 3) {
    return 1;
  }

  int x = atoi(argv[1]);
  int y = atoi(argv[2]);

  init();
  int z = Z_mainZ_iii(x, y);
  printf("%d + 3*%d = %d\n", x, y, z);
  return 0;
}
