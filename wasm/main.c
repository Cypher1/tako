#include <stdio.h>
#include <stdlib.h>

#include "wabt/wasm2c/wasm-rt.h"
#include "gen/addTwo.h"

int main(int argc, char* argv[]) {
  init();
  int z = Z_run_mainZ_iv();
  printf("%d\n", z);
  return 0;
}
