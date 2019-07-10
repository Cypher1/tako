#include "src/util.h"

#include "test/checker_tests.h"

int main() {
  Config config;
  int errors = 0;
  errors += checker_tests(config);

  return errors;
}
