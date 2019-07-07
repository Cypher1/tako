#include <iostream>
#include <vector>
#include <algorithm>

#include <rapidcheck.h>

#include "checker_tests.h"

void checker_tests() {
  std::cout << "Checker tests\n";

  rc::check("double reversal yields the original value",
            [](const std::vector<int> &l0) {
              auto l1 = l0;
              std::reverse(begin(l1), end(l1));
              std::reverse(begin(l1), end(l1));
              RC_ASSERT(l0 == l1);
            });

}
