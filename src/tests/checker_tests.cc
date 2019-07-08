#include <iostream>
#include <vector>
#include <algorithm>

#include <rapidcheck.h>

#include "ast.h"
#include "checker_tests.h"

#include "../checker/checker.h"

void checker_tests() {
  std::cout << "Checker tests\n";

  rc::check("double reversal yields the original value",
            [](const std::vector<int> &l0) {
              auto l1 = l0;
              std::reverse(begin(l1), end(l1));
              std::reverse(begin(l1), end(l1));
              RC_ASSERT(l0 == l1);
            });

  rc::check("can assign anything to x in an empty assignment",
            [](const Variable &var) {
              Assignment emp;

              // TODO: Arbitrary Values
              const Value val = {"x", errorLocation, {}};

              const auto contradiction = emp.setValueTo(var, val);
              RC_ASSERT(!contradiction);
            });

  rc::check("cannot assign anything to x in a contradicting assignment",
            [](const Variable &var) {
              Assignment emp;

              // TODO: Arbitrary Values
              const Value val1 = {"x", errorLocation, {}};
              const Value val2 = {"y", errorLocation, {}};

              emp.setValueTo(var, val1);
              const auto contradiction = emp.setValueTo(var, val2);
              RC_ASSERT(contradiction);
            });
}
