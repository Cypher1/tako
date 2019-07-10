#include <algorithm>
#include <functional>
#include <iostream>
#include <optional>
#include <vector>

#include "checker_tests.h"

#include "../src/ast.h"

#include "../src/checker.h"
#include "../src/toString.h"
#include "../src/util.h"

#define ASSERT(x, n)                                                           \
  if (!x) {                                                                    \
    failures[current_test].push_back(std::string(n));   \
  }

#define ASSERT_FALSE(x, n) ASSERT(!x, n)

void showValue(const std::optional<Contradiction> &contradiction,
               std::ostream &os) {
  if (!contradiction) {
    os << "Satifiable.";
    return;
  }
  os << "Contradiction: " << contradiction->name;
  os << " required to be both " << toString(contradiction->a);
  os << " and " << toString(contradiction->b) << std::endl;
}

std::string current_test = "pre-test";

std::map<std::string, std::vector<std::string>> failures;

void test(std::string name, std::function<void(void)> test) {
  current_test = name;
  std::cout << "> " << name << "\n";
  test();
  current_test = std::string("finish ") + name;
}

int checker_tests(const Config &config) {
  std::cout << banner("Variable assignment tests", config) << "\n";
  test("can assign anything to x in an empty assignment", [] {
    Assignment emp;

    // TODO: Arbitrary Values
    const Value val = {"v", errorLocation, {}};

    const auto contradiction = emp.setValue("x", val);
    ASSERT_FALSE(contradiction, "no contradiction");
  });

  test("can assign anything to x in a simple non-contradicting assignment", [] {
    Assignment emp;

    // TODO: Arbitrary Values
    const Value val = {"v", errorLocation, {}};

    emp.setValue("x", val);
    const auto contradiction = emp.setValue("x", val);
    ASSERT_FALSE(contradiction, "no contradiction");
  });

  test("cannot assign anything to x in a simple contradicting assignment", [] {
    Assignment emp;

    // TODO: Arbitrary Values
    const Value val1 = {"v1", errorLocation, {}};
    const Value val2 = {"v2", errorLocation, {}};

    emp.setValue("x", val1);
    const auto contradiction = emp.setValue("x", val2);
    ASSERT(contradiction, "assigning x to both v1 and v2 should fail");
  });

  std::cout << banner("Done", config) << "\n";

  int errors = 0;
  for (const auto &test : failures) {
    std::cout << "Test: " << test.first << "\n";
    for (const auto fail : test.second) {
      std::cout << "  Failed: " << fail << "\n";
      errors++;
    }
  }
  return errors;
}
