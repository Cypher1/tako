#define DOCTEST_CONFIG_IMPLEMENT_WITH_MAIN
#include "doctest.h"

#include <algorithm>
#include <functional>

#include <iostream>
#include <optional>
#include <vector>

#include "../src/ast.h"

#include "../src/checker.h"
#include "../src/toString.h"
#include "../src/util.h"

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

TEST_CASE("can assign anything to x in an empty assignment") {
  Assignment emp;

  // TODO: Arbitrary Values
  const Value val = {"v", errorLocation, {}};

  const auto contradiction = emp.setValue("x", val);
  CHECK_MESSAGE(!contradiction, "no contradiction");
}

TEST_CASE("Simple assignment checks for contradictions") {
  Assignment emp;

  // TODO: Arbitrary Values
  const Value val1 = {"v1", errorLocation, {}};
  const Value val2 = {"v2", errorLocation, {}};

  emp.setValue("x", val1);

  SUBCASE("can assign equal value without contradiction") {
    const auto contradiction = emp.setValue("x", val1);
    CHECK_MESSAGE(!contradiction, "no contradiction");
  }
  SUBCASE("cannot assign non-equal value without contradiction") {
    const auto contradiction = emp.setValue("x", val2);
    CHECK_MESSAGE(contradiction, "assigning x to both v1 and v2 should fail");
  }
}
