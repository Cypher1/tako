#define DOCTEST_CONFIG_IMPLEMENT_WITH_MAIN
#include "doctest.h"

#include <algorithm>
#include <functional>

#include <iostream>
#include <optional>
#include <vector>

#include "../src/ast.h"

#include "../src/checker.h"
#include "../src/show.h"
#include "../src/util.h"

doctest::String toString(const Value& value) {
  return show(value, 0).c_str();
}

template<typename T, typename ...U>
void eqvar(std::variant<U...> variant, T value) {
  CHECK_MESSAGE(std::holds_alternative<T>(variant), "type mismatch");
  const T v = std::get<T>(variant);
  CHECK_MESSAGE(v == value, "not equal");
}

void showValue(const std::optional<Contradiction> &contradiction,
    std::ostream &os) {
  if (!contradiction) {
    os << "Satifiable.";
    return;
  }
  os << "Contradiction: " << contradiction->name;
  os << " required to be both " << show(contradiction->a);
  os << " and " << show(contradiction->b) << std::endl;
}

TEST_CASE("can assign anything to x in an empty assignment") {
  Assignment emp;

  // TODO: Arbitrary Values
  const Value val = {"v", errorLocation, {}};

  const auto contradiction = emp.setValue("x", val);
  CHECK_MESSAGE(!contradiction, "expected no contradiction");
  SUBCASE("check value was stored") {
    eqvar(emp.getValue("x"), val);
  }
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

TEST_CASE("Simple assignment checks for contradictions when assigning another variable") {
  Assignment emp;

  // TODO: Arbitrary Values
  const Value val1 = {"v1", errorLocation, {}};
  const Value val2 = {"v2", errorLocation, {}};
  const Value val3 = {"v3", errorLocation, {}};

  emp.setValue("x", val1);

  SUBCASE("assigning equal value to unrelated variable") {
    emp.setValue("y", val1);
    SUBCASE("can assign equal value without contradiction") {
      const auto contradiction = emp.setValue("x", val1);
      CHECK_MESSAGE(!contradiction, "no contradiction");
      SUBCASE("check values were stored") {
        eqvar(emp.getValue("x"), val1);
        eqvar(emp.getValue("y"), val1);
      }
    }
    SUBCASE("cannot assign non-equal value without contradiction") {
      const auto contradiction = emp.setValue("x", val2);
      CHECK_MESSAGE(contradiction, "assigning x to both v1 and v2 should fail");
    }
  }
  SUBCASE("assigning different value to unrelated variable") {
    emp.setValue("y", val3);
    SUBCASE("can assign equal value without contradiction") {
      const auto contradiction = emp.setValue("x", val1);
      CHECK_MESSAGE(!contradiction, "no contradiction");
      SUBCASE("check values were stored") {
        eqvar(emp.getValue("x"), val1);
        eqvar(emp.getValue("y"), val3);
      }
    }
    SUBCASE("cannot assign non-equal value without contradiction") {
      const auto contradiction = emp.setValue("x", val2);
      CHECK_MESSAGE(contradiction, "assigning x to both v1 and v2 should fail");
    }
  }
}
