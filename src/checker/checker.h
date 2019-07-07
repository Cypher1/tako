#pragma once
#ifndef CHECKER_H
#define CHECKER_H

#include <string>
#include <map>
#include <variant>

#include "../util/util.h"
#include "../util/context.h"
#include "../parser/parser.h"

// Use only for values that the compiler creates
// (rather than values from the source program).
using Variable = std::string;

struct Contradiction {
  // Holds two conditions that are deemed to be
  // in contradiction...
  Variable name;
  Value a;
  Value b;
};

class Assignment {
  private:
  std::map<Variable, std::variant<Value, Variable>> assignment;
  std::optional<Contradiction> setValueToInternal(Variable name, Value value);

  public:
  Assignment() {
  }

  void setValueTo(Variable name, Value value);
  std::variant<Value, Variable> getValueTo(Variable name);
};

struct CheckedModule {

};

CheckedModule check(Module module, Context &ctx);
#endif // #ifndef CHECKER_H
