#pragma once
#ifndef CHECKER_H
#define CHECKER_H

#include <string>
#include <map>
#include <variant>

#include "util.h"
#include "context.h"
#include "parser.h"

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

  public:
  Assignment() {
  }

  std::optional<Contradiction> setValueTo(const Variable &name, const Value &value);
  std::variant<Value, Variable> getValueTo(const Variable &name);
};

struct CheckedModule {

};

CheckedModule check(Module module, Context &ctx);
#endif // #ifndef CHECKER_H
