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
struct Variable {
  std::string name;
};

struct Contradiction {

};

class Assignment {
  private:
  std::map<std::string, std::variant<Value, Variable>> assignment;
  std::optional<Contradiction> setValueToInternal(std::string name, Value value);

  public:
  Assignment() {
  }

  void setValueTo(std::string name, Value value);
  std::variant<Value, Variable> getValueTo(std::string name);
};

struct CheckedModule {

};

CheckedModule check(Module module, Context &ctx);
#endif // #ifndef CHECKER_H
