#include <iostream>
#include <variant>
#include <optional>

#include "util.h"
#include "context.h"
#include "toString.h"

#include "checker.h"

std::optional<Contradiction> Assignment::setValue(const Variable &name,
                                                            const Value &new_value) {
  const auto value_it = assignment.find(name);
  if (value_it == assignment.end()) {
    assignment.emplace(name, new_value);
    return {};
  }

  const auto value_or_var = value_it->second;
  if(std::holds_alternative<Variable>(value_or_var)) {
    // assign that...
    return setValue(std::get<Variable>(value_or_var), new_value);
  }
  const auto value = std::get<Value>(value_or_var);
  if (value == new_value) {
    // If complex, unwrap and evaluate for equality
    // else
    return {};
  }

  // TODO ...
  const Contradiction ret = {name, value, new_value};
  return ret;
}

Solutions Assignment::resolve(const Value &a, const Value &b) {
  return {
    {},
    {}
  };
}

std::variant<Value, Variable> Assignment::getValue(const Variable &name) const {
  return name; // TODO get values from the mapping.
}

CheckedModule check(Module module, Context &ctx) {
  ctx.startStep(PassStep::Check);

  std::vector<CheckedDefinition>defs = {};

  //for(const auto& _def : module.definitions) {
    // TODO
  //}
  // TODO...
  return CheckedModule(module.name, module.loc, defs);
}
