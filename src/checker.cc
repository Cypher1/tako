#include <iostream>
#include <variant>
#include <optional>

#include "util.h"
#include "context.h"
#include "toString.h"

#include "checker.h"

std::optional<Contradiction> Assignment::setValue(
    const Variable &name,
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
    // If equal, done
    return {};
  }
  // else attempt to unify

  // TODO ...
  const Contradiction ret = {name, value, new_value};
  return ret;
}

Solutions Assignment::unify(const Value &a, const Value &b) {
  // TODO: unification should build a list of solutions and failures.
  return {
    {},
    {}
  };
}

std::variant<Value, Variable> Assignment::getValue(const Variable &name) const {
  auto it = assignment.find(name);
  if (it == assignment.end()) {
    return name; // It's unrestricted.
  }
  // It is set. TODO: ensure all its contents unified.
  return it->second;
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
