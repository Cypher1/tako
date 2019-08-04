#include <iostream>
#include <variant>
#include <optional>

#include "util.h"
#include "context.h"

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

CheckedDefinition checkDefinition(Definition module, CheckerContext &ctx);

std::variant<Value, Variable> Assignment::getValue(const Variable &name) const {
  auto it = assignment.find(name);
  if (it == assignment.end()) {
    return name; // It's unrestricted.
  }
  // It is set. TODO: ensure all its contents unified.
  return it->second;
}

// Call graph / tree builders
CheckedValue checkValue(Value val, CheckerContext &ctx) {
  std::vector<CheckedDefinition> args = {};
  for(const auto& arg : val.args) {
    args.push_back(checkDefinition(arg, ctx));
  }
  return CheckedValue(val.name, val.loc, args, val.node_type);
}

CheckedDefinition checkDefinition(Definition def, CheckerContext &ctx) {
  std::vector<CheckedDefinition> args = {};
  for(const auto& arg : def.args) {
    args.push_back(checkDefinition(arg, ctx));
  }
  std::optional<CheckedValue> val;
  if(def.value) {
  val = checkValue(*def.value, ctx);
  }
  return CheckedDefinition(def.name, def.loc, args, val);
}

CheckedModule checkModule(Module module, CheckerContext &ctx) {
  std::vector<CheckedDefinition>defs = {};
  for(const auto& def : module.definitions) {
    // TODO: Branching?
    defs.push_back(checkDefinition(def, ctx));
  }
  return CheckedModule(module.name, module.loc, defs);
}

// Wrappers that do some setup work
ValueCore<Checks> check(const ValueCore<Empty> &code, CheckerContext &ctx) {
  ctx.startStep(PassStep::Check);
  return checkValue(code, ctx);
}

DefinitionCore<Checks> check(const DefinitionCore<Empty> &code, CheckerContext &ctx) {
  ctx.startStep(PassStep::Check);
  return checkDefinition(code, ctx);
}

ModuleCore<Checks> check(const ModuleCore<Empty> &code, CheckerContext &ctx) {
  ctx.startStep(PassStep::Check);
  return checkModule(code, ctx);
}
