#include <iostream>
#include <variant>
#include <optional>

#include "util.h"
#include "context.h"

#include "checker.h"

void CheckerContext::msg(Value &val, MessageType level, std::string msg_txt) {
  // TODO: Add contextual information.
  context.msg(val.loc, level, msg_txt);
}

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

DefinitionCore<Checks> check(std::vector<Symbol> def_path, const DefinitionCore<Empty> &def, CheckerContext &ctx) {
  def_path.push_back(def.name);
  std::vector<CheckedDefinition> args = {};
  for(const auto& arg : def.args) {
    args.push_back(check(def_path, arg, ctx));
  }
  std::optional<CheckedValue> val;
  if(def.value) {
    val = check(def_path, *def.value, ctx);
    // Add edges?
    Checks check;
    if(def.value) {
      auto v = *def.value;
      if(v.node_type == AstNodeType::Symbol) {
        // TODO: ctx.addSymbol(def_path, v);
        // Get the 'symbol' that the value corresponds to.
        // auto val_path = ctx.lookup(def_path, v);
        // ctx.addEdge(def_path, val_path, check);
      }
    } // TODO: If there is no value....!?
}
  return CheckedDefinition(def.name, def.loc, args, val);
}

ValueCore<Checks> check(std::vector<Symbol> path, const ValueCore<Empty> &val, CheckerContext &ctx) {
  path.push_back(val.name);
  std::vector<CheckedDefinition> args = {};
  for(const auto& arg : val.args) {
    args.push_back(check(path, arg, ctx));
  }
  return CheckedValue(val.name, val.loc, args, val.node_type);
}

ModuleCore<Checks>check(std::vector<Symbol> path, const ModuleCore<Empty> &module, CheckerContext &ctx) {
  std::vector<CheckedDefinition>defs = {};
  path.push_back(module.name);
  for(const auto& def : module.definitions) {
    // TODO: Branching?
    defs.push_back(check(path, def, ctx));
  }
  return CheckedModule(module.name, module.loc, defs);
}
