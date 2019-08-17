#ifndef CHECKER_H
#define CHECKER_H

#include <string>
#include <map>
#include <variant>
#include <optional>
#include <iostream>

#include "ast.h"
#include "util.h"
#include "context.h"
#include "parser.h"
#include "show.h"

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

class Assignment;

struct Solutions {
  std::vector<Assignment> successes;
  std::vector<Contradiction> failures;
};

struct Proof {
  // Should have the form
  // Rule applied with argumnets to Proof / assumption

};

class Assignment {
  private:
  std::map<Variable, std::variant<Value, Variable>> assignment;

  public:
  Assignment() {
  }

  std::optional<Contradiction> setValue(const Variable &name, const Value &value);
  std::variant<Value, Variable> getValue(const Variable &name) const;

  Solutions unify(const Value &a, const Value &b);
};

using Check = Value; // TODO: Should the proofs have optional proofs?

class Checks {
  public:
  std::vector<Check> pre;
  std::vector<Check> post;

  Assignment requirements;
  // TODO: update a proof / list of steps taken.
  Proof proof;

  Checks() = default;
};

using CheckedValue = ValueCore<Checks>;
using CheckedDefinition = DefinitionCore<Checks>;
using CheckedModule = ModuleCore<Checks>;

class CheckerContext {
public:
  Context &context;
  // See docs/notes/checker.md
  std::map<Path, std::vector<std::pair<Path, Checks>>> call_graph;

  CheckerContext(Context &ctx)
      : context{ctx} {}

  void msg(Value &val, MessageType level, std::string msg_txt);

  void addEdge(const Path &caller, const Path &callee, const Checks &requirements) {
    const auto it = call_graph.find(caller);
    const std::vector<std::pair<Path, Checks>> pairs = {{callee, requirements}};
    if(it == call_graph.end()) {
      call_graph.emplace(caller, pairs);
    } else {
      auto& lst = it->second;
      lst.insert(lst.end(), pairs.begin(), pairs.end());
    }
  }

};

// Call graph / tree builders
ValueCore<Checks> check(std::vector<Symbol> path, const ValueCore<Empty> &def, CheckerContext &ctx);
DefinitionCore<Checks> check(std::vector<Symbol> path, const DefinitionCore<Empty> &def, CheckerContext &ctx);
ModuleCore<Checks> check(std::vector<Symbol> path, const ModuleCore<Empty> &def, CheckerContext &ctx);

// Wrapper that does some setup work
template<template<typename> typename T>
T<Checks> check(const T<Empty> &code, Context &context) {
  context.startStep(PassStep::Check);
  CheckerContext ctx(context);
  const auto checkedAst = check({}, code, ctx); // Keep for code gen.

  // Simplify the graph, collapse edges and propagate their requirements
  std::cerr << "Graph:\n";
  for (const auto &p : ctx.call_graph) {
    const auto val = p.first;
    std::cerr << show(val) << ":\n";
    for (const auto &e : p.second) {
      const auto call = e.first;
      const auto checks = e.second;
      std::cerr << " --> " << show(call) << "\n";
      std::cerr << "Pre:\n";
      for (const auto &pre : checks.pre) {
        std::cerr << show(pre, 1) << "\n";
      }
      std::cerr << "Post:\n";
      for (const auto &post : checks.post) {
        std::cerr << show(post, 1) << "\n";
      }
    }
  }


  // Do some checking


  // After this, checks can only be used for optimisation & code gen
  // All semantic & potential runtime errors should be compiler bugs.
  return checkedAst;
}

#endif // #ifndef CHECKER_H
