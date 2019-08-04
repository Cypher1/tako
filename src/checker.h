#ifndef CHECKER_H
#define CHECKER_H

#include <string>
#include <map>
#include <variant>
#include <optional>

#include "ast.h"
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

struct Checks {
  std::vector<Check> pre;
  std::vector<Check> post;

  Assignment requirements;
  // TODO: update a proof / list of steps taken.
  Proof proof;
};

using CheckedValue = ValueCore<Checks>;
using CheckedDefinition = DefinitionCore<Checks>;
using CheckedModule = ModuleCore<Checks>;

using CheckerContext = Context; // TODO: Needs to track proofs...

ValueCore<Checks> check(const ValueCore<Empty> &code, CheckerContext &ctx);
DefinitionCore<Checks> check(const DefinitionCore<Empty> &code, CheckerContext &ctx);
ModuleCore<Checks> check(const ModuleCore<Empty> &code, CheckerContext &ctx);

#endif // #ifndef CHECKER_H
