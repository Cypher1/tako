#include "util.h"

#include "checker.h"

std::optional<Contradiction> Assignment::setValueToInternal(std::string name,
                                                            Value value) {
  auto value_it = assignment.find(name);
  if (value_it == assignment.end()) {
    assignment.emplace(name, value);
    return {};
  }

  // TODO ...
  return {};
}

CheckedModule check(Module module, Context &ctx) {
  ctx.startStep(PassStep::Check);

  // TODO...
  return {};
}
