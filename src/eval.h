#ifndef EVAL_H
#define EVAL_H

#include <optional>
#include <string>
#include <variant>
#include <vector>

#include "ast.h"
#include "context.h"
#include "parser.h"
#include "enums.h"
#include "util.h"

struct PrimError {
  std::string msg;

  PrimError(std::string msg): msg{msg} {
  }
};

using Prim = std::variant<int, std::string, PrimError>;
using OptPrim = std::optional<Prim>;
using Prims = std::vector<Prim>;
using TryPrim = std::function<Prim()>;
using TryPrims = std::vector<TryPrim>;
using Pred = std::function<bool()>;

Prim eval(Value val, parser::ParserContext& p_ctx);
Prim eval(Definition val, parser::ParserContext& p_ctx);
Prim eval(Module val, parser::ParserContext& p_ctx);

std::ostream& operator<<(std::ostream& o, const PrimError& e);
std::ostream& operator<<(std::ostream& o, const Prim& e);

#endif // #ifndef EVAL_H
