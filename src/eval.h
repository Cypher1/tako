#ifndef EVAL_H
#define EVAL_H

#include <optional>
#include <string>
#include <variant>
#include <vector>

#include "ast.h"
#include "context.h"
#include "enums.h"
#include "parser.h"
#include "util.h"

using OptPrim = std::optional<Prim>;
using Prims = std::vector<Prim>;
using TryPrim = std::function<Prim()>;
using TryPrims = std::vector<TryPrim>;
using Pred = std::function<bool()>;

Prim eval(Path context, Value val, parser::ParserContext &p_ctx);
Prim eval(Path context, Definition val, parser::ParserContext &p_ctx);
Prim eval(Path context, Module val, parser::ParserContext &p_ctx);

std::ostream &operator<<(std::ostream &o, const PrimError &e);
std::ostream &operator<<(std::ostream &o, const Prim &e);

#endif // #ifndef EVAL_H
