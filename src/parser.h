#pragma once
#ifndef PARSER_H
#define PARSER_H

#include <optional>

#include "ast.h"

namespace parser {

std::optional<Value> parseValue(const Tree<Token> &node, Context &ctx);
std::optional<Definition> parseDefinition(const Tree<Token> &node,
                                          Context &ctx);
Module parseModule(const Tree<Token> &node, Context &ctx);

template <typename T>
T parse(const Tree<Token> &node, Context &ctx,
        std::function<T(const Tree<Token> &, Context &ctx)> converter) {
  ctx.startStep(PassStep::Parse);
  return converter(node, ctx);
}

} // namespace parser
#endif // #ifndef PARSER_H
