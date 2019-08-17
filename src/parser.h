#pragma once
#ifndef PARSER_H
#define PARSER_H

#include <optional>
#include <map>

#include "ast.h"

using Symbol = std::string; // For now.
using Path = std::vector<std::string>; // For now.

namespace parser {

class SymbolTable {
  std::map<Path, Value> symbol_tree;

  SymbolTable() = default;

  void addSymbol(std::vector<Symbol> path, const Value &val);

  Path lookup(Path pth, Value &val);
};

class ParserContext {
public:
  Context &context;

  ParserContext(Context &ctx): context{ctx} {}

  void msg(const Token &tok, MessageType level, std::string msg_txt);

  std::string getStringAt(const Location &loc);
};

std::optional<Value> parseValue(const Tree<Token> &node, ParserContext &ctx);
std::optional<Definition> parseDefinition(const Tree<Token> &node,
                                          ParserContext &ctx);
Module parseModule(const Tree<Token> &node, ParserContext &ctx);

template <typename T>
T parse(const Tree<Token> &node, Context &context,
        std::function<T(const Tree<Token> &, ParserContext &ctx)> converter) {
  context.startStep(PassStep::Parse);
  ParserContext ctx(context);
  return converter(node, ctx);
}

} // namespace parser
#endif // #ifndef PARSER_H
