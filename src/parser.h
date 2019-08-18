#pragma once
#ifndef PARSER_H
#define PARSER_H

#include <map>
#include <optional>

#include "ast.h"

using Symbol = std::string;            // For now.
using Path = std::vector<std::string>; // For now.

namespace parser {

class SymbolTable {
  std::map<Path, Definition> symbol_tree;

public:
  SymbolTable() = default;

  void addSymbol(std::vector<Symbol> path, const Definition &val);
  Path lookup(Path pth, Definition &val);
};

class ParserContext {
public:
  Context &context;
  SymbolTable symbols;

  ParserContext(Context &ctx) : context{ctx} {}

  void msg(const Token &tok, MessageType level, std::string msg_txt);

  std::string getStringAt(const Location &loc);

  void addSymbol(std::vector<Symbol> path, const Definition &val);
  Path lookup(Path pth, Definition &val);
};

std::optional<Value> parseValue(Path pth, const Tree<Token> &node,
                                ParserContext &ctx);
std::optional<Definition> parseDefinition(Path pth, const Tree<Token> &node,
                                          ParserContext &ctx);
Module parseModule(Path pth, const Tree<Token> &node, ParserContext &ctx);

template <typename T>
T parse(const Tree<Token> &node, Context &context,
        std::function<T(Path pth, const Tree<Token> &, ParserContext &ctx)> converter) {
  context.startStep(PassStep::Parse);
  ParserContext ctx(context);
  Path pth;
  return converter(pth, node, ctx);
}

} // namespace parser
#endif // #ifndef PARSER_H
