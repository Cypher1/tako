#pragma once
#ifndef PARSER_H
#define PARSER_H

#include <map>
#include <optional>

#include "ast.h"

using Symbol = std::string;            // For now.
using Path = std::vector<Symbol>; // For now.

using SymbolPair = std::pair<Symbol, std::optional<Definition>>;
using SymbolMap = Forest<SymbolPair>;

namespace parser {

class SymbolTable {
  Tree<SymbolPair> symbol_tree;

public:
  SymbolTable(): symbol_tree{{"", {}}, {}} {};

  void addSymbol(const Path &path, const Definition &val);
  std::optional<Definition> lookup(const Path &path, const Path &val);
};

class ParserContext {
public:
  Context &context;
  SymbolTable symbols;
  // Should just be the 'root' / current module.

  ParserContext(Context &ctx) : context{ctx} {}

  void msg(const Token &tok, MessageType level, std::string msg_txt);

  std::string getStringAt(const Location &loc);

  void addSymbol(const Path &path, const Definition &val);
  std::optional<Definition> lookup(const Path &context, const Path &path);

  std::vector<Path> getSymbols();
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
