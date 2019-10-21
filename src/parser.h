#pragma once
#ifndef PARSER_H
#define PARSER_H

#include <functional>
#include <map>
#include <optional>

#include "ast.h"

using Symbol = std::string;       // For now.
using Path = std::vector<Symbol>; // For now.

using SymbolPair = std::pair<Symbol, std::optional<Definition>>;
using SymbolMap = Forest<SymbolPair>;

namespace parser {

class SymbolTable {
  Tree<SymbolPair> symbol_tree;

public:
  SymbolTable() : symbol_tree{{"", {}}, {}} {};

  void addSymbol(const Path &path, const Definition &val);
  std::optional<Definition> lookup(const Path &path, const Path &val);

  std::vector<Path> getSymbols(const Path &root);
  void forAll(std::function<void(Path &context, Definition &)> f);
  void
  forAll(std::function<void(const Path &context, const Definition &)> f) const;
};

class ParserContext : public Context {
public:
  SymbolTable symbols;
  // TODO(jopra): Convert to use nested modules that each contain their children
  // nodes.

  ParserContext(Context &&ctx) : Context(std::move(ctx)) {}
  ParserContext(const Context &ctx) = delete;

  void msg(const Token &tok, MessageType level, std::string msg_txt);

  void addSymbol(const Path &path, const Definition &val);
  std::optional<Definition> lookup(const Path &context, const Path &path);

  SymbolTable getTable();
};

std::optional<Value> parseValue(const Path pth, const Tree<Token> &node,
                                ParserContext &ctx);
std::optional<Definition>
parseDefinition(const Path pth, const Tree<Token> &node, ParserContext &ctx);
Module parseModule(const Path pth, const Tree<Token> &node, ParserContext &ctx);

template <typename T>
T parse(
    const Tree<Token> &node, ParserContext &ctx,
    std::function<T(const Path pth, const Tree<Token> &, ParserContext &ctx)>
        converter) {
  ctx.startStep(PassStep::Parse);
  const Path pth;
  return converter(pth, node, ctx);
}

} // namespace parser
#endif // #ifndef PARSER_H
