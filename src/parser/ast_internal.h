#pragma once
#ifndef AST_INTERNAL_H
#define AST_INTERNAL_H

#include "../util/context.h"
#include "ast.h"
#include "lex.h"
#include "toString.h"
#include <functional>
#include <map>
#include <set>
#include <stdexcept>
#include <string>
#include <vector>

const Token eofToken = {TokenType::Error, {0, 0, "<file>?"}};
const Token errorToken = {TokenType::Error, {0, 0, "<file>?"}};

struct SymbolTableEntry;

class ParserContext : public Context {
public:
  std::vector<Token>::const_iterator toks;
  std::vector<Token>::const_iterator end;

  ParserContext(Context ctx, std::vector<Token>::const_iterator toks,
                std::vector<Token>::const_iterator end)
      : Context(ctx), toks{toks}, end{end} {}
  // Other state
  bool inString = false; // TODO
  bool hasToken = true;

  bool next();
  bool expect(const TokenType &expected);
  const Token &getCurr();
  const SymbolTableEntry entry();

  void msg(MessageType level, std::string msg_txt);
};

using leftBindingPowerType =
    std::function<unsigned int(const Token &tok, ParserContext &ctx)>;
using parseInitType =
    std::function<Tree<Token>(const Token &tok, ParserContext &ctx)>;
using parseLeftType = std::function<Tree<Token>(
    Tree<Token> left, const Token &tok, ParserContext &ctx)>;

Tree<Token> parseLogicErrorInit(const Token &tok, ParserContext &ctx) {
  throw std::runtime_error("Parser logic error on token " +
                           toString(tok, ctx));
};

Tree<Token> parseLogicErrorLeft(Tree<Token>, const Token &tok,
                                ParserContext &ctx) {
  throw std::runtime_error("Parser logic error on token left " +
                           toString(tok, ctx));
};

class SymbolTableEntry {
public:
  leftBindingPowerType binding; // lbp
  const parseLeftType led;
  const parseInitType nud;

  SymbolTableEntry(const leftBindingPowerType binding, const parseLeftType led)
      : SymbolTableEntry(binding, parseLogicErrorInit, led) {}
  SymbolTableEntry(const leftBindingPowerType binding, const parseInitType nud)
      : SymbolTableEntry(binding, nud, parseLogicErrorLeft) {}
  SymbolTableEntry(const leftBindingPowerType binding, const parseInitType nud,
                   const parseLeftType led)
      : binding{binding}, nud{nud}, led{led} {}
};

Tree<Token> expression(ParserContext &ctx, unsigned int rbp = 0);
#endif // #ifndef AST_INTERNAL_H
