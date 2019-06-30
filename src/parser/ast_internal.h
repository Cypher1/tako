#pragma once
#ifndef AST_INTERNAL_H
#define AST_INTERNAL_H

#include "ast.h"
#include "lex.h"
#include "toString.h"
#include <functional>
#include <stdexcept>
#include <map>
#include <set>
#include <string>
#include <vector>


struct SymbolTableEntry;

struct ParserContext {
  std::vector<Token>::const_iterator toks;
  std::vector<Token>::const_iterator end;
  Messages msgs;

  const std::string &content;
  const std::string &filename;

  // Other state
  bool inString = false; // TODO
  bool hasToken = true;
  PassStep step = PassStep::Init;

  bool next();
  bool expect(const TokenType &expected);
  const Token &getCurr();
  const SymbolTableEntry entry();
  std::string getStringAt(const Token &tok) const;
  void startStep(PassStep step);
  void msg(MessageType level, std::string msg);
};

using leftBindingPowerType =
    std::function<unsigned int(const Token &tok, ParserContext &ctx)>;
using parseInitType = std::function<Tree<Token>(const Token &tok, ParserContext &ctx)>;
using parseLeftType = std::function<Tree<Token>(Tree<Token> left, const Token &tok, ParserContext &ctx)>;

Tree<Token> parseLogicErrorInit(const Token &tok, ParserContext &ctx) {
  throw std::runtime_error("Parser logic error on token "+toString(tok, ctx.content, ctx.filename));
};

Tree<Token> parseLogicErrorLeft(Tree<Token>, const Token &tok, ParserContext &ctx) {
  throw std::runtime_error("Parser logic error on token left "+toString(tok, ctx.content, ctx.filename));
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
  SymbolTableEntry(const leftBindingPowerType binding, const parseInitType nud, const parseLeftType led)
      : binding{binding}, nud{nud}, led{led} {}
};

Tree<Token> expression(ParserContext &ctx, unsigned int rbp = 0);
#endif // #ifndef AST_INTERNAL_H
