#pragma once
#ifndef AST_INTERNAL_H
#define AST_INTERNAL_H

#include "ast.h"
#include "lex.h"
#include <functional>
#include <map>
#include <set>
#include <string>
#include <vector>

struct ParserContext;
using leftBindingPowerType =
    std::function<unsigned int(const Token &tok, ParserContext &ctx)>;
using parseType = std::function<Forest<Token>(
    Forest<Token> left, const Token &tok, ParserContext &ctx)>;

class SymbolTableEntry {
public:
  leftBindingPowerType binding; // lbp
  parseType parse;              // Led

  SymbolTableEntry(const leftBindingPowerType& binding, const parseType& parse)
      : binding{binding}, parse{parse} {}
};

Forest<Token> expression(ParserContext &ctx, unsigned int rbp = 0);

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
#endif // #ifndef AST_INTERNAL_H
