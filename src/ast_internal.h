#ifndef AST_INTERNAL_H
#define AST_INTERNAL_H

#include <functional>
#include <map>
#include <set>
#include <stdexcept>
#include <string>
#include <vector>

#include "context.h"
#include "ast.h"
#include "lex.h"
#include "toString.h"

const Token eofToken = {TokenType::Error, errorLocation};
const Token errorToken = {TokenType::Error, errorLocation};

class SymbolTableEntry;

class ParserContext {
public:
  Context &context;
  std::vector<Token>::const_iterator toks;
  std::vector<Token>::const_iterator end;

  ParserContext(Context &ctx, std::vector<Token>::const_iterator toks,
                std::vector<Token>::const_iterator end)
      : context{ctx}, toks{toks}, end{end} {}
  // Other state
  bool inString = false; // TODO
  bool hasToken = true;

  bool next();
  bool expect(const TokenType &expected);
  const Token &getCurr() const;
  const SymbolTableEntry entry();
  std::string getCurrString() const;

  void msg(MessageType level, std::string msg_txt);
};

using leftBindingPowerType =
    std::function<unsigned int(const Token &tok, ParserContext &ctx)>;
using parseInitType =
    std::function<Tree<Token>(const Token &tok, ParserContext &ctx)>;
using parseLeftType = std::function<Tree<Token>(
    Tree<Token> left, const Token &tok, ParserContext &ctx)>;

Tree<Token> parserLogicErrorInit(const Token &tok, ParserContext &ctx) {
  throw std::runtime_error("Parser logic error on token " + toString(tok, ctx.context));
};

Tree<Token> parserLogicErrorLeft(Tree<Token>, const Token &tok,
                                ParserContext &ctx) {
  throw std::runtime_error("Parser logic error on token left " +
                           toString(tok, ctx.context));
};

class SymbolTableEntry {
public:
  leftBindingPowerType binding; // lbp
  const parseInitType nud;
  const parseLeftType led;

  SymbolTableEntry(const leftBindingPowerType binding, const parseLeftType led)
      : SymbolTableEntry(binding, parserLogicErrorInit, led) {}
  SymbolTableEntry(const leftBindingPowerType binding, const parseInitType nud)
      : SymbolTableEntry(binding, nud, parserLogicErrorLeft) {}
  SymbolTableEntry(const leftBindingPowerType binding, const parseInitType nud,
                   const parseLeftType led)
      : binding{binding}, nud{nud}, led{led} {}
};

Tree<Token> parseValue(ParserContext &ctx, unsigned int rbp = 0);
Tree<Token> parseDefinition(ParserContext &ctx, unsigned int rbp = 0);
#endif // #ifndef AST_INTERNAL_H
