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
#include "show.h"

const Token eofToken = {TokenType::Error, errorLocation};
const Token errorToken = {TokenType::Error, errorLocation};

class SymbolTableEntry;

class AstContext {
public:
  Context &context;
  std::vector<Token>::const_iterator toks;
  std::vector<Token>::const_iterator end;

  AstContext(Context &ctx, std::vector<Token>::const_iterator toks,
                std::vector<Token>::const_iterator end)
      : context{ctx}, toks{toks}, end{end} {}
  // Other state
  bool hasToken = true;

  bool next();
  bool expect(const TokenType &expected);
  const Token &getCurr() const;
  const SymbolTableEntry entry();
  std::string getCurrString() const;

  void msg(MessageType level, std::string msg_txt);
};

using leftBindingPowerType =
    std::function<unsigned int(const Token &tok, AstContext &ctx)>;
using parseInitType =
    std::function<Tree<Token>(const Token &tok, AstContext &ctx)>;
using parseLeftType = std::function<Tree<Token>(
    Tree<Token> left, const Token &tok, AstContext &ctx)>;

Tree<Token> parserLogicErrorInit(const Token &tok, AstContext &ctx) {
  throw std::runtime_error("Parser logic error on token " + show(tok, ctx.context));
};

Tree<Token> parserLogicErrorLeft(Tree<Token>, const Token &tok,
                                AstContext &ctx) {
  throw std::runtime_error("Parser logic error on token left " +
                           show(tok, ctx.context));
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

#endif // #ifndef AST_INTERNAL_H
