#include <functional>
#include <iostream>
#include <map>
#include <set>
#include <stdexcept> // TODO: Remove use of exceptions, instead use messages and fallback.
#include <string>
#include <vector>

#include "../util/context.h"

#include "ast.h"
#include "ast_internal.h"
#include "lex.h"
#include "toString.h"

const std::map<TokenType, TokenType> brackets = {
    {TokenType::OpenParen, TokenType::CloseParen},
    {TokenType::OpenBrace, TokenType::CloseBrace},
    {TokenType::OpenBracket, TokenType::CloseBracket},
    {TokenType::SingleQuote, TokenType::SingleQuote},
    {TokenType::DoubleQuote, TokenType::DoubleQuote},
    {TokenType::BackQuote, TokenType::BackQuote},
};

const std::map<TokenType, TokenType> close_brackets = []() {
  std::map<TokenType, TokenType> map;
  for (auto kv : brackets) {
    if (brackets.find(kv.second) == brackets.end()) {
      map.emplace(kv.second, kv.first);
    }
  }
  return map;
}();

constexpr bool isQuote(const TokenType &type) {
  return type == +TokenType::SingleQuote || type == +TokenType::DoubleQuote ||
         type == +TokenType::BackQuote;
}

const std::map<std::string, unsigned int> symbol_binding = {};
const leftBindingPowerType symbolBind = [](const Token &tok,
                                           const ParserContext &ctx) {
  auto p_it = symbol_binding.find(ctx.getStringAt(tok.loc));
  if (p_it == symbol_binding.end()) {
    return 0u;
  }
  return p_it->second;
};

const std::map<std::string, unsigned int> infix_binding = {
    {"-|", 20}, {"|-", 30},  {"=", 40},   {"<", 60},  {"<=", 60}, {">", 60},
    {">=", 60}, {"<>", 60},  {"!=", 60},  {"==", 60}, {"|", 70},  {"^", 80},
    {"&", 90},  {"<<", 100}, {">>", 100}, {"+", 110}, {"-", 110}, {"*", 120},
    {"/", 120}, {"//", 120}, {"%", 120},  {":", 130}, {".", 140}, {"[", 150},
    {"(", 150}, {"{", 150}};

const std::map<std::string, unsigned int> prefix_binding = {
    {"-", 130}, {"+", 130}, {"~", 130}, {"!", 130}};

const auto operatorBind = [](const Token &tok, ParserContext &ctx) { // Lbp
  auto p_it = infix_binding.find(ctx.getStringAt(tok.loc));
  if (p_it == infix_binding.end()) {
    throw std::runtime_error(std::string() +
                             "Expected an infix operator but found '" +
                             ctx.getStringAt(tok.loc) + "'");
  }
  return p_it->second;
};

Tree<Token> prefixOp(const Token &tok, ParserContext &ctx) {
  auto root = Tree<Token>(tok);
  auto p_it = prefix_binding.find(ctx.getStringAt(tok.loc));
  if (p_it == prefix_binding.end()) {
    throw std::runtime_error(std::string() +
                             "Expected a prefix operator but found '" +
                             ctx.getStringAt(tok.loc) + "'");
  }
  auto right = expression(ctx, p_it->second);
  root.children.push_back(right);
  return root;
};

Tree<Token> infixOp(Tree<Token> left, const Token &tok, ParserContext &ctx) {
  auto root = Tree<Token>(tok, {left});
  // Led
  auto p_it = infix_binding.find(ctx.getStringAt(tok.loc));
  if (p_it == infix_binding.end()) {
    // TODO Defaulting is bad...
  };
  auto right = expression(ctx, p_it->second);
  root.children.push_back(right);
  return root;
};

Tree<Token> symbol(const Token &tok, ParserContext &ctx) { // Led
  return Tree<Token>(tok);
};

Tree<Token> ignoreInit(const Token &, ParserContext &) {
  return {errorToken, {}};
};
Tree<Token> ignore(const Tree<Token> left, const Token &, ParserContext &) {
  return left;
};

Tree<Token> bracket(const Token &tok, ParserContext &ctx) { // Nud
  std::vector<Tree<Token>> inner;
  const auto close_it = brackets.find(tok.type);
  if (close_it == brackets.end()) {
    throw std::runtime_error(std::string() + "Unknown bracket type " +
                             tok.type._to_string());
  }
  const auto closeTT = close_it->second;
  while (ctx.hasToken && (ctx.getCurr().type != closeTT)) {
    auto exp = expression(ctx);
    inner.push_back(exp);
  }
  ctx.expect(closeTT);

  return {tok, inner};
};

Tree<Token> funcArgs(Tree<Token> left, const Token &tok,
                     ParserContext &ctx) { // Led
  std::vector<Tree<Token>> inner;
  const auto close_it = brackets.find(tok.type);
  if (close_it == brackets.end()) {
    throw std::runtime_error(std::string() + "Unknown bracket type " +
                             tok.type._to_string());
  }
  const auto closeTT = close_it->second;
  while (ctx.hasToken && (ctx.getCurr().type != closeTT)) {
    auto exp = expression(ctx);
    inner.push_back(exp);
  }
  ctx.expect(closeTT);

  left.children = inner;
  return left; // This is a function call
};

std::map<TokenType, SymbolTableEntry> symbolTable = {
    {TokenType::Comma, {operatorBind, infixOp}},
    {TokenType::Operator, {operatorBind, prefixOp, infixOp}},
    {TokenType::PreCond, {operatorBind, infixOp}},
    {TokenType::PostCond, {operatorBind, infixOp}},
    {TokenType::SemiColon, {symbolBind, ignore}},
    {TokenType::Symbol, {symbolBind, symbol}},
    {TokenType::OpenParen, {operatorBind, bracket, funcArgs}},
    {TokenType::CloseParen,
     {symbolBind, ignoreInit, ignore}}, // TODO: Warning / error on unmatched.
    {TokenType::OpenBrace, {operatorBind, bracket}},
    {TokenType::CloseBrace,
     {symbolBind, ignoreInit, ignore}}, // TODO: Warning / error on unmatched.
    {TokenType::OpenBracket, {operatorBind, bracket}},
    {TokenType::CloseBracket,
     {symbolBind, ignoreInit, ignore}}, // TODO: Warning / error on unmatched.
    {TokenType::DoubleQuote,
     {operatorBind, bracket}}, // TODO: Warning / error on unmatched.
    {TokenType::SingleQuote,
     {operatorBind, bracket}}, // TODO: Warning / error on unmatched.
    {TokenType::BackQuote,
     {operatorBind, bracket}}, // TODO: Warning / error on unmatched.
    {TokenType::NumberLiteral, {symbolBind, symbol}},
    {TokenType::Dot, {symbolBind, symbol}},
    {TokenType::Error, {symbolBind, symbol}},
};

bool ParserContext::next() {
  if (hasToken) {
    // std::cout << "> " << getStringAt(getCurr().loc) << "\n"; // For debugging.
    toks++;
    if (toks != end) {
      if (toks->type == +TokenType::WhiteSpace ||
          toks->type == +TokenType::Comma ||
          toks->type == +TokenType::SemiColon) {
        return next(); // TODO instring...
      }
      return true;
    }
  }
  hasToken = false;
  return false;
}

bool ParserContext::expect(const TokenType &expected) {
  if (getCurr().type != expected) {
    msg(MessageType::Error,
        std::string() + "Expected a " + expected._to_string() + " but found " +
            getCurr().type._to_string() + " '" + getStringAt(getCurr().loc) + "'");
  }
  return next();
}

void ParserContext::msg(MessageType level, std::string msg_txt) {
  // TODO make this print as EOF
  Location loc = eofToken.loc;
  if (hasToken) {
    loc = toks->loc;
  }
  Context::msg(loc, level, msg_txt);
}

const Token &ParserContext::getCurr() {
  if (toks != end) {
    return *(toks);
  } else {
    return eofToken;
    throw std::runtime_error("Unexpected end of content");
  }
}

const SymbolTableEntry ParserContext::entry() {
  auto t = getCurr();
  const auto symbol_it = symbolTable.find(t.type);
  if (symbol_it == symbolTable.end()) {
    throw std::runtime_error(std::string() + t.type._to_string() + +" '" +
                             getStringAt(t.loc) + "' not found in symbol table");
  }
  return symbol_it->second;
}

Tree<Token> expression(ParserContext &ctx, unsigned int rbp) {
  unsigned int binding = 0;
  Token t = ctx.getCurr();
  const auto t_entry = ctx.entry();
  ctx.next();
  Tree<Token> left = t_entry.nud(t, ctx);
  binding = ctx.entry().binding(ctx.getCurr(), ctx);
  while (rbp < binding && ctx.hasToken) {
    t = ctx.getCurr();
    const auto t_entry = ctx.entry();
    ctx.next();
    left = t_entry.led(left, t, ctx);
    binding = ctx.entry().binding(ctx.getCurr(), ctx);
  }
  return left;
}

Tree<Token> ast(Tokens& toks, Context &_ctx) {
  _ctx.startStep(PassStep::Ast);
  // Add a disposable char to make whitespace dropping easy.
  toks.insert(toks.begin(), errorToken);
  ParserContext ctx(_ctx, toks.cbegin(), toks.cend());
  ctx.next();
  Forest<Token> module;
  while (ctx.hasToken) {
    module.push_back(expression(ctx));
  }
  Token fileToken = {TokenType::Symbol, {0, 0, ctx.filename}};
  return Tree<Token>(fileToken, module);
}
