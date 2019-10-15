#include <functional>
#include <iostream>
#include <map>
#include <set>
#include <stdexcept> // TODO: Remove use of exceptions, instead use messages and fallback.
#include <string>
#include <vector>

#include "context.h"

#include "ast.h"
#include "ast_internal.h"
#include "lex.h"
#include "util.h"

const std::map<TokenType, TokenType> brackets = {
    {TokenType::OpenParen, TokenType::CloseParen},
    {TokenType::OpenBrace, TokenType::CloseBrace},
    {TokenType::OpenBracket, TokenType::CloseBracket},
    // {TokenType::SingleQuote, TokenType::SingleQuote},
    // {TokenType::DoubleQuote, TokenType::DoubleQuote},
    // {TokenType::BackQuote, TokenType::BackQuote},
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

/*
constexpr bool isQuote(const TokenType &type) {
  return type == +TokenType::SingleQuote || type == +TokenType::DoubleQuote ||
         type == +TokenType::BackQuote;
}
*/

const std::map<std::string, unsigned int> symbol_binding = {};
const leftBindingPowerType symbolBind = [](const Token &tok,
                                           const AstContext &ctx) {
  auto p_it = symbol_binding.find(ctx.context.getStringAt(tok.loc));
  if (p_it == symbol_binding.end()) {
    return 0u;
  }
  return p_it->second;
};

// Fixity classes
using fixity = unsigned int;
const fixity definitionF = 40;
const fixity pipeF = definitionF + 10;
const fixity comparisonF = pipeF + 10;
const fixity shiftF = comparisonF + 10;
const fixity plusMinF = shiftF + 10;
const fixity timeDivF = plusMinF + 10;
const fixity prefix = timeDivF + 10;
const fixity nameSpaceF = prefix + 10;
const fixity bracketF = nameSpaceF + 10;

const std::map<std::string, fixity> infix_binding = {
    {",", 10},           {"-|", 20},          {"|-", 30},
    {"=", definitionF},  {"+=", definitionF}, {"-=", definitionF},
    {"*=", definitionF}, {"/=", definitionF}, {"&=", definitionF},
    {"|=", definitionF}, {"?", pipeF},        {"<|", pipeF},
    {"|>", pipeF},       {"<", comparisonF},  {"<=", comparisonF},
    {">", comparisonF},  {">=", comparisonF}, {"<>", comparisonF},
    {"!=", comparisonF}, {"==", comparisonF}, {"|", shiftF},
    {"^", shiftF},       {"&", shiftF},       {"<<", shiftF},
    {">>", shiftF},      {"+", plusMinF},     {"-", plusMinF},
    {"*", timeDivF},     {"/", timeDivF},     {"%", timeDivF},
    {":", nameSpaceF},   {".", nameSpaceF},   {"[", bracketF},
    {"(", bracketF},     {"{", bracketF}};

const std::map<std::string, unsigned int> prefix_binding = {
    {"-", prefix}, {"+", prefix}, {"!", prefix}};

const auto operatorBind = [](const Token &tok, AstContext &ctx) { // Lbp
  std::string t = ctx.context.getStringAt(tok.loc);
  auto p_it = infix_binding.find(t);
  if (p_it == infix_binding.end()) {
    std::string start = std::to_string(tok.loc.start);
    std::string len = std::to_string(tok.loc.length);
    throw std::runtime_error(
        std::string() + "Expected an infix operator but found a " +
        tok.type._to_string() + "(" + start + "," + len + ") '" + t + "'");
  }
  return p_it->second;
};

Tree<Token> prefixOp(const Token &tok, AstContext &ctx) {
  auto root = Tree<Token>(tok);
  std::string t = ctx.context.getStringAt(tok.loc);
  auto p_it = prefix_binding.find(t);
  if (p_it == prefix_binding.end()) {
    ctx.msg(MessageType::Error, "Unknown prefix operator");
  }
  auto right = ast::parseValue(ctx, p_it->second);
  root.children.push_back(right);
  return root;
};

Tree<Token> infixOp(Tree<Token> left, const Token &tok, AstContext &ctx) {
  auto root = Tree<Token>(tok, {left});
  // Led
  auto p_it = infix_binding.find(ctx.context.getStringAt(tok.loc));
  if (p_it == infix_binding.end()) {
    ctx.msg(MessageType::Error, "Unknown infix operator");
  };
  auto right = ast::parseValue(ctx, p_it->second);
  root.children.push_back(right);
  return root;
};

Tree<Token> symbol(const Token &tok, AstContext &ctx) { // Led
  return Tree<Token>(tok);
};

Tree<Token> ignoreInit(const Token &, AstContext &) {
  return {errorToken, {}};
};
Tree<Token> ignore(const Tree<Token> left, const Token &, AstContext &) {
  return left;
};

Tree<Token> bracket(const Token &tok, AstContext &ctx) { // Nud
  std::vector<Tree<Token>> inner;
  const auto close_it = brackets.find(tok.type);
  if (close_it == brackets.end()) {
    throw std::runtime_error(std::string() + "Unknown bracket type " +
                             tok.type._to_string());
  }
  const auto closeTT = close_it->second;
  while (ctx.hasToken && (ctx.getCurr().type != closeTT)) {
    auto exp = ast::parseValue(ctx);
    inner.push_back(exp);
  }
  ctx.expect(closeTT);

  return {tok, inner};
};
Forest<Token> simplifyCommasAndParens(Forest<Token> nodes) {
  Forest<Token> children;
  for (auto &node : nodes) {
    // Add the children
    node.children = simplifyCommasAndParens(node.children);

    const bool isComma = node.value.type == +TokenType::Comma;
    const bool isParen = node.value.type == +TokenType::OpenParen;
    const bool isParenthesizedExpr = isParen && node.children.size() == 1;
    if (isComma || isParenthesizedExpr) {
      // Add the children
      children.insert(children.end(), node.children.begin(),
                      node.children.end());
    } else {
      // Tuple, unit or other expr
      // Add the node
      children.push_back(node);
    }
  }
  return children;
}

Tree<Token> funcArgs(Tree<Token> left, const Token &tok,
                     AstContext &ctx) { // Led
  std::vector<Tree<Token>> inner;
  const auto close_it = brackets.find(tok.type);
  if (close_it == brackets.end()) {
    throw std::runtime_error(std::string() + "Unknown bracket type " +
                             tok.type._to_string());
  }
  const auto closeTT = close_it->second;
  while (ctx.hasToken && (ctx.getCurr().type != closeTT)) {
    auto exp = ast::parseValue(ctx);
    inner.push_back(exp);
  }
  ctx.expect(closeTT);
  left.children = simplifyCommasAndParens(inner);
  return left; // This is a function call
};

std::map<TokenType, TokenTokenEntry> symbolTable = {
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
    // {TokenType::DoubleQuote,
    // {operatorBind, bracket}}, // TODO: Warning / error on unmatched.
    // {TokenType::SingleQuote,
    // {operatorBind, bracket}}, // TODO: Warning / error on unmatched.
    // {TokenType::BackQuote,
    // {operatorBind, bracket}}, // TODO: Warning / error on unmatched.
    {TokenType::StringLiteral, {symbolBind, symbol}},
    {TokenType::NumberLiteral, {symbolBind, symbol}},
    {TokenType::Dot, {symbolBind, symbol}},
    {TokenType::Error, {symbolBind, symbol}},
};

bool AstContext::next() {
  if (hasToken) {
    // std::cout << "> " << context.getStringAt(getCurr().loc) << "\n"; // For
    // debugging.
    if (toks != end) {
      toks++;
    }
    if (toks != end) {
      if (toks->type == +TokenType::WhiteSpace ||
          toks->type == +TokenType::SemiColon) {
        return next(); // TODO instring...
      }
      return true;
    }
  }
  hasToken = false;
  return false;
}

bool AstContext::expect(const TokenType &expected) {
  if (getCurr().type != expected) {
    msg(MessageType::Error,
        std::string() + "Expected a " + expected._to_string() + " but found " +
            getCurr().type._to_string() + " '" + getCurrString() + "'");
  }
  return next();
}

void AstContext::msg(MessageType level, std::string msg_txt) {
  // TODO make this print as EOF
  Location loc = eofToken.loc;
  if (hasToken) {
    loc = toks->loc;
  }
  context.msg(loc, level, msg_txt);
}

const Token &AstContext::getCurr() const {
  if (hasToken) {
    return *toks;
  }
  return eofToken;
}

std::string AstContext::getCurrString() const {
  return context.getStringAt(getCurr().loc);
}

const TokenTokenEntry AstContext::entry() {
  auto t = getCurr();
  const auto symbol_it = symbolTable.find(t.type);
  if (symbol_it == symbolTable.end()) {
    throw std::runtime_error(std::string() + t.type._to_string() + +" '" +
                             getCurrString() + "' not found in symbol table");
  }
  return symbol_it->second;
}

Tree<Token> ast::parseDefinition(AstContext &ctx, unsigned int rbp) {
  // TODO check this is a value (merge with parse pass?)
  return ast::parseValue(ctx, rbp);
}

Tree<Token> ast::parseValue(AstContext &ctx, unsigned int rbp) {
  if (!ctx.hasToken) {
    return {eofToken, {}};
  }
  unsigned int binding = 0;
  Token t = ctx.getCurr();
  const auto t_entry = ctx.entry();
  ctx.next(); // TODO: Check result
  Tree<Token> left = t_entry.nud(t, ctx);
  binding = ctx.entry().binding(ctx.getCurr(), ctx);
  while (rbp < binding && ctx.hasToken) {
    t = ctx.getCurr();
    const auto t_entry = ctx.entry();
    ctx.next(); // TODO: Check result
    left = t_entry.led(left, t, ctx);
    binding = ctx.entry().binding(ctx.getCurr(), ctx);
  }
  left.children = simplifyCommasAndParens(left.children);
  return left;
}

Tree<Token> ast::parseModule(AstContext &ctx, unsigned int rbp) {
  Forest<Token> definitions;
  while (ctx.hasToken) {
    definitions.push_back(parseDefinition(ctx));
  }
  Token fileToken = {TokenType::Symbol, {0, 0, ctx.context.filename}};
  return Tree<Token>(fileToken, definitions);
}

std::optional<Tree<Token>>
ast::ast(Tokens &toks, Context &context,
         std::function<Tree<Token>(AstContext &, unsigned int)> func) {
  context.startStep(PassStep::Ast);
  AstContext ctx(context, toks.cbegin(), toks.cend());
  if (!ctx.hasToken)
    return std::nullopt;
  // Allows next to consume whitespace without prepending to the tokens.
  ctx.toks--;
  ctx.next(); // TODO: Check result
  if (!ctx.hasToken)
    return std::nullopt;
  return func(ctx, 0);
}
