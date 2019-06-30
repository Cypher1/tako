#include "ast.h"
#include "ast_internal.h"
#include "lex.h"
#include "toString.h"
#include <functional>
#include <iostream>
#include <map>
#include <set>
#include <stdexcept> // TODO: Remove use of exceptions, instead use messages and fallback.
#include <string>
#include <vector>

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

const Token eofToken = {TokenType::Error, {0, 0, "<file>?"}};

const std::map<std::string, unsigned int> symbol_binding = {};
const leftBindingPowerType symbolBind = [](const Token &tok,
                                           const ParserContext &ctx) {
  auto p_it = symbol_binding.find(ctx.getStringAt(tok));
  if (p_it == symbol_binding.end()) {
    return 0u;
  }
  return p_it->second;
};

const std::map<std::string, unsigned int>
    infix_binding = {{"-|", 20}, {"|-", 30},
      {"=", 40},   {"<", 60},  {"<=", 60},  {">", 60},
                     {">=", 60},  {"<>", 60}, {"!=", 60},  {"==", 60},
                     {"|", 70},   {"^", 80},  {"&", 90},   {"<<", 100},
                     {">>", 100}, {"+", 110}, {"-", 110},

                     {"*", 120},  {"/", 120}, {"//", 120}, {"%", 120},
                     {".", 140},  {"[", 150}, {"(", 150},  {"{", 150}};


const auto operatorBind = [](const Token &tok, ParserContext &ctx) { // Lbp
      auto p_it = infix_binding.find(ctx.getStringAt(tok));
      if (p_it == infix_binding.end()) {
        throw std::runtime_error(std::string() +
                                 "Expected an infix operator but found '" +
                                 ctx.getStringAt(tok) + "'");
      }
      return p_it->second;
    };

const auto infixEntry = SymbolTableEntry(
    operatorBind,
    [](Forest<Token> left, const Token &tok, ParserContext &ctx) {
      std::cout << ">>> " << (ctx.getStringAt(tok)) << "(" << left.size()
                << ")\n";
      auto root = Tree<Token>(tok, left);
      if (left.empty()) {
        // Nud
        const std::map<std::string, unsigned int> prefix_binding = {
            // {"-", 130}, {"+", 130}, {"~", 130}, {"!", 130}, {"|-", 130},
        };
        auto p_it = prefix_binding.find(ctx.getStringAt(tok));
        if (p_it == prefix_binding.end()) {
          throw std::runtime_error(std::string() +
                                   "Expected a prefix operator but found '" +
                                   ctx.getStringAt(tok) + "'");
        }
        auto right = expression(ctx, p_it->second);
        root.children.insert(root.children.end(), right.begin(), right.end());
      }
      // Led
      auto p_it = infix_binding.find(ctx.getStringAt(tok));
      if (p_it == infix_binding.end()) {
        // TODO Defaulting is bad...
      };
      auto right = expression(ctx, p_it->second);
      root.children.insert(root.children.end(), right.begin(), right.end());
      return std::vector<Tree<Token>>({root});
    });

const auto symbolEntry =
    SymbolTableEntry(symbolBind, [](Forest<Token> left, const Token &tok,
                                    ParserContext &ctx) { // Led
      return std::vector<Tree<Token>>({{tok, {}}});
    });

const auto ignoreEntry =
    SymbolTableEntry(symbolBind, [](Forest<Token> left, const Token &,
                                    ParserContext &) { return left; });

const auto bracketEntry =
    SymbolTableEntry(operatorBind, [](Forest<Token> left, const Token &tok,
                                    ParserContext &ctx) { // Led
      std::vector<Tree<Token>> inner;
      const auto close_it = brackets.find(tok.type);
      if (close_it == brackets.end()) {
        throw std::runtime_error(std::string() + "Unknown bracket type " +
                                 tok.type._to_string());
        std::cout << "Closing\n";
      }
      const auto closeTT = close_it->second;
      while (ctx.hasToken && (ctx.getCurr().type != closeTT)) {
        auto exp = expression(ctx);
        inner.insert(inner.end(), exp.begin(), exp.end());
      }
      ctx.expect(closeTT);

      if(left.empty()) {
        return inner; // This is a group of parens
      }
      left.back().children = inner;
      return left; // This is a function call
    });

std::map<TokenType, SymbolTableEntry> symbolTable = {
    {TokenType::Comma, infixEntry},
    {TokenType::Operator, infixEntry},
    {TokenType::PreCond, infixEntry},
    {TokenType::PostCond, infixEntry},
    {TokenType::SemiColon, ignoreEntry},
    {TokenType::Symbol, symbolEntry},
    {TokenType::OpenParen, bracketEntry},
    {TokenType::CloseParen, ignoreEntry}, // TODO: Warning / error on unmatched.
    {TokenType::OpenBrace, bracketEntry},
    {TokenType::CloseBrace, ignoreEntry}, // TODO: Warning / error on unmatched.
    {TokenType::OpenBracket, bracketEntry},
    {TokenType::CloseBracket, ignoreEntry}, // TODO: Warning / error on unmatched.
    {TokenType::DoubleQuote, bracketEntry}, // TODO: Warning / error on unmatched.
    {TokenType::SingleQuote, bracketEntry}, // TODO: Warning / error on unmatched.
    {TokenType::BackQuote, bracketEntry}, // TODO: Warning / error on unmatched.
    {TokenType::NumberLiteral, symbolEntry},
    {TokenType::Dot, symbolEntry},
    {TokenType::Error, symbolEntry},
};

bool ParserContext::next() {
  if (hasToken) {
    // std::cout << "> " << getStringAt(getCurr()) << "\n";
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
            getCurr().type._to_string() + " '" + getStringAt(getCurr()) + "'");
  }
  return next();
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
                             getStringAt(t) + "' not found in symbol table");
  }
  return symbol_it->second;
}

std::string ParserContext::getStringAt(const Token &tok) const {
  return getString(tok.loc, content);
}

void ParserContext::startStep(PassStep step) { step = step; };

void ParserContext::msg(MessageType level, std::string msg) {
  // TODO make this print as EOF
  Location loc = eofToken.loc;
  if (hasToken) {
    loc = toks->loc;
  }
  msgs.push_back({step, level, msg, loc});
}

Forest<Token> expression(ParserContext &ctx, unsigned int rbp) {
  unsigned int binding = 0;
  Forest<Token> left;
  do {
    Token t = ctx.getCurr();
    const auto t_entry = ctx.entry();
    ctx.next();
    left = t_entry.parse(left, t, ctx);
    binding = ctx.entry().binding(ctx.getCurr(), ctx);
  } while (rbp < binding && ctx.hasToken);
  std::cout << "HERE " << binding << " :: " << rbp << "\n";
  return left;
}

Tree<Token> ast(Tokens &toks, Messages &msgs, const std::string &content,
                const std::string &filename) {
  Token fileToken = {TokenType::Symbol, {0, 0, filename}};
  // file token does double duty as a skipable first character (what a mess).
  toks.insert(toks.begin(), fileToken);
  ParserContext ctx = {toks.cbegin(), toks.cend(), msgs, content, filename};
  ctx.startStep(PassStep::Ast);
  ctx.next();
  Forest<Token> module;
  while(ctx.hasToken) {
    Forest<Token> definition = expression(ctx);
    std::cout << "------------------------------------------\n";
    std::cout << toString(definition, ctx.content, ctx.filename) << "\n";
    std::cout << "------------------------------------------\n";
    module.insert(module.end(), definition.begin(), definition.end());
  }
  msgs = ctx.msgs;
  std::cout << "-- Done: " << !ctx.hasToken << " --\n";
  return Tree<Token>(fileToken, module);
}
