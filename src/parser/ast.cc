#include <iostream>
#include <vector>
#include <map>
#include <set>
#include <string>
#include "ast.h"
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

const std::map<TokenType, TokenType> close_brackets = [](){
  std::map<TokenType, TokenType>map;
  for(auto kv : brackets) {
    if(brackets.find(kv.second) == brackets.end()) {
      map.emplace(kv.second, kv.first);
    }
  }
  return map;
}();

const std::map<std::string, unsigned int> precedence = {
  {"(", 10},
  {"^", 20},
  {"*", 40},
  {"/", 40},
  {"+", 50},
  {"-", 50},
  {"=", 60},
  {",", 70},
};

constexpr bool isOperator(const TokenType& type) {
  return type == +TokenType::Operator
  || type == +TokenType::Comma
  || type == +TokenType::Declaration
  || type == +TokenType::OpenParen;
}

constexpr bool isQuote(const TokenType& type) {
  return type == +TokenType::SingleQuote || type == +TokenType::DoubleQuote || type == +TokenType::BackQuote;
}

struct ParserContext {
  std::vector<Token>::const_iterator toks;
  std::vector<Token>::const_iterator end;
  Messages msgs;
  const std::string& content;
  const std::string& filename;

  PassStep step = PassStep::Init;

  bool next() {
    if(this->curr()) {
      this->toks++;
      if(this->toks != this->end) {
        return true;
      }
    }
    return false;
  }

  bool curr() {
    return (this->toks != this->end);
  }

  TokenType currType() {
    if(this->toks != this->end) {
      return this->toks->type;
    } else {
      throw "Unexpected end of content";
    }
  }

  void changeStep(PassStep step) {
    this->step = step;
  };

  void msg(MessageType level, std::string msg) {
    // TODO make this read EOF
    Location loc = {0, 0, this->filename};
    if(this->curr()) {
      loc = this->toks->loc;
    }
    this->msgs.push_back({
      this->step,
      level,
      msg,
      loc
    });
  }
};

int leftBindingPower(TokenType type) {
  return 0;
}

Forest<Token> parseStartWith(TokenType type, ParserContext& ctx) {
  return {}; //TODO
}

template<typename T>
Forest<Token> parseWithLeftAnd(Forest<Token> left, T curr, ParserContext& ctx) {
  return {}; //TODO
}


Forest<Token> parseExpr(ParserContext& ctx, int rbp=0) {
  auto type = ctx.currType();
  Forest<Token> left = parseStartWith(type, ctx);
  while(rbp < leftBindingPower(type) && ctx.next()) {
    left = parseWithLeftAnd(left, t, ctx);
  }
  return left;
}

Tree<Token> ast(Tokens& toks, Messages& msgs, const std::string& content, const std::string& filename) {
  ParserContext ctx = {
    toks.cbegin(),
    toks.cend(),
    msgs,
    content,
    filename
  };
  Forest<Token> module = parseExpr(ctx);
  msgs = ctx.msgs;
  Token fileToken = {TokenType::Symbol, {0, 0, filename}};
  return Tree<Token>(fileToken, module);
}
