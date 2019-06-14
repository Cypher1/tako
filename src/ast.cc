#include <iostream>
#include <vector>
#include <map>
#include <string>
#include "ast.h"

const std::string lower = "abcdefghijklmnopqrstuvwxyz";
const std::string upper = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
const std::string nums = "0123456789";

const std::string whiteSpace = " \t\n\r";
const std::string numberChar = "."+nums;
const std::string operatorChar = "-+&#@<>^~∆%•|=÷×°$\\/*:?!.;";
const std::string symbolChar = lower+upper+nums+"_";
const TokenType comma = TokenType::Comma;
const TokenType white = TokenType::WhiteSpace;
const TokenType quote = TokenType::SingleQuote;
const TokenType doublequote = TokenType::DoubleQuote;
const TokenType backquote = TokenType::BackQuote;
const TokenType openbrace = TokenType::OpenBrace;

const std::vector<std::pair<std::string, TokenType>> matchToken = {
  {"(", TokenType::OpenParen},
  {")", TokenType::CloseParen},
  {"{", TokenType::OpenBrace},
  {"}", TokenType::CloseBrace},
  {"[", TokenType::OpenBracket},
  {"]", TokenType::CloseBracket},
  {"'", TokenType::SingleQuote},
  {"\"", TokenType::DoubleQuote},
  {"`", TokenType::BackQuote},
  {"-|", TokenType::PreCond},
  {"|-", TokenType::PostCond},
  {".", TokenType::Dot},
  {",", TokenType::Comma},
  {"=", TokenType::Definition}
};

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

Offset consumeWhiteSpace(const std::string content) {
  Offset loc = 0;
  for(;loc < content.size(); loc++) {
    char cur = content[loc];
    if(whiteSpace.find(cur) != std::string::npos) {
      continue;
    }
    if(cur == '/' && loc+1 < content.size() && content[loc+1] == '*') {
      loc++;
      for(loc++; loc < content.size(); loc++) {
        if(content[loc] == '/' && content[loc-1] == '*') break;
      }
      loc++;
    }
    if(cur == '/' && loc+1 < content.size() && content[loc+1] == '/') {
      loc++;
      for(loc++; loc < content.size(); loc++) {
        if(content[loc] == '\n') break;
      }
    }
    break;
  }
  return loc;
}

int matchesFrom(const std::string chars, const std::string content) {
  Offset length = 0;
  while(length < content.size()) {
    if(chars.find(content[length]) == std::string::npos) {
      break;
    }
    length++;
  }
  return length;
}

std::pair<TokenType, Offset> chooseTok(std::string content, Messages& msgs) {
  // TODO: use string views.
  for(const auto sym : matchToken) {
    const auto& tokS = sym.first;
    if(content.substr(0, tokS.size()) == tokS) {
      return {sym.second, tokS.size()};
    }
  }
  if(Offset length = consumeWhiteSpace(content)) {
    return {TokenType::WhiteSpace, length};
  }
  if(Offset length = matchesFrom(numberChar, content)) {
    return {TokenType::NumberLiteral, length};
  }
  if(Offset length = matchesFrom(symbolChar, content)) {
    return {TokenType::Symbol, length};
  }
  if(Offset length = matchesFrom(operatorChar, content)) {
    return {TokenType::Operator, length};
  }
  Message msg = {
    MessageType::Error,
    "Unexpected character '" + content.substr(0,1) + "'",
    {0, 0, "ha"}
  };
  msgs.push_back(msg);

  return {TokenType::Error, 1};
}

Result<Tokens> lex(const std::string& content, const std::string& filename) {
  Tokens toks;
  Messages msgs;

  Position loc = 0;
  while(loc < content.size()) {
    std::pair<TokenType, Offset> next = chooseTok(content.substr(loc), msgs);
    TokenType type = next.first;
    Offset length = next.second;
    Token tok = {type, {loc, length, filename}};
    toks.insert(toks.begin(), tok);
    loc += length;
  }
  return {toks, msgs};
}

std::vector<Tree<Token>> toAst(Tokens& toks, Messages& msgs, const TokenType close) {
  std::vector<Tree<Token>> children;
  while(toks.size()) {
    Token curr = toks.back();
    const TokenType& type = curr.type;
    const bool inString = close == quote || close == doublequote || close == backquote;
    toks.pop_back();

    // Check that this isn't the close.
    if(type == close) {
      break;
    }
    if(!inString && close_brackets.find(type) != close_brackets.end()) {
      // Unbalanced bracket
      Message msg = {
        MessageType::Error,
        "Unbalanced bracket",
        curr.loc
      };
      msgs.push_back(msg);
      curr.type = TokenType::Error;
    }

    if(!inString) {
      if(type == comma || type == white) {
        continue;
      }
    }

    std::vector<Tree<Token>> recurse;
    // Matching brackets?
    const auto match = brackets.find(type);
    if(match != brackets.end()) {
      if(!inString || type == openbrace) {
        recurse = toAst(toks, msgs, match->second);
      }
    }
    children.push_back({curr, recurse});
  }
  return children;
}

Result<Tree<Token>> ast(Result<Tokens>& toks, const std::string& content, const std::string& filename) {
  Tree<Token> root = {{TokenType::Symbol, {0, 0, filename}}, toAst(toks.value, toks.msgs, TokenType::Error)};

  return {root, toks.msgs};
}
