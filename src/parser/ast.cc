#include <iostream>
#include <vector>
#include <map>
#include <set>
#include <string>
#include "ast.h"

const std::string lower = "abcdefghijklmnopqrstuvwxyz";
const std::string upper = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
const std::string nums = "0123456789";

const std::string whiteSpace = " \t\n\r";
const std::string numberChar = "."+nums;
const std::string operatorChar = "-+&#@<>^~∆%•|=÷×°$\\/*:?!.";
const std::string symbolChar = lower+upper+nums+"_";

const std::vector<std::pair<std::string, TokenType>> matchToken = {
  {"(", TokenType::OpenParen},
  {")", TokenType::CloseParen},
  {"{", TokenType::OpenBrace},
  {"}", TokenType::CloseBrace},
  {"[", TokenType::OpenBracket},
  {"]", TokenType::CloseBracket},
  {"=", TokenType::Declaration},
  {";", TokenType::SemiColon},
  {"'", TokenType::SingleQuote},
  {"\"", TokenType::DoubleQuote},
  {"`", TokenType::BackQuote},
  {"-|", TokenType::PreCond},
  {"|-", TokenType::PostCond},
  {".", TokenType::Dot},
  {",", TokenType::Comma}
};

const std::map<TokenType, TokenType> brackets = {
  {TokenType::OpenParen, TokenType::CloseParen},
  {TokenType::OpenBrace, TokenType::CloseBrace},
  {TokenType::OpenBracket, TokenType::CloseBracket},
  {TokenType::SingleQuote, TokenType::SingleQuote},
  {TokenType::DoubleQuote, TokenType::DoubleQuote},
  {TokenType::BackQuote, TokenType::BackQuote},
  {TokenType::Declaration, TokenType::SemiColon},
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

bool isQuote(const TokenType& type) {
  return type == +TokenType::SingleQuote || type == +TokenType::DoubleQuote || type == +TokenType::BackQuote;
}

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

std::pair<TokenType, Offset> chooseTok(std::string content) {
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
  return {TokenType::Error, 1};
}

Result<Tokens> lex(const std::string& content, const std::string& filename) {
  Tokens toks;
  Messages msgs;

  Position loc = 0;
  while(loc < content.size()) {
    std::pair<TokenType, Offset> next = chooseTok(content.substr(loc));
    TokenType type = next.first;
    Offset length = next.second;
    if(type == +TokenType::Error) {
      msgs.push_back({
        MessageType::Error,
        "Unexpected character",
        {loc, length, filename}
      });
    }
    Token tok = {type, {loc, length, filename}};
    toks.insert(toks.begin(), tok);
    loc += length;
  }
  return {toks, msgs};
}


const std::vector<std::set<std::string>> precedence = {
  {"+", "-"},
  {"*", "/"}
};

std::vector<Tree<Token>> toExpression(std::vector<Tree<Token>> nodes, Messages& msgs, const std::string& content) {
  if(nodes.empty()) {
    return nodes;
  }
  for(int level = 0; level < precedence.size(); ++level) {
    auto it = nodes.begin();
      while(it != nodes.end()) {
        if(it->value.type == +TokenType::Operator) {
          // If we're at the right precedence level, this is out next splitter.
          std::string s = content.substr(it->value.loc.start, it->value.loc.length);
          if(precedence[level].find(s) != precedence[level].end()) {
            break;
          }
        }
        ++it;
      }

    if(it == nodes.end()) {
      continue;
    }

    // Get the left and right and make them into nodes of this op.
    Tree<Token> curr = *it;
    std::vector<Tree<Token>> left = toExpression({nodes.begin(), it}, msgs, content);
    if(left.size() > 1) {
      curr.children.push_back({{TokenType::OpenParen, curr.value.loc}, left});
    } else if (!left.empty()) {
      curr.children.push_back(left[0]);
    }

    std::vector<Tree<Token>> right = toExpression({it+1, nodes.end()}, msgs, content);
    if(right.size() > 1) {
      curr.children.push_back({{TokenType::OpenParen, curr.value.loc}, right});
    } else if (!right.empty()) {
      curr.children.push_back(right[0]);
    }

    return {curr};
  }

  return nodes;
}

std::vector<Tree<Token>> toAst(Tokens& toks, Messages& msgs, const TokenType close, const std::string& content) {
  std::vector<Tree<Token>> children;
  while(toks.size()) {
    Token curr = toks.back();
    const TokenType& type = curr.type;
    const bool inString = isQuote(close);
    toks.pop_back();

    // Check that this isn't the close.
    if(type == close) {
      break;
    }
    if(!inString && close_brackets.find(type) != close_brackets.end()) {
      // Unbalanced bracket
      msgs.push_back({
        MessageType::Error,
        "Unbalanced bracket",
        curr.loc
      });
      curr.type = TokenType::Error;
    }

    if(!inString) {
      if(type == +TokenType::Comma || type == +TokenType::WhiteSpace) {
        continue;
      }
    }

    std::vector<Tree<Token>> recurse;
    // Matching brackets?
    const auto match = brackets.find(type);
    if(match != brackets.end()) {
      if(!inString || type == +TokenType::OpenBrace) {
        recurse = toAst(toks, msgs, match->second, content);
      }
    }
    children.push_back({curr, toExpression(recurse, msgs, content)});
  }
  return children;
}

Result<Tree<Token>> ast(Result<Tokens>& toks, const std::string& content, const std::string& filename) {
  Tree<Token> root = {{TokenType::Symbol, {0, 0, filename}}, toAst(toks.value, toks.msgs, TokenType::Error, content)};

  return {root, toks.msgs};
}
