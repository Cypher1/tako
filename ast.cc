#include <iostream>
#include <vector>
#include <string>
#include "ast.h"

std::pair<TokenType, Offset> chooseTok(std::string content) {
  // TODO: use string views.
  Offset length = 4;
  TokenType type = TokenType::Symbol;
  return std::make_pair(type, length);
}

void consumeWhiteSpace(Position& loc, const std::string content) {
  for(;loc < content.size(); loc++) {
    char cur = content[loc];
    if(std::string(" \t\n\r").find(cur) != std::string::npos) {
      continue;
    }
    if(cur == '/' && loc+1 < content.size() && content[loc+1] == '/') {
      loc++;
      for(;loc < content.size() && content[loc] != '\n'; loc++);
    }
    break;
  }
}

Result<Tokens> lex(std::string content, std::string filename) {
  Tokens toks;
  Messages msgs;

  Position loc = 0;
  consumeWhiteSpace(loc, content);
  while(loc < content.size()) {
    std::pair<TokenType, Offset> next = chooseTok(content.substr(loc));
    TokenType type = next.first;
    Offset length = next.second;
    Token tok = {type, {loc, length, filename}};
    toks.push_back(tok);
    loc += length;
    consumeWhiteSpace(loc, content);
  }
  Message msg = {
    MessageType::Info,
    "Test info",
    {0, 0, "ha"}
  };
  msgs.push_back(msg);

  return {toks, msgs};
}
