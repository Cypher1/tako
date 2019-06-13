#include <vector>
#include <string>
#include "ast.h"

Result<Tokens> lex(std::string filename, std::string content) {
  Tokens toks;
  std::vector<Message> errs;

  Token example;
  example.type = TokenType::Symbol;
  example.loc.start = 5;
  example.loc.length = 4;
  example.loc.file = filename;
  toks.push_back(example);

  Message er;
  er.type = MessageType::Info;
  er.msg = "Test info";
  er.loc = example.loc;
  errs.push_back(er);

  return {toks, errs};
}
