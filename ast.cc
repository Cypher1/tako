#include <vector>
#include <string>
#include "ast.h"

Result<Tokens> lex(std::string filename, std::string content) {
  Tokens toks;
  std::vector<Message> errs;

  Token example = {
    TokenType::Symbol,
    {5, 4, filename}
  };
  toks.push_back(example);

  Message er = {
    MessageType::Info,
    "Test info",
    example.loc
  };
  errs.push_back(er);

  return {toks, errs};
}
