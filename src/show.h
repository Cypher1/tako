#ifndef TOSTRING_H
#define TOSTRING_H

#include <string>
#include <sstream>

#include "context.h"

#include "lex.h"
#include "parser.h"

void indent(std::stringstream& o, int depth, char dent=' ');

std::string banner(const std::string &text, const Config &config);

template<typename T>
std::string show(const std::vector<T>& vec, Context &ctx, int depth=0, std::string sep=", ") {
  std::stringstream o;
  bool first = true;
  for(const auto& val : vec) {
    if(first) {
      first = false;
    } else {
      o << sep;
    }
    o << show(val, ctx, depth);
  }
  return o.str();
}

std::string show(const Value& val, int depth=0);
std::string show(const Definition& val, int depth=0);
std::string show(const Module& module, int depth=0);

std::string show(const Location& loc, Context &ctx, int depth=0);
std::string show(const Token& tok, Context &ctx, int depth=0);
std::string show(const Message& msg, Context &ctx, int depth=0);
std::string show(const Tree<Token>& tree, Context &ctx, int depth=0);

#endif // #ifndef TOSTRING_H
