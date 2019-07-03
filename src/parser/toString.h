#pragma once
#ifndef TOSTRING_H
#define TOSTRING_H

#include <string>
#include <sstream>

#include "../util/context.h"

#include "lex.h"
#include "parser.h"

extern int width;
extern int height;

void indent(std::stringstream& o, int depth);

template<typename T>
std::string toString(const std::vector<T>& vec, const Context &ctx, int depth=0, std::string sep=", ") {
  std::stringstream o;
  bool first = true;
  for(const auto& val : vec) {
    if(first) {
      first = false;
    } else {
      o << sep;
    }
    o << toString(val, ctx, depth);
  }
  return o.str();
}

std::string toString(const Location& loc, const Context &ctx, int depth=0);
std::string toString(const Value& val, const Context &ctx, int depth=0);
std::string toString(const Definition& val, const Context &ctx, int depth=0);
std::string toString(const Token& tok, const Context &ctx, int depth=0);
std::string toString(const Message& msg, const Context &ctx, int depth=0);
std::string toString(const Tree<Token>& tree, const Context &ctx, int depth=0);
std::string toString(const Module& module, const Context &ctx, int depth=0);

#endif // #ifndef TOSTRING_H
