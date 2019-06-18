#pragma once
#ifndef TOSTRING_H
#define TOSTRING_H

#include <string>
#include <sstream>

#include "ast.h"
#include "parser.h"

void indent(std::stringstream& o, int depth);

template<typename T>
std::string toString(const std::vector<T>& vec, const std::string& contents, const std::string& filename, int depth=0) {
  std::stringstream o;
  for(const auto& item : vec) {
    o << toString(item, contents, filename, depth);
  }
  return o.str();
}

std::string toString(const Location& loc, const std::string& contents, const std::string& filename, int depth=0);
std::string toString(const FuncArg& arg, const std::string& contents, const std::string& filename, int depth=0);
std::string toString(const Token& tok, const std::string& contents, const std::string& filename, int depth=0);
std::string toString(const Message& msg, const std::string& contents, const std::string& filename, int depth=0);
std::string toString(const Tree<Token>& tree, const std::string& contents, const std::string& filename, int depth=0);

#endif // #ifndef TOSTRING_H
