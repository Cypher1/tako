#pragma once
#ifndef TOSTRING_H
#define TOSTRING_H

#include <string>
#include <sstream>

#include "ast.h"
#include "parser.h"

extern int width;
extern int height;

void indent(std::stringstream& o, int depth);

std::string getString(const Location& loc, const std::string& contents);

template<typename T>
std::string toString(const std::vector<T>& vec, const std::string& contents, const std::string& filename, int depth=0, std::string sep=", ") {
  std::stringstream o;
  bool first = true;
  for(const auto& val : vec) {
    if(first) {
      first = false;
    } else {
      o << sep;
    }
    o << toString(val, contents, filename, depth);
  }
  return o.str();
}

std::string toString(const Location& loc, const std::string& contents, const std::string& filename, int depth=0);
std::string toString(const Value& val, const std::string& contents, const std::string& filename, int depth=0);
std::string toString(const Definition& val, const std::string& contents, const std::string& filename, int depth=0);
std::string toString(const Token& tok, const std::string& contents, const std::string& filename, int depth=0);
std::string toString(const Message& msg, const std::string& contents, const std::string& filename, int depth=0);
std::string toString(const Tree<Token>& tree, const std::string& contents, const std::string& filename, int depth=0);
std::string toString(const Module& module, const std::string& contents, const std::string& filename, int depth=0);

#endif // #ifndef TOSTRING_H
