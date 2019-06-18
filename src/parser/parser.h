#pragma once
#ifndef PARSER_H
#define PARSER_H

#include <vector>
#include <string>

#include "../lib/enums.h"

#include "ast.h"

struct FuncArg {
  std::string name;
  int ord;
  std::vector<Tree<Token>> def;
  // TODO: consider pattern matching? maybe not in func args?
};

struct Value {
  std::string name;
  std::vector<FuncArg> args;
  std::vector<Tree<Token>> scope;
};

struct Module {
  std::string file;
  std::vector<Value> values;
};

Result<Module> parse(Result<Tree<Token>>& tree, const std::string& content, const std::string& filename);

#endif // #ifndef PARSER_H
