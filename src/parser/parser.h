#include "util.h"
#pragma once
#ifndef PARSER_H
#define PARSER_H

#include <vector>
#include <string>

#include "../lib/enums.h"

#include "ast.h"

struct Value;

struct FuncArg {
  std::string name = "A???";
  int ord = 0;
  std::vector<Value> def;
  // TODO: consider pattern matching? maybe not in func args?
};

struct Value {
  std::string name = "V???";
  std::vector<FuncArg> args;
  std::vector<Value> scope;
  Location loc;
};

struct Module {
  std::string file = "M???";
  std::vector<Value> values;
};

Module parse(Tree<Token>& tree, Messages& msgs, const std::string& content, const std::string& filename);

#endif // #ifndef PARSER_H
