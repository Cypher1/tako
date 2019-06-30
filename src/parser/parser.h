#include "util.h"
#pragma once
#ifndef PARSER_H
#define PARSER_H

#include <variant>
#include <optional>
#include <vector>
#include <string>

#include "../lib/enums.h"

#include "ast.h"

struct Definition;

struct Value {
  // TODO: support non symbol/operator values.
  // e.g. numbers, strings, arrays, sets.
  std::string name;
  Location loc;
  std::vector<Value> args;

  Value() = delete;
  Value(std::string name, Location loc, std::vector<Value> args): name{name}, loc{loc}, args{args} {}
};

struct FuncArg;

struct Definition {
  std::string name;
  std::vector<FuncArg> args;
  Location loc;
  std::optional<Value> value;
  Definition() = delete;
  Definition(std::string name, std::vector<FuncArg>args, Location loc, std::optional<Value> value): name{name}, args{args}, loc{loc}, value{value} {}
};

struct FuncArg : Definition {
  int ord;
  // TODO: consider pattern matching? maybe not in func args?
  FuncArg() = delete;
  FuncArg(int ord, Definition def): ord{ord}, Definition(def.name, def.args, def.loc, def.value) {
  }
};

struct Module {
  std::string name;
  std::vector<Definition> definitions;
  Module() = delete;
  Module(std::string name, std::vector<Definition>definitions): name{name}, definitions{definitions} {}
};

Module parse(const Tree<Token>& tree, Messages& msgs, const std::string& content, const std::string& filename);

#endif // #ifndef PARSER_H
