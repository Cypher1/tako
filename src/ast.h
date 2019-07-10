#pragma once
#ifndef AST_H
#define AST_H

#include <variant>
#include <optional>
#include <vector>
#include <string>

#include "enums.h"
#include "util.h"
#include "context.h"
#include "lex.h"

struct Definition;

struct Value {
  // TODO: support non symbol/operator values.
  // e.g. numbers, strings, arrays, sets.
  std::string name;
  Location loc;
  std::vector<Definition> args;

  Value() = delete;
  Value(std::string name, Location loc, std::vector<Definition> args): name{name}, loc{loc}, args{args} {}

  bool operator ==(const Value& other) const;
};

struct Definition : Value {
  std::optional<Value> value;
  Definition() = delete;
  Definition(const std::string name, Location loc, std::vector<Definition>args, std::optional<Value> value): Value(name, loc, args), value{value} {}
};

struct Module {
  std::string name;
  std::vector<Definition> definitions;
  Module() = delete;
  Module(std::string name, std::vector<Definition>definitions): name{name}, definitions{definitions} {}
};

Tree<Token> ast(Tokens& toks, Context &ctx);

#endif // #ifndef AST_H
