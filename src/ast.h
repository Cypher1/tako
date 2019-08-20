#pragma once
#ifndef AST_H
#define AST_H

#include <variant>
#include <optional>
#include <vector>
#include <string>
#include <functional>

#include "enums.h"
#include "util.h"
#include "context.h"
#include "lex.h"

enum class AstNodeType {
  Symbol,
  Numeric,
  Text
}; // TODO: arrays, sets, dictionaries etc.

template<class T>
class DefinitionCore;

template<class T>
class AstNode {
  public:
  std::string name;
  Location loc;
  T info;

  AstNode(std::string name, Location loc): name{name}, loc{loc} {}

  bool operator<(const AstNode &other) const {
    return loc < other.loc; // Use the loc which is unique.
  }
};

template<class T>
class ValueCore : public AstNode<T> {
  public:
  // Should this be an ast node property instead?
  std::vector<DefinitionCore<T>> args;
  AstNodeType node_type;

  ValueCore() = delete;
  ValueCore(std::string name, Location loc, std::vector<DefinitionCore<T>> args, AstNodeType node_type):
    AstNode<T>(name, loc),
    args{args},
    node_type{node_type} {}

  bool operator ==(const ValueCore<T>& other) const {
    if (this->name != other.name) return false;
    if (args.size() != other.args.size()) return false;
    auto it = args.begin();
    auto o_it = other.args.begin();
    while (it != args.end()) {
      if(*it != *o_it) return false;
      it++;
      o_it++;
    }
    return true;
  }
  bool operator !=(const ValueCore<T>& other) const {
    return !(*this == other);
  }

};

template<class T>
class DefinitionCore : public ValueCore<T> {
  public:
  std::optional<ValueCore<T>> value;
  DefinitionCore() = delete;
  DefinitionCore(const std::string name, Location loc, std::vector<DefinitionCore<T>>args, std::optional<ValueCore<T>> value):
    ValueCore<T>(name, loc, args, AstNodeType::Symbol),
    value{value} {}
};

template<class T>
class ModuleCore : public AstNode<T>{
  public:
  std::vector<DefinitionCore<T>> definitions;
  ModuleCore() = delete;
  ModuleCore(std::string name, Location loc, std::vector<DefinitionCore<T>>definitions): AstNode<T>(name, loc), definitions{definitions} {}
};

class Empty { };

using Value = ValueCore<Empty>;
using Definition = DefinitionCore<Empty>;
using Module = ModuleCore<Empty>;

class AstContext;

namespace ast {

using Parser = std::function<Tree<Token>(AstContext&, unsigned int)>;
Tree<Token> parseDefinition(AstContext &ctx, unsigned int rbp=0);
Tree<Token> parseValue(AstContext &ctx, unsigned int rbp=0);
Tree<Token> parseModule(AstContext &ctx, unsigned int rbp=0);

std::optional<Tree<Token>> ast(Tokens& toks, Context &ctx, std::function<Tree<Token>(AstContext &, unsigned int)> func);

}

#endif // #ifndef AST_H
