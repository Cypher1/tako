#include <iostream>
#include <vector>
#include <optional>
#include <map>
#include <string>

#include "ast.h"
#include "parser.h"
#include "toString.h"

/*
struct Value {
  // TODO: support non symbol/operator values.
  // e.g. numbers, strings, arrays, sets.
  std::string name;
  Location loc;
  std::vector<Value> args;

  Value() = delete;
  Value(std::string name, Location loc, std::vector<Value> args): name{name}, loc{loc}, args{args} {}
};

struct FuncArg {
  std::string name;
  int ord;
  std::optional<Value> def; // Default value for the arg.
  // TODO: consider pattern matching? maybe not in func args?
  FuncArg() = delete;
  FuncArg(std::string name, int ord): name{name}, ord{ord}, def{std::nullopt} {}
  FuncArg(std::string name, int ord, Value def): name{name}, ord{ord}, def{def} {}
};

*/

Definition parseDefinition(const Tree<Token>& node, Messages& msgs, const std::string& content, const std::string& filename) {
  // Todo check that root is =
  std::string op = getString(node.value.loc, content);
  if (node.value.type == +TokenType::Operator && op == "=") {
    // 
    std::cout << "Got def\n";
  } else {
    std::cout << "Expected def got expr\n";
  }
  // Todo check that root.child[0] is = symbol
  // Get symbol name
  std::string name = "#error";
  std::vector<FuncArg> args = {};
  Location loc = {0, 0, "#errorfile"};
  // Todo check that root.child[1] is = expr
  std::optional<Value> value = {};
  if (!node.children.empty() && node.children[0].value.type == +TokenType::Operator) {
    name = getString(node.children[0].value.loc, content);
    // Todo check that root.child[0].child* is = definition
    args = {};
    loc = {0, 0, "#errorfile"};
    // Todo check that root.child[1] is = expr
    value = {};
    /*
    msgs.push_back({
        PassStep::Parse,
        MessageType::Error,
        "Reached end of scope, expected end of definition for '"+val.name+"', got '"+toString(node.value, content, filename)+"' instead.",
        val.loc
    });
    */
  }
  return Definition(name, args, loc, value);
}

Module parse(const Tree<Token>& module, Messages& msgs, const std::string& content, const std::string& filename) {
  std::vector<Definition> definitions;
  for(const auto& def : module.children) {
    definitions.push_back(parseDefinition(def, msgs, content, filename));
  }
  return { filename, definitions };
}
