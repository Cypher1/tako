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

std::optional<Definition> parseDefinition(const Tree<Token>& node, Messages& msgs, const std::string& content, const std::string& filename) {
  // Todo check that root is =
  std::string op = getString(node.value.loc, content);
  if (node.value.type != +TokenType::Operator || op != "=") {
    std::cout << "Expected def got expr\n"; // TODO msg
    return {};
  }
  std::string name = "#error";
  std::vector<FuncArg> args = {};
  Location loc = {0, 0, "#errorfile"};
  // Get symbol name
  std::optional<Value> value = {};

  if (!node.children.empty()) {
    const auto& fst = node.children[0];
    if(fst.value.type == +TokenType::Symbol) {
      name = getString(fst.value.loc, content);
      // Todo check that root.child[0].child* is = definition
      int ord = 0;
      for(const auto& argTree : fst.children) {
        std::cout << name << ", arg=" << toString(argTree, content, filename) << "\n";

        std::string argStr = getString(node.value.loc, content);
        std::optional<Definition> argDef;
        if (argTree.value.type != +TokenType::Operator || argStr != "=") {
            // TODO handle name + values args (not just kw args)
          argDef = parseDefinition(argTree, msgs, content, filename);
        } else {
          argDef = parseDefinition(argTree, msgs, content, filename);
        }
        if(argDef) {
          FuncArg arg(ord++, *argDef);
          args.push_back(arg);
        } else {
          // TODO msg
        }
      }
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
  }
  // Todo check that root.child[1] is = expr
  return Definition(name, args, loc, value);
}

Module parse(const Tree<Token>& module, Messages& msgs, const std::string& content, const std::string& filename) {
  std::vector<Definition> definitions;
  for(const auto& defTree : module.children) {
    auto def = parseDefinition(defTree, msgs, content, filename);
    if(def) {
      definitions.push_back(*def);
    }
  }
  return { filename, definitions };
}
