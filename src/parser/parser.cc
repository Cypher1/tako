#include <iostream>
#include <vector>
#include <optional>
#include <map>
#include <string>

#include "ast.h"
#include "lex.h"
#include "parser.h"
#include "toString.h"

std::optional<Definition> parseDefinition(const Tree<Token>& node, Messages& msgs, const std::string& content, const std::string& filename);

std::optional<Value> parseValue(const Tree<Token>& node, Messages& msgs, const std::string& content, const std::string& filename) {
  std::vector<Definition> args;
  int ord = 0;
  for(const auto& child : node.children) {
    const auto arg = parseDefinition(child, msgs, content, filename);
    if(arg) {
      args.push_back(*arg);
    } else {
      // TODO Msg?
      const auto arg_value = parseValue(child, msgs, content, filename);
      const std::string name = "#"+std::to_string(ord++); // Name the anonymous arg something impossible
      args.push_back(Definition(name, child.value.loc, {}, arg_value));
    }
  }
  return Value(getString(node.value.loc, content), node.value.loc, args);
}

std::optional<Definition> parseDefinition(const Tree<Token>& node, Messages& msgs, const std::string& content, const std::string& filename) {
  // Todo check that root is =
  std::string op = getString(node.value.loc, content);
  if (node.value.type != +TokenType::Operator || op != "=") {
    return {};
  }
  std::string name = "#error";
  std::vector<Definition> args = {};
  Location loc = {0, 0, "#errorfile"};
  // Get symbol name
  std::optional<Value> value = {};

  if (!node.children.empty()) {
    const auto& fst = node.children[0];
    if(fst.value.type == +TokenType::Symbol) {
      name = getString(fst.value.loc, content);
      // Todo check that root.child[0].child* is = definition
      for(const auto& argTree : fst.children) {
        const std::string argStr = getString(argTree.value.loc, content);
        std::optional<Definition> argDef;
        if (argTree.value.type == +TokenType::Operator && argStr == "=") {
          argDef = parseDefinition(argTree, msgs, content, filename);
        } else if(argTree.value.type == +TokenType::Symbol) {
          argDef = Definition(argStr, argTree.value.loc, {}, std::nullopt);
        }

        if(argDef) {
          Definition arg(*argDef);
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
    if(node.children.size() > 1) {
      value = parseValue(node.children[1], msgs, content, filename);
    }
  }
  // Todo check that root.child[1] is = expr
  return Definition(name, loc, args, value);
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
