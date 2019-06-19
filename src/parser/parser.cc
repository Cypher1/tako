#include <iostream>
#include <vector>
#include <map>
#include <string>

#include "ast.h"
#include "parser.h"
#include "toString.h"

Value parseDefinition( std::vector<Tree<Token>>::iterator& it, const std::vector<Tree<Token>>::iterator& end, Messages& msgs, const std::string& content, const std::string& filename);
std::vector<Value> parseDefinitions(std::vector<Tree<Token>> nodes, Messages& msgs, const std::string& content, const std::string& filename);

FuncArg parseArg(Tree<Token> arg, const int ord, Messages& msgs, const std::string& content, const std::string& filename) {
  std::vector<Value> def;
  std::string name;
  auto it = arg.children.begin();
  if (arg.value.type == +TokenType::Declaration && it != arg.children.end()) {
    Value val = parseDefinition(it, arg.children.end(), msgs, content, filename);
    name = val.name; //Args?
    def = val.scope;
  } else if (arg.value.type == +TokenType::Symbol) {
    name = content.substr(arg.value.loc.start, arg.value.loc.length);
  } else {
    // Just a value
    def = parseDefinitions({arg}, msgs, content, filename);
  }
  return {name, ord, def};
}

Value parseDefinition( std::vector<Tree<Token>>::iterator& it, const std::vector<Tree<Token>>::iterator& end, Messages& msgs, const std::string& content, const std::string& filename) {
  Value val = {
    "error", // TODO msg
    {},
    {}
  };
  if(it == end) {
    return val;
  }
  const auto node = *it;
  const auto& loc = node.value.loc;
  if(node.value.type == +TokenType::Symbol) {
    val.name = content.substr(loc.start, loc.length);
    ++it;
    if(it != end && it->value.type == +TokenType::OpenParen) {
      int n = 0;
      for(const auto& arg : it->children) {
        val.args.push_back(parseArg(arg, n++, msgs, content, filename));
      }
      ++it;
    }
    if(it == end || it->value.type != +TokenType::Declaration) {
      msgs.push_back({
          MessageType::Error,
          "Needed definition ?",
          node.value.loc
      });
      return val;
    }
    val.scope = parseDefinitions(it->children, msgs, content, filename);
    return val;
  }
  msgs.push_back({
      MessageType::Error,
      "Unexpected ?",
      node.value.loc
  });
  return val;
}

std::vector<Value> parseDefinitions(std::vector<Tree<Token>> nodes, Messages& msgs, const std::string& content, const std::string& filename) {
  std::vector<Value> values;
  for(auto it = nodes.begin(); it != nodes.end(); ++it) {
    values.push_back(
      parseDefinition(it, nodes.end(), msgs, content, filename)
    );
  }
  return values;
}

Result<Module> parse(Result<Tree<Token>>& tree, const std::string& content, const std::string& filename) {
  Messages& msgs = tree.msgs;
  auto children = tree.value.children;
  std::vector<Value> values = parseDefinitions(children, msgs, content, filename);
  return {{filename, values}, msgs};
}
