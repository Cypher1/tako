#include <iostream>
#include <vector>
#include <map>
#include <string>

#include "ast.h"
#include "parser.h"
#include "toString.h"

Value parseDefinition( std::vector<Tree<Token>>::const_iterator& it, const std::vector<Tree<Token>>::const_iterator& end, Messages& msgs, const std::string& content, const std::string& filename);
std::vector<Value> parseDefinitions(std::vector<Tree<Token>>::const_iterator& it, const std::vector<Tree<Token>>::const_iterator& end, Messages& msgs, const std::string& content, const std::string& filename);

FuncArg parseArg(Tree<Token> arg, const int ord, Messages& msgs, const std::string& content, const std::string& filename) {
  std::vector<Value> def;
  std::string name;
  auto it = arg.children.cbegin();
  if (arg.value.type == +TokenType::Declaration && it != arg.children.end()) {
    Value val = parseDefinition(it, arg.children.end(), msgs, content, filename);
    name = val.name; //Args?
    def = val.scope;
  } else if (arg.value.type == +TokenType::Symbol) {
    name = content.substr(arg.value.loc.start, arg.value.loc.length);
  } else {
    // Just a value
    std::vector<Tree<Token>> argV = {arg};
    auto it_it = argV.cbegin();
    def = parseDefinitions(it_it, argV.cend(), msgs, content, filename);
  }
  return {name, ord, def};
}

Value parseDefinition( std::vector<Tree<Token>>::const_iterator& it, const std::vector<Tree<Token>>::const_iterator& end, Messages& msgs, const std::string& content, const std::string& filename) {
  Value val = { "#error", {}, {}, {0, 0, "??l"} };
  if(it == end) {
    return val;
  }
  auto loc = it->value.loc;
  if (it->value.type == +TokenType::Symbol) {
    val.name = content.substr(loc.start, loc.length);
  } else {
    msgs.push_back({
        MessageType::Error,
        "Unexpected ?",
        loc
    });
    return val;
  }
  ++it;
  if(it != end && it->value.type == +TokenType::OpenParen) {
    int n = 0;
    for(const auto& arg : it->children) {
      val.args.push_back(parseArg(arg, n++, msgs, content, filename));
    }
    if(val.args.empty()) {
      msgs.push_back({
          MessageType::Info,
          "No need for the parentheses '()' here.",
          val.loc
      });
    }
    ++it;
  }
  if(it == end) {
    msgs.push_back({
        MessageType::Error,
        "Reached end of scope, expected a definition for '?'",
        val.loc
    });
    return val;
  }
  if(it->value.type == +TokenType::Declaration) {
    msgs.push_back({
        MessageType::Error,
        "Reached end of scope, expected a definition for '?'",
        val.loc
    });
    return val;
  }

  const auto childers = it->children;
  auto it_it = childers.cbegin();
  val.scope = parseDefinitions(it_it, childers.cend(), msgs, content, filename);

  return val;
}

std::vector<Value> parseDefinitions(std::vector<Tree<Token>>::const_iterator& it, const std::vector<Tree<Token>>::const_iterator& end, Messages& msgs, const std::string& content, const std::string& filename) {
  std::vector<Value> values;
  while(it != end) {
      values.push_back(parseDefinition(it, end, msgs, content, filename));
    if(it == end) break;
    ++it;
  }
  return values;
}

Module parse(Tree<Token>& tree, Messages& msgs, const std::string& content, const std::string& filename) {
  auto children = tree.children;
  auto it = children.cbegin();
  std::vector<Value> values = parseDefinitions(it, children.cend(), msgs, content, filename);
  return {filename, values};
}
