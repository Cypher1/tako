#include <iostream>
#include <vector>
#include <map>
#include <string>
#include <optional>

#include "ast.h"
#include "parser.h"
#include "toString.h"

std::optional<Value> parseValue( std::vector<Tree<Token>>::iterator& it, const std::vector<Tree<Token>>::iterator& end, Messages& msgs, const std::string& content, const std::string& filename) {
  const auto& node = *it;
  if(node.value.type == +TokenType::Symbol) {
    const auto& loc = node.value.loc;
    Value val = {
      content.substr(loc.start, loc.length),
      {},
      node
    };
    ++it;
    if(it != end && it->value.type == +TokenType::OpenParen) {
      val.args = it->children;
      ++it;
    }
    if(it == end) {
      //TODO: needed definition
      return std::nullopt;
    }
    val.def = *it;
    ++it;
    return val;
  }
  //TODO: Unparseable.
  msgs.push_back({
      MessageType::Error,
      "Unexpected ?",
      it->value.type
  });
  return std::nullopt;
}

Result<Module> parse(Result<Tree<Token>>& tree, const std::string& content, const std::string& filename) {
  Messages& msgs = tree.msgs;
  std::vector<Value> values;
  auto children = tree.value.children;
  for(auto it = children.begin(); it != children.end(); ++it) {
    std::optional<Value> val = parseValue(it, children.end(), msgs, content, filename);
    if(val) {
      values.push_back(*val);
    }
  }
  return {{filename, values}, msgs};
}
