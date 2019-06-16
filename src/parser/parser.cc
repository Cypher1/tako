#include <iostream>
#include <vector>
#include <map>
#include <string>
#include <optional>

#include "ast.h"
#include "parser.h"
#include "toString.h"

std::optional<Value> parseValue(
std::vector<Tree<Token>>::iterator& it, Messages& msgs, const std::string& content, const std::string& filename) {
  const auto& node = *it;
  if(node.value.type == +TokenType::Symbol) {
    const auto& loc = node.value.loc;
    Value val = {
      content.substr(loc.start, loc.length),
      {},
      node
    };
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
  for(auto it = tree.value.children.begin(); it != tree.value.children.end(); ++it) {
    std::optional<Value> val = parseValue(it, msgs, content, filename);
    if(val) {
      values.push_back(*val);
    }
  }
  return {{filename, values}, msgs};
}
