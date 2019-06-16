#include <iostream>
#include <vector>
#include <map>
#include <string>

#include "ast.h"
#include "parser.h"
#include "toString.h"

Value parseValue( std::vector<Tree<Token>>::iterator& it, const std::vector<Tree<Token>>::iterator& end, Messages& msgs, const std::string& content, const std::string& filename) {
  const auto& node = *it;
  const auto& loc = node.value.loc;
  Value val = {
    content.substr(loc.start, loc.length),
    {},
    {}
  };
  if(node.value.type == +TokenType::Symbol) {
    ++it;
    if(it != end && it->value.type == +TokenType::OpenParen) {
      val.args = it->children;
      ++it;
    }
    if(it == end || it->value.type != +TokenType::Declaration) {
      msgs.push_back({
          MessageType::Error,
          "Needed definition ?",
          it->value.type
      });
      return val;
    }
    val.scope = it->children;
    return val;
  }
  msgs.push_back({
      MessageType::Error,
      "Unexpected ?",
      it->value.type
  });
  return val;
}

Result<Module> parse(Result<Tree<Token>>& tree, const std::string& content, const std::string& filename) {
  Messages& msgs = tree.msgs;
  std::vector<Value> values;
  auto children = tree.value.children;
  for(auto it = children.begin(); it != children.end(); ++it) {
    values.push_back(
      parseValue(it, children.end(), msgs, content, filename)
    );
  }
  return {{filename, values}, msgs};
}
