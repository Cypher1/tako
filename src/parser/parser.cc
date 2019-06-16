#include <iostream>
#include <vector>
#include <map>
#include <string>
#include "ast.h"
#include "parser.h"
#include "toString.h"

Result<Module> parse(Result<Tree<Token>>& tree, const std::string& content, const std::string& filename) {
  Messages& msgs = tree.msgs;
  std::vector<Value> values;
  for(const auto& node : tree.value.children) {
    if(node.value.type == +TokenType::Symbol) {
      const auto& loc = node.value.loc;
      Value val = {
        content.substr(loc.start, loc.length),
        {},
        node
      };
      values.push_back(val);
      // std::cout << toString(node, content, filename, 0);
    }
  }
  return {{filename, values}, msgs};
}
