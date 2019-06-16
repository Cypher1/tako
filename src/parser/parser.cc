#include <iostream>
#include <vector>
#include <map>
#include <string>
#include "ast.h"
#include "parser.h"
#include "toString.h"

Result<Module> parse(Result<Tree<Token>>& tree, const std::string& content, const std::string& filename) {
  Messages& msgs = tree.msgs;
  std::vector<std::string> values;
  Value val;
  for(const auto& node : tree.value.children) {
    if(node.value.type == +TokenType::Symbol) {
      const auto& loc = node.value.loc;
      val.name = content.substr(loc.start, loc.length);
      // std::cout << toString(node, content, filename, 0);
      values.push_back(val);
    }
  }
  return {{filename, values}, msgs};
}
