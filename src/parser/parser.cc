#include <iostream>
#include <vector>
#include <map>
#include <string>
#include "ast.h"
#include "parser.h"
#include "toString.h"

Result<Module> parse(Result<Tree<Token>>& tree, const std::string& content, const std::string& filename) {
  Messages& msgs = tree.msgs;
  std::vector<std::string> names;
  for(const auto node : tree.value.children) {
    std::cout << toString(node, content, filename, 0);
    // names.push_back(name);
  }
  return {{filename, names}, msgs};
}
