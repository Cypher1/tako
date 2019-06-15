#include <iostream>
#include <vector>
#include <map>
#include <string>
#include "ast.h"
#include "parser.h"

Result<Module> parse(Result<Tree<Token>>& tree, const std::string& content, const std::string& filename) {
  Messages& msgs = tree.msgs;
  return {{{"foo", "bar"}}, msgs};
}
