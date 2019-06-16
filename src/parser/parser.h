#pragma once
#ifndef PARSER_H
#define PARSER_H

#include <vector>
#include <string>

#include "../lib/enums.h"

#include "ast.h"

struct Module {
  std::string file;
  std::vector<std::string> names;
};

Result<Module> parse(Result<Tree<Token>>& tree, const std::string& content, const std::string& filename);

#endif // #ifndef PARSER_H
