#include <iostream>
#include <string>
#include <sstream>

#include "../util/util.h"

#include "ast.h"

std::string toString(const Location& loc, const std::string contents) {
  std::stringstream o;
  size_t line = 1+std::count(contents.begin(), contents.begin()+loc.start, '\n');
  size_t col = loc.start - contents.rfind("\n", loc.start);
  o << "line " << line << " column " << col << "->" << loc.length << ": " << contents.substr(loc.start, loc.length);
  return o.str();
}

std::string toString(const Token& tok, const std::string& contents) {
  std::stringstream o;
  o << tok.type << "@" << toString(tok.loc, contents);
  return o.str();
}

std::string toString(const Message& msg, const std::string& contents) {
  std::stringstream o;
  o << msg.type << "@" << toString(msg.loc, contents) << ": " << msg.msg;
  return o.str();
}

std::string toString(const Tree<Token>& tree, const std::string& contents, int depth) {
  std::stringstream o;
  for(int i=0; i<depth; i++) {
    o << "  ";
  }
  o << toString(tree.value, contents) << "\n";
  for(const auto& child : tree.children) {
    o << toString(child, contents, depth+1);
  }
  return o.str();
}
