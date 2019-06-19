#include <iostream>
#include <string>
#include <sstream>
#include <algorithm>

#include "../util/util.h"

#include "ast.h"
#include "toString.h"

void indent(std::stringstream& o, int depth) {
  for(int i=0; i<depth; i++) {
    o << " ";
  }
}

std::string getString(const Location& loc, const std::string& contents) {
  size_t line = 1+std::count(contents.begin(), contents.begin()+loc.start, '\n');
  size_t col = loc.start - contents.rfind("\n", loc.start);
  return contents.substr(loc.start, loc.length);
}

std::string toString(const Location& loc, const std::string& contents, const std::string& filename, int depth) {
  size_t line = 1+std::count(contents.begin(), contents.begin()+loc.start, '\n');
  size_t col = loc.start - contents.rfind("\n", loc.start);
  std::stringstream o;
  indent(o, depth);
  // o << " in " << filename;
  o << " line " << line;
  o << " column " << col;
  return o.str();
}

std::string toString(const Value& val, const std::string& contents, const std::string& filename, int depth) {
  std::stringstream o;
  indent(o, depth);
  o << val.name << "(" << toString(val.args, contents, filename) << ")";
  if (val.scope.size()) {
    o << "= " << toString(val.scope, contents, filename, 0);
  }
  return o.str();
}

std::string toString(const FuncArg& arg, const std::string& contents, const std::string& filename, int depth) {
  std::stringstream o;
  indent(o, depth);
  o << arg.name << "[" << arg.ord << "]";
  if (arg.def.size()) {
    o << "= " << toString(arg.def, contents, filename, 0);
  }
  return o.str();
}

std::string toString(const Token& tok, const std::string& contents, const std::string& filename, int depth) {
  std::stringstream o;
  indent(o, depth);
  o << getString(tok.loc, contents) << " : " << tok.type;
  std::stringstream s;
  s << toString(tok.loc, contents, filename, 0);
  indent(o, 80-s.str().length()-o.str().length());
  o << s.str();
  return o.str();
}

std::string toString(const Message& msg, const std::string& contents, const std::string& filename, int depth) {
  std::stringstream o;
  indent(o, depth);
  o << msg.type << ": ";
  o << msg.msg << " ";
  o << toString(msg.loc, contents, filename, 0) << "\n";
  return o.str();
}

std::string toString(const Tree<Token>& tree, const std::string& contents, const std::string& filename, int depth) {
  std::stringstream o;
  o << toString(tree.value, contents, filename, depth) << "\n";
  o << toString(tree.children, contents, filename, depth+2);
  return o.str();
}
