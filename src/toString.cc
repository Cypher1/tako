#include <iostream>
#include <string>
#include <sstream>
#include <algorithm>

#include "util.h"

#include "lex.h"
#include "toString.h"

void indent(std::stringstream& o, int depth, char dent) {
  for(int i=0; i<depth; i++) {
    o << dent;
  }
}

std::string banner(const std::string &text, const Config &config) {
  std::stringstream o;
  const unsigned int w = config.width-text.length();
  indent(o, w/2-1, '-');
  o << " " << text << " ";
  indent(o, w-w/2-1, '-');
  return o.str();
}
std::string toString(const Location& loc, const Context &ctx, int depth) {
  size_t line = 1+std::count(ctx.content.begin(), ctx.content.begin()+loc.start, '\n');
  size_t col = loc.start - ctx.content.rfind("\n", loc.start);
  std::stringstream o;
  indent(o, depth);
  o << " line " << line;
  o << " column " << col;
  return o.str();
}

std::string toString(const Value& val, int depth) {
  std::stringstream o;
  indent(o, depth);
  o << val.name;
  if (!val.args.empty()) {
    o << "(\n";
    for(const auto& arg : val.args) {
      o << toString(arg, depth+2) << "\n";
    }
    indent(o, depth);
    o << ")";
  }
  return o.str();
}

std::string toString(const Definition& val, int depth) {
  std::stringstream o;
  o << toString(Value(val), depth);
  if (val.value) {
    o << " =\n";
    o << toString(*val.value, depth+2);
  }
  return o.str();
}

std::string toString(const Module& module, int depth) {
  std::stringstream o;
  indent(o, depth);
  o << "module " << module.name << " (" << module.definitions.size() << " top level definitions) {\n";
  for(const auto& val : module.definitions) {
    o << toString(val, depth+2) << "\n";
  }
  indent(o, depth);
  o << "}";
  return o.str();
}

std::string toString(const Token& tok, const Context &ctx, int depth) {
  std::stringstream o;
  indent(o, depth);
  if (tok.type == +TokenType::WhiteSpace) {
    o << "'";
  }
  o << ctx.getStringAt(tok.loc);
  if (tok.type == +TokenType::WhiteSpace) {
    o << "'";
  }
  o << " : " << tok.type;
  if(/*show locations*/ false) {
    std::stringstream s;
    s << toString(tok.loc, ctx, 0);
    indent(o, ctx.config.width-s.str().length()-o.str().length());
    o << s.str();
  }
  return o.str();
}

std::string toString(const Message& msg, const Context &ctx, int depth) {
  std::stringstream o;
  indent(o, depth);
  o << msg.pass << " ";
  o << msg.type << ": ";
  o << msg.msg << " ";
  o << toString(msg.loc, ctx, 0);
  return o.str();
}

std::string toString(const Tree<Token>& tree, const Context &ctx, int depth) {
  std::stringstream o;
  o << toString(tree.value, ctx, depth);
  o << toString(tree.children, ctx, depth+2, "\n");
  return o.str();
}
