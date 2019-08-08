#include <iostream>
#include <string>
#include <sstream>
#include <algorithm>

#include "util.h"

#include "lex.h"
#include "show.h"
#include "checker.h"

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

std::string show(const Empty&, int depth) {
  return "";
}

std::string show(const Checks &chk, int depth) {
  std::stringstream o;
  if(!chk.pre.empty()) {
    indent(o, depth);
    o << "Pre:\n";
    o << show(chk.pre, depth+1);
  }
  if(!chk.post.empty()) {
    indent(o, depth);
    o << "Post:\n";
    o << show(chk.post, depth+1);
  }
  return "";
}

std::string show(const Location& loc, Context &ctx, int depth) {
  size_t line = 1+std::count(ctx.content.begin(), ctx.content.begin()+loc.start, '\n');
  size_t col = loc.start - ctx.content.rfind("\n", loc.start-1);
  std::stringstream o;
  indent(o, depth);
  o << " line " << line;
  o << " column " << col;
  return o.str();
}

std::string show(const Token& tok, Context &ctx, int depth) {
  std::stringstream o;
  indent(o, depth);
  if (tok.type == +TokenType::WhiteSpace) {
    o << "'";
  }
  o << ctx.getStringAt(tok.loc);
  if (tok.type == +TokenType::WhiteSpace) {
    o << "'";
  }
  o << "(" << tok.type << ")";
  if(/*show locations*/ false) {
    std::stringstream s;
    s << show(tok.loc, ctx, 0);
    indent(o, ctx.config.width-s.str().length()-o.str().length());
    o << s.str();
  }
  return o.str();
}

std::string show(const Message& msg, Context &ctx, int depth) {
  std::stringstream o;
  indent(o, depth);
  o << msg.pass << " ";
  o << msg.type << ": ";
  o << msg.msg << " ";
  o << show(msg.loc, ctx, 0);
  return o.str();
}

std::string show(const Tree<Token>& tree, Context &ctx, int depth) {
  std::stringstream o;
  o << show(tree.value, ctx, depth);
  if(!tree.children.empty()) {
    o << "\n";
    o << show(tree.children, ctx, depth+2, "\n");
  }
  return o.str();
}
