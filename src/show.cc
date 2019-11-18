#include <algorithm>
#include <iostream>
#include <sstream>
#include <string>

#include "util.h"

#include "checker.h"
#include "lex.h"
#include "show.h"

void indent(std::stringstream &o, int depth, char dent) {
  for (int i = 0; i < depth; i++) {
    o << dent;
  }
}

std::string banner(const std::string &text, const Config &config) {
  std::stringstream o;
  const unsigned int w = config.width - text.length();
  indent(o, w / 2 - 1, '-');
  o << " " << text << " ";
  indent(o, w - w / 2 - 1, '-');
  return o.str();
}

std::string show(const std::string &s, int depth) {
  std::stringstream o;
  indent(o, depth);
  o << s;
  return o.str();
}

std::string show(const Empty &, int) { return ""; }

std::string show(const Checks &chk, int depth) {
  std::stringstream o;
  if (!chk.pre.empty()) {
    indent(o, depth);
    o << "Pre:\n";
    o << show(chk.pre, depth + 1);
  }
  if (!chk.post.empty()) {
    indent(o, depth);
    o << "Post:\n";
    o << show(chk.post, depth + 1);
  }
  return o.str();
}

std::string show(const PrimError &p, int depth) {
  std::stringstream o;
  indent(o, depth);
  o << "Error: " << p.msg;
  return o.str();
}

std::string show(const Prim &p, int depth) {
  std::stringstream o;
  indent(o, depth);
  if (std::holds_alternative<int>(p)) {
    o << std::get<int>(p);
  }
  if (std::holds_alternative<std::string>(p)) {
    o << std::get<std::string>(p);
  }
  if (std::holds_alternative<PrimError>(p)) {
    o << show(std::get<PrimError>(p));
  }
  return o.str();
}

std::string show(const Location &loc, Context &ctx, int depth) {
  size_t line = 1 + std::count(ctx.content.begin(),
                               ctx.content.begin() + loc.start, '\n');
  size_t col = loc.start - ctx.content.rfind("\n", loc.start - 1);
  std::stringstream o;
  indent(o, depth);
  o << " line " << line;
  o << " column " << col;
  return o.str();
}

std::string show(const TokenType &ty, int depth) {
  std::stringstream o;
  indent(o, depth);
  o << ty;
  return o.str();
}

std::string show(const Token &tok, Context &ctx, int depth) {
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
  if (/*show locations*/ false) {
    std::stringstream s;
    s << show(tok.loc, ctx, 0);
    indent(o, ctx.config.width - s.str().length() - o.str().length());
    o << s.str();
  }
  return o.str();
}

std::string show(const Message &msg, Context &ctx, int depth) {
  std::stringstream o;
  indent(o, depth);
  o << msg.pass << " ";
  o << msg.type << ": ";
  o << msg.msg << " ";
  o << show(msg.loc, ctx, 0);
  return o.str();
}

std::string show(const Tree<Token> &tree, Context &ctx, int depth) {
  std::stringstream o;
  o << show(tree.value, ctx, depth);
  if (!tree.children.empty()) {
    o << "\n";
    o << show(tree.children, ctx, depth + 2, "\n");
  }
  return o.str();
}
