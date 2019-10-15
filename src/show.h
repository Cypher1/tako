#ifndef TOSTRING_H
#define TOSTRING_H

#include <sstream>
#include <string>

#include "context.h"
#include "parser.h"

void indent(std::stringstream &o, int depth, char dent = ' ');

std::string banner(const std::string &text, const Config &config);

std::string show(const std::string &s, int depth = 0);

class Empty;
std::string show(const Empty &, int depth = 0);
class Checks;
std::string show(const Checks &, int depth = 0);

template <typename T>
std::string show(const DefinitionCore<T> &val, int depth = 0);
template <typename T>
std::string show(const ModuleCore<T> &module, int depth = 0);

std::string show(const Location &loc, Context &ctx, int depth);
std::string show(const Token &tok, Context &ctx, int depth = 0);
std::string show(const Message &msg, Context &ctx, int depth = 0);
std::string show(const Tree<Token> &tree, Context &ctx, int depth = 0);

template <typename T>
std::string show(const std::vector<T> &vec, int depth = 0,
                 std::string sep = ", ") {
  std::stringstream o;
  bool first = true;
  for (const auto &val : vec) {
    if (first) {
      first = false;
    } else {
      o << sep;
    }
    o << show(val, depth);
  }
  return o.str();
}

template <typename T>
std::string show(const std::vector<T> &vec, Context &ctx, int depth = 0,
                 std::string sep = ", ") {
  std::stringstream o;
  bool first = true;
  for (const auto &val : vec) {
    if (first) {
      first = false;
    } else {
      o << sep;
    }
    o << show(val, ctx, depth);
  }
  return o.str();
}

template <typename T> std::string show(const ValueCore<T> &val, int depth = 0) {
  std::stringstream o;
  o << show(val.info, depth);
  indent(o, depth);
  o << val.name;
  if (!val.args.empty()) {
    o << "(\n";
    for (const auto &arg : val.args) {
      o << show(arg, depth + 2) << "\n";
    }
    indent(o, depth);
    o << ")";
  }
  return o.str();
}

template <typename T>
std::string show(const DefinitionCore<T> &val, int depth) {
  std::stringstream o;
  o << show(ValueCore<T>(val), depth);
  if (val.value) {
    o << " =\n";
    o << show(*val.value, depth + 2);
  }
  return o.str();
}

template <typename T> std::string show(const ModuleCore<T> &module, int depth) {
  std::stringstream o;
  indent(o, depth);
  o << "module " << module.name << " (" << module.definitions.size()
    << " top level definitions) {\n";
  for (const auto &val : module.definitions) {
    o << show(val, depth + 2) << "\n";
  }
  indent(o, depth);
  o << "}";
  return o.str();
}

#endif // #ifndef TOSTRING_H
