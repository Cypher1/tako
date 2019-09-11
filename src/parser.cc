#include <functional>
#include <iostream>
#include <map>
#include <optional>
#include <stdexcept>
#include <string>
#include <vector>

#include "context.h"

#include "ast.h"
#include "lex.h"
#include "parser.h"
#include "show.h"
#include "util.h"

namespace parser {

void SymbolTable::addSymbol(const Path &path, const Definition &def) {
  symbol_tree.emplace(path, def);
}

int getMatch(const Path& match, const Path& context, const Path& path) {
  if(match.size() < path.size()) {
    return -1;
  }
  //Check the suffix matches
  int i = match.size()-path.size();
  for(int j = 0; j < path.size(); j++) {
    if(path[j] != match[i+j]) {
      // Candidate doesn't match
      return -1;
    }
  }

  //check that the match is in the context
  i = 0;
  for(; i < match.size()-path.size(); i++) {
    if (i >= context.size()) {
      // Match is hidden inside something in our context
      return -1;
    }
    if (match[i] != context[i]) {
      // Match is hidden inside a sibling
      return -1;
    }
  }
  return i;
}

std::optional<Definition> SymbolTable::lookup(const Path &context, const Path &path) {
  // assert(!context.empty());
  std::cerr << "Lookup " << show(context) << "\n";
  std::optional<Definition> best;
  int best_locality = -1;
  for (const auto &p : symbol_tree) {
    int locality = getMatch(p.first, context, path);
    if(locality > -1 && locality > best_locality) {
      best = p.second;
      locality = best_locality;
    }
  }

  if (best) {
    std::cerr << "Found match\n";
    return best;
  } else {
    std::cerr << "No match found for " << show(path) << "\n";
  }

  return {};
}

void ParserContext::addSymbol(const Path &path, const Definition &def) {
  symbols.addSymbol(path, def);
}

std::optional<Definition> ParserContext::lookup(const Path &context, const Path &path) {
  return symbols.lookup(context, path);
}

std::vector<Path> ParserContext::getSymbols() {
  std::vector<Path> syms;

  return syms;
}

void ParserContext::msg(const Token &tok, MessageType level, std::string msg_txt) {
  context.msg(tok.loc, level, msg_txt);
}

std::string ParserContext::getStringAt(const Location &loc) {
  return context.getStringAt(loc);
}

std::optional<Definition> parseDefinition(Path, const Tree<Token> &node,
                                          ParserContext &ctx);

std::optional<Value> parseValue(Path pth, const Tree<Token> &node, ParserContext &ctx) {
  std::string name = ctx.getStringAt(node.value.loc);
  if (name.empty()) { // End of file?
    return std::nullopt;
  }
  std::vector<Definition> args;
  int ord = 0;
  for (const auto &child : node.children) {
    if (child.value.type == +TokenType::Declaration) {
      const auto arg = parseDefinition(pth, child, ctx);
      // TODO require arg
      args.push_back(*arg);
    } else {
      const auto arg_value = parseValue(pth, child, ctx);
      const std::string name =
          "#" +
          std::to_string(ord++); // Name the anonymous arg something impossible
      args.push_back(Definition(name, child.value.loc, {}, arg_value));
    }
  }
  if (node.value.type == +TokenType::NumberLiteral) {
    return Value(name, node.value.loc, args, AstNodeType::Numeric);
  }
  if (node.value.type == +TokenType::StringLiteral) {
    return Value(name, node.value.loc, args, AstNodeType::Text);
  }
  if ((node.value.type == +TokenType::Symbol) ||
      (node.value.type == +TokenType::Operator) ||
      (node.value.type == +TokenType::PreCond) ||
      (node.value.type == +TokenType::PostCond) ||
      (node.value.type == +TokenType::OpenBrace) ||
      (node.value.type == +TokenType::OpenBracket) ||
      (node.value.type == +TokenType::OpenParen)) {
    return Value(name, node.value.loc, args, AstNodeType::Symbol);
  }
  throw std::runtime_error(std::string("Unexpected value token type ") +
                           node.value.type._to_string());
}

std::optional<Definition> parseDefinition(Path pth, const Tree<Token> &node,
                                          ParserContext &ctx) {
  // Todo check that root is =
  std::string op = ctx.getStringAt(node.value.loc);
  if (node.value.type != +TokenType::Operator || op != "=") {
    // TODO msg conditionally
    ctx.msg(node.value, MessageType::Error, "Expected definition");
    return std::nullopt;
  }
  if (node.children.empty()) {
    // TODO msg conditionally
    ctx.msg(node.value, MessageType::Error, "Expected definition");
    return std::nullopt;
  }
  const auto &fst = node.children[0];
  if (fst.value.type != +TokenType::Symbol) {
    // TODO msg conditionally
    ctx.msg(node.value, MessageType::Error, "Cannot assign to non-symbol");
    return std::nullopt;
  }

  // Get symbol name
  Location loc = fst.value.loc;
  std::string name = ctx.getStringAt(loc);
  std::vector<Definition> args = {};
  // Todo check that root.child[0].child* is = definition
  for (const auto &argTree : fst.children) {
    const std::string argStr = ctx.getStringAt(argTree.value.loc);
    std::optional<Definition> argDef;
    if (argTree.value.type == +TokenType::Operator && argStr == "=") {
      argDef = parseDefinition(pth, argTree, ctx);
    } else if (argTree.value.type == +TokenType::Symbol) {
      argDef = Definition(argStr, argTree.value.loc, {}, std::nullopt);
    }

    if (argDef) {
      Definition arg(*argDef);
      args.push_back(arg);
    } else {
      ctx.msg(argTree.value, MessageType::Error, "Expected a definition");
    }
  }

  std::optional<Value> value = {};
  if (node.children.size() > 1) {
    value = parseValue(pth, node.children[1], ctx);
    if (node.children.size() > 2) {
      // TODO: error if there are other children?
      ctx.msg(node.value, MessageType::Error,
              "Expected a single value for definition");
    }
  }

  if (!value) {
    ctx.msg(node.value, MessageType::Error,
            "Expected a value for definition");
  }

  // Todo check that root.child[1] is = expr
  auto def = Definition(name, loc, args, value);
  ctx.addSymbol(pth, def);
  return def;
}

Module parseModule(Path pth, const Tree<Token> &node, ParserContext &ctx) {
  std::vector<Definition> definitions;
  for (const auto &defTree : node.children) {
    auto def = parseDefinition(pth, defTree, ctx);
    if (def) {
      definitions.push_back(*def);
    }
  }
  return {ctx.context.filename, node.value.loc, definitions};
}

} // namespace parser
