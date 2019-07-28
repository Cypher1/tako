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

namespace parser {
std::optional<Definition> parseDefinition(const Tree<Token> &node,
                                          Context &ctx);

std::optional<Value> parseValue(const Tree<Token> &node, Context &ctx) {
  std::string name = ctx.getStringAt(node.value.loc);
  if (name.empty()) { // End of file?
    return std::nullopt;
  }
  std::vector<Definition> args;
  int ord = 0;
  for (const auto &child : node.children) {
    const auto arg = parseDefinition(child, ctx);
    if (arg) {
      args.push_back(*arg);
    } else {
      // TODO Msg?
      const auto arg_value = parseValue(child, ctx);
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

std::optional<Definition> parseDefinition(const Tree<Token> &node,
                                          Context &ctx) {
  // Todo check that root is =
  std::string op = ctx.getStringAt(node.value.loc);
  if (node.value.type != +TokenType::Operator || op != "=") {
    // TODO msg conditionally
    return std::nullopt;
  }
  if (node.children.empty()) {
    // TODO msg conditionally
    return std::nullopt;
  }
  const auto &fst = node.children[0];
  if (fst.value.type != +TokenType::Symbol) {
    // TODO msg conditionally
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
      argDef = parseDefinition(argTree, ctx);
    } else if (argTree.value.type == +TokenType::Symbol) {
      argDef = Definition(argStr, argTree.value.loc, {}, std::nullopt);
    }

    if (argDef) {
      Definition arg(*argDef);
      args.push_back(arg);
    } else {
      // TODO msg
    }
  }

  std::optional<Value> value = {};
  if (node.children.size() > 1) {
    value = parseValue(node.children[1], ctx);
    // TODO: error if there are other children?
  }

  // Todo check that root.child[1] is = expr
  return Definition(name, loc, args, value);
}

Module parseModule(const Tree<Token> &node, Context &ctx) {
  std::vector<Definition> definitions;
  for (const auto &defTree : node.children) {
    auto def = parseDefinition(defTree, ctx);
    if (def) {
      definitions.push_back(*def);
    }
  }
  return {ctx.filename, node.value.loc, definitions};
}

} // namespace parser
