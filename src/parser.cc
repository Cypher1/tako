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

void SymbolTable::addSymbol(std::vector<Symbol> path, const Value &val) {
  symbol_tree.emplace(path, val);
}

Path SymbolTable::lookup(Path pth, Value &val) {
  // assert(!pth.empty());
  std::cerr << "Lookup " << show(pth) << "\n";
  Path best;
  size_t best_dep_eq = 0;
  for (const auto &p : symbol_tree) {
    // assert(!p.first.empty());
    size_t dep_eq = 0;
    if (p.first.back() == val.name) { // same name
      // Found a match... is it in our scope?
      while (dep_eq < p.first.size() && dep_eq < pth.size()) {
        if (p.first[dep_eq] != pth[dep_eq]) {
          break;
        }
        dep_eq++;
      }
      std::cerr << "Matching " << show(p.first) << " matches " << dep_eq
                << " deep\n";
      // foo/bar/symbol
      // foo/bar/baz/function uses symbol
      //
      // foo/bar is the match, symbol == symbol
      if (dep_eq >= p.first.size() - 1) {
        std::cerr << "Matches!\n";
        if (dep_eq > best_dep_eq) {
          std::cerr << "Shadows previous\n";
          best_dep_eq = dep_eq;
          best = p.first; // Copy the path
        } else {
          std::cerr << "Is shadowed by previous\n";
        }
      } else {
        std::cerr << "Does not match! Cannot access " << show(p.first)
                  << " from " << show(pth) << "\n";
      }
    } else {
    }
  }

  if (best.empty()) {
    std::cerr << "No match found for " << val.name << "\n";
  } else {
    std::cerr << "Found match\n";
  }

  return {};
}

void ParserContext::msg(const Token &tok, MessageType level, std::string msg_txt) {
  context.msg(tok.loc, level, msg_txt);
}

std::string ParserContext::getStringAt(const Location &loc) {
  return context.getStringAt(loc);
}

std::optional<Definition> parseDefinition(const Tree<Token> &node,
                                          ParserContext &ctx);

std::optional<Value> parseValue(const Tree<Token> &node, ParserContext &ctx) {
  std::string name = ctx.getStringAt(node.value.loc);
  if (name.empty()) { // End of file?
    return std::nullopt;
  }
  std::vector<Definition> args;
  int ord = 0;
  for (const auto &child : node.children) {
    if (child.value.type == +TokenType::Declaration) {
      const auto arg = parseDefinition(child, ctx);
      // TODO require arg
      args.push_back(*arg);
    } else {
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
      argDef = parseDefinition(argTree, ctx);
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
    value = parseValue(node.children[1], ctx);
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
  return Definition(name, loc, args, value);
}

Module parseModule(const Tree<Token> &node, ParserContext &ctx) {
  std::vector<Definition> definitions;
  for (const auto &defTree : node.children) {
    auto def = parseDefinition(defTree, ctx);
    if (def) {
      definitions.push_back(*def);
    }
  }
  return {ctx.context.filename, node.value.loc, definitions};
}

} // namespace parser
