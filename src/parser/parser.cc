#include <iostream>
#include <vector>
#include <optional>
#include <map>
#include <string>

#include "../util/context.h"

#include "ast.h"
#include "lex.h"
#include "parser.h"
#include "toString.h"

std::optional<Definition> parseDefinition(const Tree<Token>& node, Context ctx);

std::optional<Value> parseValue(const Tree<Token>& node, Context ctx) {
  std::vector<Definition> args;
  int ord = 0;
  for(const auto& child : node.children) {
    const auto arg = parseDefinition(child, ctx);
    if(arg) {
      args.push_back(*arg);
    } else {
      // TODO Msg?
      const auto arg_value = parseValue(child, ctx);
      const std::string name = "#"+std::to_string(ord++); // Name the anonymous arg something impossible
      args.push_back(Definition(name, child.value.loc, {}, arg_value));
    }
  }
  return Value(ctx.getStringAt(node.value.loc), node.value.loc, args);
}

std::optional<Definition> parseDefinition(const Tree<Token>& node, Context ctx) {
  ctx.startStep(PassStep::Parse);
  // Todo check that root is =
  std::string op = ctx.getStringAt(node.value.loc);
  if (node.value.type != +TokenType::Operator || op != "=") {
    return {};
  }
  std::string name = "#error";
  std::vector<Definition> args = {};
  Location loc = {0, 0, "#errorfile"};
  // Get symbol name
  std::optional<Value> value = {};

  if (!node.children.empty()) {
    const auto& fst = node.children[0];
    if(fst.value.type == +TokenType::Symbol) {
      name = ctx.getStringAt(fst.value.loc);
      // Todo check that root.child[0].child* is = definition
      for(const auto& argTree : fst.children) {
        const std::string argStr = ctx.getStringAt(argTree.value.loc);
        std::optional<Definition> argDef;
        if (argTree.value.type == +TokenType::Operator && argStr == "=") {
          argDef = parseDefinition(argTree, ctx);
        } else if(argTree.value.type == +TokenType::Symbol) {
          argDef = Definition(argStr, argTree.value.loc, {}, std::nullopt);
        }

        if(argDef) {
          Definition arg(*argDef);
          args.push_back(arg);
        } else {
          // TODO msg
        }
      }
      loc = {0, 0, "#errorfile"};
      // Todo check that root.child[1] is = expr
      value = {};
      /*
      msgs.push_back({
          PassStep::Parse,
          MessageType::Error,
          "Reached end of scope, expected end of definition for '"+val.name+"', got '"+toString(node.value, content, filename)+"' instead.",
          val.loc
      });
      */
    }
    if(node.children.size() > 1) {
      value = parseValue(node.children[1], ctx);
    }
  }
  // Todo check that root.child[1] is = expr
  return Definition(name, loc, args, value);
}

Module parse(const Tree<Token>& module, Context ctx) {
  std::vector<Definition> definitions;
  for(const auto& defTree : module.children) {
    auto def = parseDefinition(defTree, ctx);
    if(def) {
      definitions.push_back(*def);
    }
  }
  return { ctx.filename, definitions };
}
