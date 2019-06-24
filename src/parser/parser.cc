#include <iostream>
#include <vector>
#include <optional>
#include <map>
#include <string>

#include "ast.h"
#include "parser.h"
#include "toString.h"

Value parseValue( std::vector<Tree<Token>>::const_iterator& it, const std::vector<Tree<Token>>::const_iterator& end, Messages& msgs, const std::string& content, const std::string& filename) {
  if(it != end) {
    std::string name = getString(it->value.loc, content);
    auto loc = it->value.loc;

    std::vector<Value> exprs;
    const auto childers = it->children;
    auto expr_it = childers.cbegin();
    while(expr_it != childers.cend()) {
      exprs.push_back(parseValue(expr_it, childers.cend(), msgs, content, filename));
    }
    ++it;
    return Value(name, loc, exprs);
  }
  return Value("#error", {0, 0, "#error"}, {});
}

FuncArg parseArg(std::vector<Tree<Token>>::const_iterator& it, const std::vector<Tree<Token>>::const_iterator& end, int ord, Messages& msgs, const std::string& content, const std::string& filename) {
  std::string name = "#error";
  if (it != end && it->value.type == +TokenType::Symbol) {
    name = getString(it->value.loc, content);
    ++it;
  } else if (it != end) {
    msgs.push_back({
        PassStep::Parse,
        MessageType::Info,
        "Expected symbol name",
        {0, 0, "#???"}
    });
  } else {
    msgs.push_back({
        PassStep::Parse,
        MessageType::Info,
        "Unexpected end of input",
        {0, 0, "#???"}
    });
  }
  if (it != end && it->value.type == +TokenType::Declaration) {
    const auto childers = it->children;
    auto val_it = childers.cbegin();
    Value def = parseValue(val_it, childers.cend(), msgs, content, filename);
    if(val_it != childers.cend()) {
      // TODO
    }
    ++it;
    return {name, ord, def};
  }
  return FuncArg(name, ord);
}

Definition parseDefinition( std::vector<Tree<Token>>::const_iterator& it, const std::vector<Tree<Token>>::const_iterator& end, Messages& msgs, const std::string& content, const std::string& filename) {
  Definition val = { "#error", {}, {0, 0, "??l"}, {"#unparsed def", {0, 0, "#??l"}, {}} };
  if(it == end) {
    return val;
  }
  val.loc = it->value.loc;;
  val.name = getString(val.loc, content);
  const auto type = it->value.type;
  if (type != +TokenType::Symbol
  && type != +TokenType::Operator) {
    msgs.push_back({
        PassStep::Parse,
        MessageType::Error,
        "Unexpected '"+val.name+"'",
        val.loc
    });
  }
  int n = 0;
  const auto childers = it->children;
  auto arg_it = childers.cbegin();
  while(arg_it != childers.cend()) {
    val.args.push_back(parseArg(arg_it, childers.cend(), n++, msgs, content, filename));
  }
  ++it;
  if(it != end && it->value.type == +TokenType::Declaration) {
const auto childers = it->children;
    auto val_it = childers.cbegin();
    val.value = parseValue(val_it, childers.cend(), msgs, content, filename);
    if(val_it != childers.cend()) {
      msgs.push_back({
          PassStep::Parse,
          MessageType::Error,
          "Expected end of declaration for '"+val.name+"', got " + (val_it->value.type)._to_string() + " '" + getString(val_it->value.loc, content) + "' instead.",
          val_it->value.loc
      });
      return val;
    }
    ++it;
  } else if(it != end) {
    msgs.push_back({
        PassStep::Parse,
        MessageType::Error,
        "Reached end of scope, expected a definition for '"+val.name+"', got '"+toString(it->value, content, filename)+"' instead.",
        val.loc
    });
  } else {
    msgs.push_back({
        PassStep::Parse,
        MessageType::Error,
        "Reached end of scope, expected a definition for '"+val.name+"'",
        val.loc
    });
  }
  return val;
}

std::vector<Definition> parseDefinitions(std::vector<Tree<Token>>::const_iterator& it, const std::vector<Tree<Token>>::const_iterator& end, Messages& msgs, const std::string& content, const std::string& filename) {
  std::vector<Definition> definitions;
  while(it != end) {
    definitions.push_back(parseDefinition(it, end, msgs, content, filename));
  }
  return definitions;
}

Module parse(Tree<Token>& tree, Messages& msgs, const std::string& content, const std::string& filename) {
  auto children = tree.children;
  auto it = children.cbegin();
  return {
    filename,
    parseDefinitions(it, children.cend(), msgs, content, filename)
  };
}
