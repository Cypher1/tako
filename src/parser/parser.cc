#include <iostream>
#include <vector>
#include <optional>
#include <map>
#include <string>

#include "ast.h"
#include "parser.h"
#include "toString.h"

Value parseValue(const Tree<Token>& node, Messages& msgs, const std::string& content, const std::string& filename) {
  std::vector<Value> exprs;
  for(const auto& child: node.children) {
    exprs.push_back(parseValue(child, msgs, content, filename));
  }
  auto loc = node.value.loc;
  std::string name = getString(loc, content);
  return Value(name, loc, exprs);
}

FuncArg parseArg(Forest<Token>::const_iterator& it, const Forest<Token>::const_iterator& end, int ord, Messages& msgs, const std::string& content, const std::string& filename) {
  std::string name = "#error";
  if (it != end && it->value.type == +TokenType::Symbol) {
    name = getString(it->value.loc, content);
    ++it;
  } else if (it != end) {
    msgs.push_back({
        PassStep::Parse,
        MessageType::Info,
        "Expected symbol name",
        it->value.loc
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
    const auto children = it->children;
    it++;
    auto ch_it = children.cbegin();
    if(ch_it != children.cend()) {
      Value def = parseValue(*ch_it, msgs, content, filename);
      ++ch_it;
      if(ch_it != children.cend()) {
        msgs.push_back({
            PassStep::Parse,
            MessageType::Info,
            std::string("Unexpected ")+(ch_it->value.type)._to_string()+" '"+getString(ch_it->value.loc, content)+"' after declaration",
            ch_it->value.loc
        });
      }
      return {name, ord, def};
    }
    // TODO
  }
  return {name, ord};
}

Definition parseDefinition( Forest<Token>::const_iterator& it, const Forest<Token>::const_iterator& end, Messages& msgs, const std::string& content, const std::string& filename) {
  Definition val = { "#error", {}, {0, 0, "??l"}, {"#unparsed def", {0, 0, "#??l"}, {}} };
  if(it == end) {
    return val;
  }
  val.loc = it->value.loc;;
  val.name = getString(val.loc, content);
  const auto type = it->value.type;
  if (type != +TokenType::Symbol && type != +TokenType::Operator) {
    msgs.push_back({
        PassStep::Parse,
        MessageType::Error,
        "Unexpected '"+val.name+"'",
        val.loc
    });
    ++it;
    return val;
  }
  int n = 0;
  const auto children = it->children;
  auto arg_it = children.cbegin();
  while(arg_it != children.cend()) {
    val.args.push_back(parseArg(arg_it, children.cend(), n++, msgs, content, filename));
  }
  ++it;
  if(it != end && it->value.type == +TokenType::Declaration) {
    const auto children = it->children;
    auto val_it = children.cbegin();
    if(val_it != children.cend()) {
      val.value = parseValue(*val_it, msgs, content, filename);
      ++val_it;
    }
    if(val_it != children.cend()) {
      msgs.push_back({
          PassStep::Parse,
          MessageType::Error,
          "Expected end of declaration for '"+val.name+"', got " + (val_it->value.type)._to_string() + " '" + getString(val_it->value.loc, content) + "' instead.",
          val_it->value.loc
      });
    }
    ++it;
  } else if(it != end) {
    msgs.push_back({
        PassStep::Parse,
        MessageType::Error,
        "Reached end of scope, expected end of definition for '"+val.name+"', got '"+toString(it->value, content, filename)+"' instead.",
        val.loc
    });
  } else {
    msgs.push_back({
        PassStep::Parse,
        MessageType::Error,
        "Reached end of scope, expected end of definition for '"+val.name+"'",
        val.loc
    });
  }
  return val;
}

std::vector<Definition> parseDefinitions(Forest<Token>::const_iterator& it, const Forest<Token>::const_iterator& end, Messages& msgs, const std::string& content, const std::string& filename) {
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
