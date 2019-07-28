#include <iostream>
#include <string>
#include <variant>

#include "eval.h"
#include "ast.h"
#include "show.h"

using Prim = std::variant<int, std::string>;

Prim eval(Value val) {
  std::cerr << "Eval (" << show(val) << ")\n";
  // TODO: Eval
  if(val.node_type == AstNodeType::Text) {
    // Get the text
    return val.name.substr(1, val.name.length()-2);
  }
  if(val.node_type == AstNodeType::Numeric) {
    // Get the number
    return std::stoi(val.name, nullptr, 10); // Assume base 10
  }
  if(val.node_type == AstNodeType::Symbol) {
    // Look up the symbol
    const std::vector<Definition> args = val.args;
    std::vector<Prim> values;
    for(const auto& arg : args) {
      if(arg.value) {
        values.push_back(eval(*arg.value));
      } else {
        return "Missing value for arg in !!! "+val.name;
      }
    }
    if(val.name == "+") {
      // Require two args for now?
      if(std::holds_alternative<int>(values[0]) && std::holds_alternative<int>(values[1])) {
        return std::get<int>(values[0])+std::get<int>(values[1]);
      }
      if(std::holds_alternative<std::string>(values[0]) && std::holds_alternative<std::string>(values[1])) {
        return std::get<std::string>(values[0])+std::get<std::string>(values[1]);
      }
      return "Unexpected types at (+) !!! "+val.name;
    } else if(val.name == "*") {
      // Require two args for now?
      if(std::holds_alternative<int>(values[0]) && std::holds_alternative<int>(values[1])) {
        return std::get<int>(values[0])*std::get<int>(values[1]);
      }
      return "Unexpected types at (*) !!! "+val.name;
    } else {
      return "Unknown symbol !!! "+val.name;
    }
  }
  return "OH NO!!! "+val.name;
}
