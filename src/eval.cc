#include <iostream>
#include <string>
#include <variant>

#include "ast.h"
#include "eval.h"
#include "show.h"

using Prim = std::variant<int, std::string>;

std::string repeat(int n, std::string rep) {
  std::string o = rep;
  while (o.length() < rep.length() * n) {
    o += o;
  }
  return o.substr(0, rep.length() * n);
}

template<typename T>
bool require2(const std::vector<T>& val) {
  return val.size() == 2;
}

template<typename T, typename U, typename R, typename Holder>
std::optional<R> simpleOperator(std::vector<Holder> vals, std::function<R(T, U)> f) {
  // TODO: require 2.
  auto x = vals[0];
  auto y = vals[1];
  if (!std::holds_alternative<T>(x)) {
    return std::nullopt;
  }
  if (!std::holds_alternative<U>(y)) {
    return std::nullopt;
  }
  return f(std::get<T>(x), std::get<U>(y));
}

template<typename T>
T add(T x, T y) { return x + y; }

template<typename T>
T mult(T x, T y) { return x * y; }

Prim eval(Value val) {
  std::cerr << "Eval (" << show(val) << ")\n";
  // TODO: Eval
  if (val.node_type == AstNodeType::Text) {
    // Get the text
    return val.name.substr(1, val.name.length() - 2);
  }
  if (val.node_type == AstNodeType::Numeric) {
    // Get the number
    return std::stoi(val.name, nullptr, 10); // Assume base 10
  }
  if (val.node_type == AstNodeType::Symbol) {
    // Look up the symbol
    const std::vector<Definition> args = val.args;
    std::vector<Prim> values;
    for (const auto &arg : args) {
      if (arg.value) {
        values.push_back(eval(*arg.value));
      } else {
        return "Missing value for arg in !!! " + val.name;
      }
    }
    if (!require2(values)) {
      return "Expected two arguments at !!! " + val.name;
    }
    if (val.name == "+") {
      // Require two args for now?
      if (auto v = simpleOperator<int, int, int>(values, add<int>)) {
        return *v;
      }
      if (auto v = simpleOperator<std::string, std::string, std::string>(values, add<std::string>)) {
        return *v;
      }
      return "Unexpected types at (+) !!! " + val.name;
    } else if (val.name == "*") {
      // Require two args for now?
      if (auto v = simpleOperator<int, int, int>(values, mult<int>)) {
        return *v;
      }
      if (auto v = simpleOperator<int, std::string, std::string>(std::vector({values[1], values[0]}), repeat)) {
        return *v;
      }
      if (auto v = simpleOperator<int, std::string, std::string>(values, repeat)) {
        return *v;
      }
      return "Unexpected types at (*) !!! " + val.name;
    }
    return "Unknown symbol !!! " + val.name;
  }
  return "OH NO!!! " + val.name;
}
