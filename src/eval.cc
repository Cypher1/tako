#include <cstddef>
#include <functional>
#include <iostream>
#include <optional>
#include <ostream>
#include <string>
#include <variant>

#include "ast.h"
#include "eval.h"
#include "parser.h"
#include "show.h"

std::ostream& operator<<(std::ostream& o, const PrimError e) {
  o << e.msg << "\n";
  return o;
}

std::string repeat(int n, std::string rep) {
  size_t l = rep.length() * n;
  while (rep.length() < l) {
    rep += rep;
  }
  return rep.substr(0, l);
}

std::string repeatR(std::string rep, int n) {
  return repeat(n, rep);
}

template<typename T>
Prim mins(T x, T y) { return x - y; }

template<typename T>
Prim add(T x, T y) { return x + y; }

template<typename T>
Prim mult(T x, T y) { return x * y; }

TryPrim require(const Pred req, const TryPrim cont) {
  return [=]() -> Prim {
    if (req()) {
      return cont();
    }
    return PrimError("requirement failure");
  };
}

TryPrim tryEach(const TryPrims fs, const PrimError msg) {
  return [=]() -> Prim {
    for (const auto& f : fs) {
      const Prim v = f();
      if (!std::holds_alternative<PrimError>(v)) {
        return v;
      }
    }
    return Prim(msg);
  };
}

template<typename T, typename U>
TryPrim operator2(const std::string name, const Prims vals, const std::function<Prim(T, U)> f) {
    const auto typeErr = PrimError("Expected two arguments at !!! " + name);
    if (vals.size() != 2) {
      return [name, typeErr](){ return typeErr;};
    }
  return [vals, f, typeErr]() -> Prim {
    auto x = vals[0];
    if (!std::holds_alternative<T>(x)) {
      return typeErr;
    }
    auto y = vals[1];
    if (!std::holds_alternative<U>(y)) {
      return typeErr;
    }
    return f(std::get<T>(x), std::get<U>(y));
  };
}

Prim evalSymbol(Path context, Path name, parser::ParserContext& p_ctx) {
  // TODO(cypher1): Context should only change on imports
  // TODO(cypher1): Stack should change on calls
  auto o_def = p_ctx.getTable().lookup(context, name);
  if (!o_def) {
    return PrimError("Module has no "+show(context, 0, "/")+" with appropriate arguments");
  }
  auto def = *o_def;
  if (!def.value) {
    return PrimError(show(name, 0, "/")+" has no set value");
  }
  auto val = *def.value;
  for (Definition arg : def.args) {
    // Push default args and args in.
    auto child = context;
    child.push_back(arg.name);
    auto arg_val = p_ctx.getTable().lookup(context, {arg.name});
    if (arg_val) {
      arg.value = arg_val->value;
    }
    if (!arg.value) {
      // Argument missing
    } else {
      p_ctx.addSymbol(child, arg);
    }
  }
  return eval(context, val, p_ctx);
}

Prim eval(Path context, Value val, parser::ParserContext& p_ctx) {
  // TODO: Eval
  if (val.data) {
    // If we've already 'forced' a value we should use it.
    return *val.data;
  }
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
    std::vector<Prim> values;
    for (const auto &arg : val.args) {
      if (!arg.value) {
        return PrimError("Missing value for arg in !!! " + val.name);
      }
      const auto& val = eval(context, *arg.value, p_ctx);
      if (std::holds_alternative<PrimError>(val)) {
        return val;
      }
      values.push_back(val);
    }

    const TryPrim adders =
      require(
          [val]{return val.name == "+";},
          tryEach({
            operator2<int, int>("+", values, add<int>),
            operator2<std::string, std::string>("+", values, add<std::string>)
            }, "Unexpected types at (+) !!! " + val.name)
          );

    const TryPrim subs =
      require(
          [val]{return val.name == "-";},
          tryEach({
            operator2<int, int>("-", values, mins<int>),
            }, "Unexpected types at (-) !!! " + val.name)
          );

    const TryPrim mults =
      require(
          [val]{return val.name == "*";},
          tryEach({
            operator2<int, int>("*", values, mult<int>),
            operator2<std::string, int>("*", values, repeatR),
            operator2<int, std::string>("*", values, repeat)
            }, "Unexpected types at (*) !!! " + val.name)
          );

    const Prim v = tryEach({adders, subs, mults}, "Unknown symbol !!! " + val.name)();
    if (!std::holds_alternative<PrimError>(v)) {
      return v;
    }

    // Function call (or variable evaluation)
    auto def = p_ctx.lookup(context, {val.name});
    std::cerr << "Looking for def " << val.name << "\n";
    if (!def) {
      return PrimError("Module has no " + val.name + " with appropriate arguments");
    }
    std::vector<std::string> missing;
    for (auto arg : def->args) { //TODO: Unsafe
      bool gotIt = false;
      for (auto varg : val.args) {
        if (varg.name == arg.name) {
          gotIt = true;
          break;
        }
      }
      if (!gotIt) {
        missing.push_back(arg.name);
      }
    }
    int argInd = 0;
    for (auto& arg : val.args) {
      std::cerr << "arg:" << argInd << " " << arg.name << "\n";
      if (arg.name[0] == '#') {
        arg.name = missing[argInd]; // TODO: Unsafe
        argInd++;
      }
      std::cerr << "arg:" << argInd << " " << arg.name << "\n";
    }
    for (auto arg : val.args) {
      if (arg.value) {
        const Prim v = eval(context, *arg.value, p_ctx);
        if (std::holds_alternative<PrimError>(v)) {
          // TODO: Throw the error
          return v;
        }
        arg.value->data = v;
        auto child = context;
        child.push_back(val.name);
        child.push_back(arg.name);
        p_ctx.addSymbol(child, arg);
      } else {
        // Check for a default argument
        // If theres not one, throw.
      }
    }
    // Manage 'path'
    context.push_back(val.name);
    auto sym_v = evalSymbol(context, {val.name}, p_ctx);
    // Remove things from the stack?
    // Undo the 'path'
    context.pop_back();
    std::cerr << "v: " << show(sym_v) << "\n";
    return sym_v;
  }
  return PrimError("OH NO!!! " + val.name);
}

Prim eval(Path context, Module mod, parser::ParserContext& p_ctx) {
  return evalSymbol(context, {"main"}, p_ctx);
}
