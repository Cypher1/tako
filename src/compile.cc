#include <iostream>
#include "parser.h"
#include "eval.h"
#include "checker.h"
#include "show.h"

std::optional<Tree<Token>> getTree(Context &ctx) {
    if (ctx.done()) {
      return {};
    }

    Tokens toks = lex(ctx);
    if (ctx.done()) {
      std::cerr << "Lexed " << toks.size() << " tokens.\n";
      std::cerr << show(toks, ctx) << "\n";
      return {};
    }

    std::optional<Tree<Token>> tree = ast::ast(toks, ctx, ast::parseValue);
    if (ctx.done()) {
      std::cerr << show(*tree, ctx) << "\n";
    }
    return tree;
}

void runCompilerInteractive(Context &ctx) {
  try {
    auto tree = getTree(ctx);
    if (!tree || ctx.done()) {
      return;
    }

    parser::ParserContext p_ctx(std::move(ctx));
    std::optional<Value> o_val =
        parser::parse<std::optional<Value>>(*tree, p_ctx, parser::parseValue);
    if (!o_val) {
      std::cerr << "Parse Failed\n";
      return;
    }
    if (p_ctx.done()) {
      std::cerr << show(*o_val) << "\n";
      for (const auto msg : p_ctx.getMsgs()) {
        std::cerr << show(msg, p_ctx, 2) << "\n";
      }
      return;
    }
    auto val = *o_val;
    CheckedValue checked = check(val, p_ctx);
    if (p_ctx.done()) {
      std::cerr << show(checked) << "\n";
      return;
    }

    // TODO
    const auto res = eval(val);
    if (std::holds_alternative<int>(res)) {
      std::cout << std::get<int>(res) << "\n";
    } else {
      std::cout << std::get<std::string>(res) << "\n";
    }

    if (p_ctx.done()) {
      return;
    }
  } catch (const std::runtime_error &er) {
    std::cerr << "Parser crashed with: " << er.what() << "\n";
  }

  for (const auto msg : ctx.getMsgs()) {
    std::cerr << show(msg, ctx, 2) << "\n";
  }
  ctx.getMsgs() = {}; // Clear out the message log.
}

void runCompiler(Context &ctx) {
  try {
    auto tree = getTree(ctx);
    if (!tree || ctx.done()) {
      return;
    }

    parser::ParserContext p_ctx(std::move(ctx));
    Module module = parser::parse<Module>(*tree, p_ctx, parser::parseModule);
    if (p_ctx.done()) {
      auto &symbs = p_ctx.symbols;
      symbs.forAll([](Path &context, Definition &def) {
        std::cerr << "path: " << show(context, 0, "/") << "\n";
        std::cerr << "def: " << show(def) << "\n";
      });
      // std::cerr << show(module, 0) << "\n";
      return;
    }

    CheckedModule checked = check(module, p_ctx);
    if (p_ctx.done()) {
      std::cerr << show(checked) << "\n";
      return;
    }

    // TODO: Code gen (no biggy)
  } catch (const std::runtime_error &er) {
    std::cerr << "Parser crashed with: " << er.what() << "\n";
  }

  for (const auto msg : ctx.getMsgs()) {
    std::cerr << show(msg, ctx, 2) << "\n";
  }
  ctx.getMsgs() = {}; // Clear out the message log.
}
