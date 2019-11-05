#define DOCTEST_CONFIG_IMPLEMENT_WITH_MAIN
#include "doctest.h"

#include <algorithm>
#include <iostream>
#include <optional>
#include <variant>
#include <vector>

#include "../src/ast.h"
#include "../src/eval.h"
#include "../src/lex.h"
#include "../src/parser.h"
#include "../src/show.h"

#define CHECK_SHOW(a, b, c)                                                    \
  {                                                                            \
    const auto show_str = show((b), (c));                                      \
    CHECK_MESSAGE((a), show_str);                                              \
  }
#define REQUIRE_SHOW(a, b, c)                                                  \
  {                                                                            \
    const auto show_str = show((b), (c));                                      \
    REQUIRE_MESSAGE((a), show_str);                                            \
  }
#define CHECK_SHOW_VALUE(a, b)                                                 \
  {                                                                            \
    const auto show_str = show((b));                                           \
    CHECK_MESSAGE((a), show_str);                                              \
  }
#define REQUIRE_SHOW_VALUE(a, b)                                               \
  {                                                                            \
    const auto show_str = show((b));                                           \
    REQUIRE_MESSAGE((a), show_str);                                            \
  }

// TODO rapidcheck that all tokens from a lexed 'file' are inside the file.
// i.e. their location is 'in bounds' and the getString matches the
// input.getString.

Value getValue(Messages &msgs, Context &ctx) {
  Tokens toks = lex(ctx);
  CHECK_SHOW(msgs.empty(), msgs, ctx);
  const std::string toksS = show(toks, ctx);
  auto tree = ast::ast(toks, ctx, ast::parseValue);
  CHECK_SHOW(msgs.empty(), msgs, ctx);
  REQUIRE(tree);
  parser::ParserContext p_ctx(std::move(ctx));
  auto val =
      parser::parse<std::optional<Value>>(*tree, p_ctx, parser::parseValue);
  CHECK_SHOW(msgs.empty(), msgs, ctx);
  REQUIRE(val);
  return *val;
}

TEST_CASE("a numeric literal") {
  Messages msgs;
  Context ctx = {msgs, "12", "<filename>"};

  Value num = getValue(msgs, ctx);

  SUBCASE("eval") {
    parser::ParserContext p_ctx{std::move(ctx)};
    auto val = eval(num, p_ctx);
    REQUIRE(std::holds_alternative<int>(val));
    REQUIRE(std::get<int>(val) == 12);
  }
}

TEST_CASE("a sum of two numeric literal") {
  Messages msgs;
  Context ctx = {msgs, "2+3", "<filename>"};

  Value num = getValue(msgs, ctx);

  SUBCASE("eval") {
    parser::ParserContext p_ctx{std::move(ctx)};
    auto val = eval(num, p_ctx);
    REQUIRE(std::holds_alternative<int>(val));
    REQUIRE(std::get<int>(val) == 5);
  }
}

TEST_CASE("a mult of two numeric literal") {
  Messages msgs;
  Context ctx = {msgs, "2*3", "<filename>"};

  Value num = getValue(msgs, ctx);

  SUBCASE("eval") {
    parser::ParserContext p_ctx{std::move(ctx)};
    auto val = eval(num, p_ctx);
    REQUIRE(std::holds_alternative<int>(val));
    REQUIRE(std::get<int>(val) == 6);
  }
}

TEST_CASE("a sum of three numeric literal") {
  Messages msgs;
  Context ctx = {msgs, "1+2+3", "<filename>"};

  Value num = getValue(msgs, ctx);

  SUBCASE("eval") {
    parser::ParserContext p_ctx{std::move(ctx)};
    auto val = eval(num, p_ctx);
    REQUIRE(std::holds_alternative<int>(val));
    REQUIRE(std::get<int>(val) == 6);
  }
}

TEST_CASE("a expression with three numeric literal") {
  Messages msgs;
  Context ctx = {msgs, "1+2*3", "<filename>"};

  Value num = getValue(msgs, ctx);

  SUBCASE("eval") {
    parser::ParserContext p_ctx{std::move(ctx)};
    auto val = eval(num, p_ctx);
    REQUIRE(std::holds_alternative<int>(val));
    REQUIRE(std::get<int>(val) == 7);
  }
}

TEST_CASE("a expression with three numeric literal") {
  Messages msgs;
  Context ctx = {msgs, "(1+2)*3", "<filename>"};

  Value num = getValue(msgs, ctx);

  SUBCASE("eval") {
    parser::ParserContext p_ctx{std::move(ctx)};
    auto val = eval(num, p_ctx);
    REQUIRE(std::holds_alternative<int>(val));
    REQUIRE(std::get<int>(val) == 9);
  }
}

TEST_CASE("a string literal") {
  Messages msgs;
  Context ctx = {msgs, "'abc'", "<filename>"};

  Value num = getValue(msgs, ctx);

  SUBCASE("eval") {
    parser::ParserContext p_ctx{std::move(ctx)};
    auto val = eval(num, p_ctx);
    REQUIRE(std::holds_alternative<std::string>(val));
    REQUIRE(std::get<std::string>(val) == "abc");
  }
}

TEST_CASE("an expression with string literals") {
  Messages msgs;
  Context ctx = {msgs, "'abc' + 'def'", "<filename>"};

  Value num = getValue(msgs, ctx);

  SUBCASE("eval") {
    parser::ParserContext p_ctx{std::move(ctx)};
    auto val = eval(num, p_ctx);
    REQUIRE(std::holds_alternative<std::string>(val));
    REQUIRE(std::get<std::string>(val) == "abcdef");
  }
}

TEST_CASE("an expression with string literals") {
  Messages msgs;
  Context ctx = {msgs, "'abc' * (2+1)", "<filename>"};

  Value num = getValue(msgs, ctx);

  SUBCASE("eval") {
    parser::ParserContext p_ctx{std::move(ctx)};
    auto val = eval(num, p_ctx);
    REQUIRE(std::holds_alternative<std::string>(val));
    REQUIRE(std::get<std::string>(val) == "abcabcabc");
  }
}

TEST_CASE("an expression with string literals") {
  Messages msgs;
  Context ctx = {msgs, "'abc' + ('def'*(2+1))", "<filename>"};

  Value num = getValue(msgs, ctx);

  SUBCASE("eval") {
    parser::ParserContext p_ctx{std::move(ctx)};
    auto val = eval(num, p_ctx);
    REQUIRE(std::holds_alternative<std::string>(val));
    REQUIRE(std::get<std::string>(val) == "abcdefdefdef");
  }
}
