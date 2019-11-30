#define DOCTEST_CONFIG_IMPLEMENT_WITH_MAIN
#include "doctest.h"

#include <algorithm>
#include <iostream>
#include <optional>
#include <vector>

#include "../src/ast.h"
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

#define CHECK_ALL_MESSAGE(val, expected, msg)                                  \
  {                                                                            \
    const auto res = (val);                                                    \
    const auto expt = (expected);                                              \
    const std::string s = show(res, 0, "/");                                   \
    INFO(s);                                                                   \
    REQUIRE_MESSAGE(res.size() == expt.size(), msg);                           \
    for (size_t i = 0; i < expt.size(); i++) {                                 \
      CHECK_MESSAGE(res[i] == expt[i], msg);                                   \
    }                                                                          \
  }

// TODO rapidcheck that all tokens from a lexed 'file' are inside the file.
// i.e. their location is 'in bounds' and the getString matches the
// input.getString.

TEST_CASE("empty file") {
  Messages msgs;
  Context ctx = {msgs, "", "<filename>"};

  SUBCASE("tokenize yields no tokens") {
    Tokens toks = lex(ctx);
    CHECK_SHOW(msgs.empty(), msgs, ctx);
    CHECK_MESSAGE(toks.empty(), toks[0].type);
    SUBCASE("ast should be an empty tree") {
      auto tree = ast::ast(toks, ctx, ast::parseValue);
      CHECK_SHOW(msgs.empty(), msgs, ctx);
      REQUIRE(!tree);
    }
  }
}

TEST_CASE("non-empty file") {
  Messages msgs;
  Context ctx = {msgs, " ", "<filename>"};

  SUBCASE("tokenize") {
    Tokens toks = lex(ctx);
    CHECK_SHOW(msgs.empty(), msgs, ctx);
    CHECK(toks.empty());
    SUBCASE("ast") {
      auto tree = ast::ast(toks, ctx, ast::parseValue);
      CHECK_SHOW(msgs.empty(), msgs, ctx);
      REQUIRE(!tree);
    }
  }
}

TEST_CASE("a numeric literal") {
  Messages msgs;
  Context ctx = {msgs, "12", "<filename>"};

  SUBCASE("tokenize") {
    Tokens toks = lex(ctx);
    CHECK_SHOW(msgs.empty(), msgs, ctx);
    REQUIRE(toks.size() >= 1);
    CHECK(toks.size() == 1);
    CHECK_SHOW(toks[0].type == +TokenType::NumberLiteral, toks, ctx);
    SUBCASE("ast") {
      auto tree = ast::ast(toks, ctx, ast::parseValue);
      CHECK_SHOW(msgs.empty(), msgs, ctx);
      REQUIRE(tree);
      CHECK(tree->value.type == +TokenType::NumberLiteral);
      CHECK(tree->value.loc.file == "<filename>");
      CHECK(tree->value.loc.start == 0);
      CHECK(tree->value.loc.length == 2);
      CHECK(tree->children.size() == 0);
      SUBCASE("parse") {
        parser::ParserContext p_ctx(std::move(ctx));
        auto o_val = parser::parse<std::optional<Value>>(*tree, p_ctx,
                                                         parser::parseValue);
        // TODO: Check that we got the actual value
        CHECK_SHOW(msgs.empty(), msgs, ctx);
        REQUIRE(o_val);
      }
    }
  }
}

TEST_CASE("a string literal") {
  Messages msgs;
  Context ctx = {msgs, "'123abc!'", "<filename>"};

  SUBCASE("tokenize") {
    Tokens toks = lex(ctx);
    CHECK_SHOW(msgs.empty(), msgs, ctx);
    REQUIRE(toks.size() >= 1);
    CHECK_SHOW(toks.size() == 1, toks, ctx);
    CHECK_SHOW(toks[0].type == +TokenType::StringLiteral, toks, ctx);
    SUBCASE("ast") {
      auto tree = ast::ast(toks, ctx, ast::parseValue);
      CHECK_SHOW(msgs.empty(), msgs, ctx);
      REQUIRE(tree);
      CHECK(tree->value.type == +TokenType::StringLiteral);
      CHECK(tree->children.size() == 0);
      SUBCASE("parse") {
        parser::ParserContext p_ctx(std::move(ctx));
        auto o_val = parser::parse<std::optional<Value>>(*tree, p_ctx,
                                                         parser::parseValue);
        // TODO: Check that we got the actual value
        CHECK_SHOW(msgs.empty(), msgs, ctx);
        REQUIRE(o_val);
      }
    }
  }
}

TEST_CASE("unterminated string literal with newlines") {
  Messages msgs;
  Context ctx = {msgs, "'123\n", "<filename>"};

  SUBCASE("tokenize") {
    Tokens toks = lex(ctx);
    REQUIRE(toks.size() >= 1);
    CHECK(toks.size() == 1);
    REQUIRE_SHOW(msgs.size() == 1, msgs, ctx);
    CHECK(msgs[0].msg == "Unterminated string literal (or maybe you wanted a "
                         "\"multiline string\"?)");
    CHECK_SHOW(toks[0].type == +TokenType::StringLiteral, toks, ctx);
    SUBCASE("ast") {
      auto tree = ast::ast(toks, ctx, ast::parseValue);
      CHECK_SHOW(msgs.size() == 1, msgs, ctx);
      REQUIRE(tree);
      CHECK(tree->value.type == +TokenType::StringLiteral);
      CHECK(tree->children.size() == 0);
      SUBCASE("parse") {
        parser::ParserContext p_ctx(std::move(ctx));
        auto o_val = parser::parse<std::optional<Value>>(*tree, p_ctx,
                                                         parser::parseValue);
        // TODO: Check that we got the actual value
        CHECK_SHOW(msgs.size() == 1, msgs, ctx);
        REQUIRE(o_val);
      }
    }
  }
}

TEST_CASE("a string literal with newlines") {
  Messages msgs;
  Context ctx = {msgs, "'123\n'foo", "<filename>"};

  SUBCASE("tokenize") {
    Tokens toks = lex(ctx);
    REQUIRE(toks.size() >= 2);
    CHECK(toks.size() == 2);
    CHECK_SHOW(toks[0].type == +TokenType::StringLiteral, toks, ctx);
    CHECK_SHOW(toks[1].type == +TokenType::StringLiteral, toks, ctx);
    REQUIRE_SHOW(msgs.size() == 2, msgs, ctx);
    CHECK(msgs[0].msg == "Unterminated string literal (or maybe you wanted a "
                         "\"multiline string\"?)");
    CHECK(msgs[1].msg == "Unterminated string literal, found end of file.");
  }
}

TEST_CASE("a multiline string literal with newlines") {
  Messages msgs;
  Context ctx = {msgs, "\"123\nabc!\"", "<filename>"};

  SUBCASE("tokenize") {
    Tokens toks = lex(ctx);
    REQUIRE(toks.size() >= 1);
    CHECK(toks.size() == 1);
    CHECK_SHOW(msgs.empty(), msgs, ctx);
    CHECK_SHOW(toks[0].type == +TokenType::StringLiteral, toks, ctx);
    SUBCASE("ast") {
      auto tree = ast::ast(toks, ctx, ast::parseValue);
      CHECK_SHOW(msgs.empty(), msgs, ctx);
      REQUIRE(tree);
      CHECK(tree->value.type == +TokenType::StringLiteral);
      CHECK(tree->children.size() == 0);
      SUBCASE("parse") {
        parser::ParserContext p_ctx(std::move(ctx));
        auto o_val = parser::parse<std::optional<Value>>(*tree, p_ctx,
                                                         parser::parseValue);
        // TODO: Check that we got the actual value
        CHECK_SHOW(msgs.empty(), msgs, ctx);
        REQUIRE(o_val);
      }
    }
  }
}

TEST_CASE("variable name") {
  Messages msgs;
  Context ctx = {msgs, "var", "<filename>"};

  SUBCASE("tokenize") {
    Tokens toks = lex(ctx);
    CHECK_SHOW(msgs.empty(), msgs, ctx);
    REQUIRE(toks.size() == 1);
    CHECK(toks[0].type == +TokenType::Symbol);
    SUBCASE("ast") {
      auto tree = ast::ast(toks, ctx, ast::parseValue);
      CHECK_SHOW(msgs.empty(), msgs, ctx);
      REQUIRE(tree);
      CHECK(tree->value.type == +TokenType::Symbol);
      CHECK(tree->children.size() == 0);
    }
  }
}

TEST_CASE("simple expressions") {
  Messages msgs;
  Context ctx = {msgs, "var * 32", "<filename>"};

  SUBCASE("tokenize") {
    Tokens toks = lex(ctx);
    CHECK_SHOW(msgs.empty(), msgs, ctx);
    std::vector<TokenType> tokTypes = {};
    for (const auto &tok : toks) {
      tokTypes.push_back(tok.type);
    }
    CHECK_ALL_MESSAGE(
        tokTypes,
        std::vector<TokenType>({+TokenType::Symbol, +TokenType::Operator,
                                +TokenType::NumberLiteral}),
        "Correct tokens");
    SUBCASE("ast") {
      auto tree = ast::ast(toks, ctx, ast::parseValue);
      CHECK_SHOW(msgs.empty(), msgs, ctx);
      REQUIRE(tree);
      CHECK(tree->value.type == +TokenType::Operator);
      REQUIRE(tree->children.size() == 2);
      CHECK(tree->children[0].value.type == +TokenType::Symbol);
      CHECK(tree->children[0].children.size() == 0);
      CHECK(tree->children[1].value.type == +TokenType::NumberLiteral);
      CHECK(tree->children[1].children.size() == 0);
    }
  }
}

TEST_CASE("simple expressions reversed") {
  Messages msgs;
  Context ctx = {msgs, "32 * var", "<filename>"};

  SUBCASE("tokenize") {
    Tokens toks = lex(ctx);
    CHECK_SHOW(msgs.empty(), msgs, ctx);
    std::vector<TokenType> tokTypes = {};
    for (const auto &tok : toks) {
      tokTypes.push_back(tok.type);
    }
    CHECK_ALL_MESSAGE(
        tokTypes,
        std::vector<TokenType>({+TokenType::NumberLiteral, +TokenType::Operator,
                                +TokenType::Symbol}),
        "Check tokens");
    SUBCASE("ast") {
      auto tree = ast::ast(toks, ctx, ast::parseValue);
      CHECK_SHOW(msgs.empty(), msgs, ctx);
      REQUIRE(tree);
      CHECK(tree->value.type == +TokenType::Operator);
      REQUIRE(tree->children.size() == 2);
      CHECK(tree->children[0].value.type == +TokenType::NumberLiteral);
      CHECK(tree->children[0].children.size() == 0);
      CHECK(tree->children[1].value.type == +TokenType::Symbol);
      CHECK(tree->children[1].children.size() == 0);
    }
  }
}

TEST_CASE("simple expressions with calls") {
  Messages msgs;
  Context ctx = {msgs, "32 * var()", "<filename>"};

  SUBCASE("tokenize") {
    Tokens toks = lex(ctx);
    CHECK_SHOW(msgs.empty(), msgs, ctx);
    std::vector<TokenType> tokTypes = {};
    for (const auto &tok : toks) {
      tokTypes.push_back(tok.type);
    }
    CHECK_ALL_MESSAGE(
        tokTypes,
        std::vector<TokenType>({+TokenType::NumberLiteral, +TokenType::Operator,
                                +TokenType::Symbol, +TokenType::OpenParen,
                                +TokenType::CloseParen}),
        "Check tokens");

    SUBCASE("ast") {
      auto tree = ast::ast(toks, ctx, ast::parseValue);
      CHECK_SHOW(msgs.empty(), msgs, ctx);
      REQUIRE(tree);
      CHECK(tree->value.type == +TokenType::Operator);
      REQUIRE(tree->children.size() == 2);
      CHECK(tree->children[0].value.type == +TokenType::NumberLiteral);
      REQUIRE(tree->children[0].children.size() == 0);
      CHECK(tree->children[1].value.type == +TokenType::Symbol);
      REQUIRE(tree->children[1].children.size() == 0);
    }
  }
}

TEST_CASE("simple expressions with calls with arguments") {
  Messages msgs;
  Context ctx = {msgs, "32 * var(a, 3)", "<filename>"};

  SUBCASE("tokenize") {
    Tokens toks = lex(ctx);
    std::vector<TokenType> tokTypes = {};
    for (const auto &tok : toks) {
      tokTypes.push_back(tok.type);
    }
    CHECK_ALL_MESSAGE(
        tokTypes,
        std::vector<TokenType>(
            {TokenType::NumberLiteral, TokenType::Operator, TokenType::Symbol,
             TokenType::OpenParen, TokenType::Symbol, TokenType::Comma,
             TokenType::NumberLiteral, TokenType::CloseParen}),
        "token types match");
    CHECK_SHOW(msgs.empty(), msgs, ctx);

    SUBCASE("ast") {
      auto tree = ast::ast(toks, ctx, ast::parseValue);
      CHECK_SHOW(msgs.empty(), msgs, ctx);
      REQUIRE(tree);
      CHECK(tree->value.type == +TokenType::Operator);
      REQUIRE(tree->children.size() == 2);
      const auto &l32 = tree->children[0];
      CHECK(l32.value.type == +TokenType::NumberLiteral);
      REQUIRE(l32.children.size() == 0);
      const auto &call = tree->children[1];
      CHECK(call.value.type == +TokenType::Symbol);
      REQUIRE(call.children.size() == 2);
      const auto &args = call.children;
      REQUIRE(args.size() == 2);
      CHECK(args[0].value.type == +TokenType::Symbol);
      CHECK(args[1].value.type == +TokenType::NumberLiteral);
      // TODO: parse
    }
  }
}

TEST_CASE("simple expressions with parenthesis") {
  Messages msgs;
  Context ctx = {msgs, "(32 * var)", "<filename>"};

  SUBCASE("tokenize") {
    Tokens toks = lex(ctx);
    std::vector<TokenType> tokTypes = {};
    for (const auto &tok : toks) {
      tokTypes.push_back(tok.type);
    }
    CHECK_ALL_MESSAGE(
        tokTypes,
        std::vector({+TokenType::OpenParen, +TokenType::NumberLiteral,
                     +TokenType::Operator, +TokenType::Symbol,
                     +TokenType::CloseParen}),
        "token types match");
    CHECK_SHOW(msgs.empty(), msgs, ctx);

    SUBCASE("ast") {
      auto tree = ast::ast(toks, ctx, ast::parseValue);
      CHECK_SHOW(msgs.empty(), msgs, ctx);
      const auto t = "\n" + show(tree, ctx);
      INFO(t);
      REQUIRE(tree);
      CHECK(tree->value.type == +TokenType::Operator);
      REQUIRE(tree->children.size() == 2);
      const auto &operands = tree->children;
      CHECK(operands[0].value.type == +TokenType::NumberLiteral);
      CHECK(operands[1].value.type == +TokenType::Symbol);
    }
  }
}

TEST_CASE("simple expressions using associativity") {
  Messages msgs;
  Context ctx = {msgs, "a * b * c", "<filename>"};

  SUBCASE("tokenize") {
    Tokens toks = lex(ctx);
    std::vector<TokenType> tokTypes = {};
    for (const auto &tok : toks) {
      tokTypes.push_back(tok.type);
    }
    CHECK_ALL_MESSAGE(tokTypes,
                      std::vector({
                          +TokenType::Symbol,
                          +TokenType::Operator,
                          +TokenType::Symbol,
                          +TokenType::Operator,
                          +TokenType::Symbol,
                      }),
                      "token types match");
    CHECK_SHOW(msgs.empty(), msgs, ctx);

    SUBCASE("ast") {
      auto tree = ast::ast(toks, ctx, ast::parseValue);
      CHECK_SHOW(msgs.empty(), msgs, ctx);
      const auto t = "\n" + show(tree, ctx);
      INFO(t);
      REQUIRE(tree);
      CHECK(tree->value.type == +TokenType::Operator);
      const auto &operands = tree->children;
      REQUIRE(operands.size() == 2);
      CHECK(operands[0].value.type == +TokenType::Operator);
      CHECK(operands[1].value.type == +TokenType::Symbol);
      const auto subs = operands[0].children;
      REQUIRE(subs.size() == 2);
      CHECK(subs[0].value.type == +TokenType::Symbol);
      CHECK(subs[1].value.type == +TokenType::Symbol);
    }
  }
}

TEST_CASE("simple expressions using right associativity") {
  Messages msgs;
  Context ctx = {msgs, "a ? b ? c", "<filename>"};

  SUBCASE("tokenize") {
    Tokens toks = lex(ctx);
    std::vector<TokenType> tokTypes = {};
    for (const auto &tok : toks) {
      tokTypes.push_back(tok.type);
    }
    CHECK_ALL_MESSAGE(tokTypes,
                      std::vector({
                          +TokenType::Symbol,
                          +TokenType::QuestionMark,
                          +TokenType::Symbol,
                          +TokenType::QuestionMark,
                          +TokenType::Symbol,
                      }),
                      "token types match");
    CHECK_SHOW(msgs.empty(), msgs, ctx);

    SUBCASE("ast") {
      auto tree = ast::ast(toks, ctx, ast::parseValue);
      CHECK_SHOW(msgs.empty(), msgs, ctx);
      const auto t = "\n" + show(tree, ctx);
      INFO(t);
      REQUIRE(tree);
      CHECK(tree->value.type == +TokenType::QuestionMark);
      const auto &operands = tree->children;
      REQUIRE(operands.size() == 2);
      CHECK(operands[0].value.type == +TokenType::Symbol);
      CHECK(operands[1].value.type == +TokenType::QuestionMark);
      const auto subs = operands[1].children;
      REQUIRE(subs.size() == 2);
      CHECK(subs[0].value.type == +TokenType::Symbol);
      CHECK(subs[1].value.type == +TokenType::Symbol);
    }
  }
}

TEST_CASE("simple tuple") {
  Messages msgs;
  Context ctx = {msgs, "(32, var)", "<filename>"};

  SUBCASE("tokenize") {
    Tokens toks = lex(ctx);
    std::vector<TokenType> tokTypes = {};
    for (const auto &tok : toks) {
      tokTypes.push_back(tok.type);
    }
    CHECK_ALL_MESSAGE(tokTypes,
                      std::vector({+TokenType::OpenParen,
                                   +TokenType::NumberLiteral, +TokenType::Comma,
                                   +TokenType::Symbol, +TokenType::CloseParen}),
                      "token types match");
    CHECK_SHOW(msgs.empty(), msgs, ctx);

    SUBCASE("ast") {
      auto tree = ast::ast(toks, ctx, ast::parseValue);
      CHECK_SHOW(msgs.empty(), msgs, ctx);
      REQUIRE(tree);
      CHECK(tree->value.type == +TokenType::OpenParen);
      REQUIRE(tree->children.size() == 2);
      const auto &operands = tree->children;
      CHECK(operands[0].value.type == +TokenType::NumberLiteral);
      CHECK(operands[1].value.type == +TokenType::Symbol);
    }
  }
}

TEST_CASE("simple expressions with guards") {
  Messages msgs;
  Context ctx = {msgs, "{a, b-|del(a)|-b}", "<filename>"};

  SUBCASE("tokenize") {
    Tokens toks = lex(ctx);
    std::vector<TokenType> tokTypes = {};
    for (const auto &tok : toks) {
      tokTypes.push_back(tok.type);
    }
    CHECK_ALL_MESSAGE(tokTypes,
                      std::vector({+TokenType::OpenBrace, +TokenType::Symbol,
                                   +TokenType::Comma, +TokenType::Symbol,
                                   +TokenType::PreCond, +TokenType::Symbol,
                                   +TokenType::OpenParen, +TokenType::Symbol,
                                   +TokenType::CloseParen, +TokenType::PostCond,
                                   +TokenType::Symbol, +TokenType::CloseBrace}),
                      "token types match");
    CHECK_SHOW(msgs.empty(), msgs, ctx);

    SUBCASE("ast") {
      auto tree = ast::ast(toks, ctx, ast::parseValue);
      CHECK_SHOW(msgs.empty(), msgs, ctx);
      REQUIRE(tree);
      CHECK(tree->value.type == +TokenType::PreCond);
      REQUIRE(tree->children.size() == 3);
      const auto &pre_expr_1 = tree->children[0];
      CHECK(pre_expr_1.value.type == +TokenType::Symbol);
      const auto &pre_expr_2 = tree->children[1];
      CHECK(pre_expr_2.value.type == +TokenType::Symbol);

      const auto &expr_post = tree->children[2];
      const auto t = "\n" + show(expr_post, ctx);
      INFO(t);
      CHECK(expr_post.value.type == +TokenType::PostCond);
      REQUIRE(expr_post.children.size() == 2);

      const auto &expr_body = expr_post.children[0];
      CHECK(expr_body.value.type == +TokenType::Symbol);
      REQUIRE(expr_body.children.size() == 1);

      const auto &expr_arg = expr_body.children[0];
      CHECK(expr_arg.value.type == +TokenType::Symbol);
      REQUIRE(expr_arg.children.size() == 0);

      const auto &post_root = expr_post.children[1];
      REQUIRE(post_root.children.size() == 0);
      CHECK(post_root.value.type == +TokenType::Symbol);
    }
  }
}

TEST_CASE("definition of two") {
  Messages msgs;
  Context ctx = {msgs, "two() = 2", "<filename>"};

  SUBCASE("tokenize") {
    Tokens toks = lex(ctx);
    CHECK_SHOW(msgs.empty(), msgs, ctx);

    SUBCASE("ast") {
      auto tree = ast::ast(toks, ctx, ast::parseDefinition);
      CHECK_SHOW(msgs.empty(), msgs, ctx);
      REQUIRE(tree);
      SUBCASE("parse") {
        parser::ParserContext p_ctx(std::move(ctx));
        auto o_def = parser::parse<std::optional<Definition>>(
            *tree, p_ctx, parser::parseDefinition);
        CHECK_SHOW(msgs.empty(), msgs, ctx);
        REQUIRE(o_def);
        const auto def = *o_def;
        CHECK(def.name == "two");
        REQUIRE(def.args.empty());

        SUBCASE("symbol table add symbol and lookup") {
          parser::SymbolTable syms;
          syms.addSymbol({"foo", "two"}, def);
          SUBCASE("lookup name in same scope") {
            auto res1 = syms.lookup({}, {"foo", "two"});
            CHECK_MESSAGE(res1 == def,
                          "1 lookup should find stored definition");
            auto res2 = syms.lookup({"foo"}, {"two"});
            CHECK_MESSAGE(res2 == def,
                          "2 lookup should find stored definition");
            auto res3 = syms.lookup({"foo", "two"}, {});
            CHECK_MESSAGE(res3 == def,
                          "3 lookup should find stored definition");
          }
          SUBCASE("lookup different name in same scope") {
            auto res1 = syms.lookup({}, {"foo", "three"});
            CHECK_MESSAGE(!res1, "1 lookup should not find stored definition");
            auto res2 = syms.lookup({"foo"}, {"three"});
            CHECK_MESSAGE(!res2, "2 lookup should not find stored definition");
            auto res3 = syms.lookup({"foo", "three"}, {});
            CHECK_MESSAGE(!res3, "3 lookup should not find stored definition");
          }
          SUBCASE("lookup name in different scope") {
            auto res1 = syms.lookup({}, {"foo", "b", "two"});
            CHECK_MESSAGE(!res1, "1 lookup should not find stored definition");
            auto res2 = syms.lookup({"foo"}, {"b", "two"});
            CHECK_MESSAGE(!res2, "2 lookup should not find stored definition");
            auto res3 = syms.lookup({"foo", "b", "two"}, {});
            CHECK_MESSAGE(!res3, "3 lookup should not find stored definition");
          }
          SUBCASE("lookup name in outer scope") {
            auto res1 = syms.lookup({"foo", "b"}, {"two"});
            CHECK_MESSAGE(res1 == def,
                          "1 lookup should find stored definition");
            auto res2 = syms.lookup({"foo", "b", "two"}, {"two"});
            CHECK_MESSAGE(res2 == def,
                          "2 lookup should find stored definition");
          }
        }

        SUBCASE("symbol table built by parser") {
          const auto &symbols = p_ctx.symbols;

          std::vector<Path> pths = {};
          symbols.forAll([&pths](auto &pth, auto &) { pths.push_back(pth); });

          REQUIRE_MESSAGE(pths.size() == 1, "expect only a single definition");
          CHECK_ALL_MESSAGE(pths[0], std::vector({"", "two"}),
                            "expected definition of 'two'");
        }
      }
    }
  }
}

TEST_CASE("small function containing calls") {
  Messages msgs;
  Context ctx = {
      msgs, "nand(a, b) = sequence(And(a, b, c),Free(a),\nFree(b),\nNot(c, c))",
      "<filename>"};

  SUBCASE("tokenize") {
    Tokens toks = lex(ctx);
    CHECK_SHOW(msgs.empty(), msgs, ctx);

    SUBCASE("ast") {
      auto tree = ast::ast(toks, ctx, ast::parseDefinition);
      CHECK_SHOW(msgs.empty(), msgs, ctx);
      REQUIRE(tree);
      SUBCASE("parse") {
        parser::ParserContext p_ctx(std::move(ctx));
        auto o_def = parser::parse<std::optional<Definition>>(
            *tree, p_ctx, parser::parseDefinition);
        CHECK_SHOW(msgs.empty(), msgs, ctx);
        REQUIRE(o_def);
        const auto def = *o_def;
        CHECK(def.name == "nand");
        REQUIRE(def.args.size() == 2);
        CHECK(def.args[0].name == "a");
        CHECK(def.args[1].name == "b");
      }
    }
  }
}

TEST_CASE("small function containing a parenthesized expression") {
  Messages msgs;
  Context ctx = {
      msgs,
      "nand5(b) = sequence(And((1,0,1), b, c),Free(a),\nFree(b),\nNot(c, c))",
      "<filename>"};

  SUBCASE("tokenize") {
    Tokens toks = lex(ctx);
    CHECK_SHOW(msgs.empty(), msgs, ctx);

    SUBCASE("ast") {
      auto tree = ast::ast(toks, ctx, ast::parseValue);
      CHECK_SHOW(msgs.empty(), msgs, ctx);
      REQUIRE(tree);
      SUBCASE("parse") {
        parser::ParserContext p_ctx(std::move(ctx));
        std::optional<Definition> def =
            parser::parse<std::optional<Definition>>(*tree, p_ctx,
                                                     parser::parseDefinition);
        CHECK_SHOW(msgs.empty(), msgs, ctx);
        CHECK(def);
        CHECK(def->name == "nand5");
        REQUIRE(def->args.size() == 1);
        CHECK(def->args[0].name == "b");
      }
    }
  }
}

TEST_CASE("small function definition without a parenthesized argument") {
  Messages msgs;
  Context ctx = {msgs, "a=b(c,d)", "<filename>"};

  SUBCASE("tokenize") {
    Tokens toks = lex(ctx);
    CHECK_SHOW(msgs.empty(), msgs, ctx);

    SUBCASE("ast") {
      auto tree = ast::ast(toks, ctx, ast::parseValue);
      CHECK_SHOW(msgs.empty(), msgs, ctx);
      REQUIRE(tree);
      SUBCASE("parse") {
        parser::ParserContext p_ctx(std::move(ctx));
        auto opdef = parser::parse<std::optional<Definition>>(
            *tree, p_ctx, parser::parseDefinition);
        CHECK_SHOW(msgs.empty(), msgs, ctx);
        REQUIRE(opdef);
        Definition def = *opdef;
        CHECK(def.name == "a");
        CHECK(def.args.size() == 0);
        REQUIRE(def.value);
        Value value = *def.value;
        CHECK(value.name == "b");
        REQUIRE(value.args.size() == 2);
        CHECK(value.args[0].name == "#0");
        REQUIRE(value.args[0].value);
        CHECK(value.args[0].value->name == "c");
        CHECK(value.args[1].name == "#1");
        REQUIRE(value.args[1].value);
        CHECK(value.args[1].value->name == "d");
      }
    }
  }
}

TEST_CASE("small function definition with a parenthesized argument") {
  Messages msgs;
  Context ctx = {msgs, "a=b(c,(d))", "<filename>"};

  SUBCASE("tokenize") {
    Tokens toks = lex(ctx);
    CHECK_SHOW(msgs.empty(), msgs, ctx);

    SUBCASE("ast") {
      auto tree = ast::ast(toks, ctx, ast::parseValue);
      CHECK_SHOW(msgs.empty(), msgs, ctx);
      REQUIRE(tree);
      SUBCASE("parse") {
        parser::ParserContext p_ctx(std::move(ctx));
        auto opdef = parser::parse<std::optional<Definition>>(
            *tree, p_ctx, parser::parseDefinition);
        CHECK_SHOW(msgs.empty(), msgs, ctx);
        REQUIRE(opdef);
        Definition def = *opdef;
        CHECK(def.name == "a");
        CHECK(def.args.size() == 0);
        REQUIRE(def.value);
        Value value = *def.value;
        CHECK(value.name == "b");
        REQUIRE(value.args.size() == 2);
        CHECK(value.args[0].name == "#0");
        REQUIRE(value.args[0].value);
        CHECK(value.args[0].value->name == "c");
        CHECK(value.args[1].name == "#1");
        REQUIRE(value.args[1].value);
        CHECK(value.args[1].value->name == "d");
      }
    }
  }
}

TEST_CASE("tuples") {
  Messages msgs;
  Context ctx = {msgs, "(a, b)", "<filename>"};

  SUBCASE("tokenize") {
    Tokens toks = lex(ctx);
    CHECK_SHOW(msgs.empty(), msgs, ctx);

    SUBCASE("ast") {
      auto tree = ast::ast(toks, ctx, ast::parseValue);
      CHECK_SHOW(msgs.empty(), msgs, ctx);
      SUBCASE("parse") {
        parser::ParserContext p_ctx(std::move(ctx));
        auto o_val = parser::parse<std::optional<Value>>(*tree, p_ctx,
                                                         parser::parseValue);
        CHECK_SHOW(msgs.empty(), msgs, ctx);
        REQUIRE(o_val);
        Value val = *o_val;
        CHECK(val.name == "(");
        const auto args = val.args;
        CHECK(args.size() == 2);
        CHECK(args[0].name == "#0");
        CHECK(args[1].name == "#1");
        REQUIRE(args[0].value);
        CHECK(args[0].value->name == "a");
        REQUIRE(args[1].value);
        CHECK(args[1].value->name == "b");
      }
    }
  }
}

TEST_CASE("nested tuples") {
  Messages msgs;
  Context ctx = {msgs, "((a, b, c), (d, (e, f)))", "<filename>"};

  SUBCASE("tokenize") {
    Tokens toks = lex(ctx);
    CHECK_SHOW(msgs.empty(), msgs, ctx);

    SUBCASE("ast") {
      auto tree = ast::ast(toks, ctx, ast::parseValue);
      CHECK_SHOW(msgs.empty(), msgs, ctx);
      SUBCASE("parse") {
        parser::ParserContext p_ctx(std::move(ctx));
        auto o_val = parser::parse<std::optional<Value>>(*tree, p_ctx,
                                                         parser::parseValue);
        CHECK_SHOW(msgs.empty(), msgs, ctx);
        REQUIRE(o_val);
        Value val = *o_val;
        CHECK(val.name == "(");
        const auto args = val.args;
        CHECK(args.size() == 2);
        CHECK(args[0].name == "#0");
        // LEFT: (a,b,c)
        REQUIRE(args[0].value);
        const auto left = *args[0].value;
        CHECK(left.name == "(");
        REQUIRE(left.args.size() == 3);
        CHECK(left.args[0].name == "#0");
        CHECK(left.args[1].name == "#1");
        CHECK(left.args[2].name == "#2");
        REQUIRE(left.args[0].value);
        CHECK(left.args[0].value->name == "a");
        REQUIRE(left.args[1].value);
        CHECK(left.args[1].value->name == "b");
        REQUIRE(left.args[2].value);
        CHECK(left.args[2].value->name == "c");
        // RIGHT: (d,(e,f))
        REQUIRE(args[1].value);
        const auto right = *args[1].value;
        CHECK(right.name == "(");
        REQUIRE(right.args.size() == 2);
        CHECK(right.args[0].name == "#0");
        REQUIRE(right.args[0].value);
        CHECK(right.args[0].value->name == "d");
        // RIGHT_RIGHT: (e,f)
        REQUIRE(right.args[1].value);
        const auto right_right = *right.args[1].value;
        CHECK(right_right.name == "(");
        REQUIRE(right_right.args.size() == 2);
        CHECK(right_right.args[0].name == "#0");
        CHECK(right_right.args[1].name == "#1");
        REQUIRE(right_right.args[0].value);
        CHECK(right_right.args[0].value->name == "e");
        REQUIRE(right_right.args[1].value);
        CHECK(right_right.args[1].value->name == "f");
      }
    }
  }
}

TEST_CASE("function with default arguments") {
  Messages msgs;
  Context ctx = {msgs, "interp(a, b, first=0.5) = a*first+b*(1-first)",
                 "<filename>"};

  SUBCASE("tokenize") {
    Tokens toks = lex(ctx);
    CHECK_SHOW(msgs.empty(), msgs, ctx);

    SUBCASE("ast") {
      auto tree = ast::ast(toks, ctx, ast::parseDefinition);
      CHECK_SHOW(msgs.empty(), msgs, ctx);
      REQUIRE(tree);
      SUBCASE("parse") {
        parser::ParserContext p_ctx(std::move(ctx));
        auto o_def = parser::parse<std::optional<Definition>>(
            *tree, p_ctx, parser::parseDefinition);
        CHECK_SHOW(msgs.empty(), msgs, ctx);
        REQUIRE(o_def);
        const auto def = *o_def;
        CHECK(def.name == "interp");
        REQUIRE_MESSAGE(def.args.size() == 3, "correct number of args");
        CHECK(def.args[0].name == "a");
        CHECK(def.args[1].name == "b");
        CHECK(def.args[2].name == "first");

        SUBCASE("symbol table built by parser") {
          const auto &symbols = p_ctx.symbols;

          std::vector<Path> pths = {};
          symbols.forAll([&pths](auto &pth, auto &) { pths.push_back(pth); });

          REQUIRE(pths.size() == 4);

          CHECK_ALL_MESSAGE(pths[0], std::vector({"", "interp", "a"}),
                            "expect definition of '/interp/a'");
          CHECK_ALL_MESSAGE(pths[1], std::vector({"", "interp", "b"}),
                            "expect definition of '/interp/b'");
          CHECK_ALL_MESSAGE(pths[2], std::vector({"", "interp", "first"}),
                            "expect definition of '/interp/first'");
          CHECK_ALL_MESSAGE(pths[3], std::vector({"", "interp"}),
                            "expect definition of '/interp'");
        }
      }
    }
  }
}

TEST_CASE("function with pre+post definitions") {
  Messages msgs;
  Context ctx = {msgs, "increment(x) = {x=y-|x=x+1|-x=y+1}", "<filename>"};

  SUBCASE("tokenize") {
    Tokens toks = lex(ctx);
    CHECK_SHOW(msgs.empty(), msgs, ctx);

    SUBCASE("ast") {
      auto tree = ast::ast(toks, ctx, ast::parseDefinition);
      CHECK_SHOW(msgs.empty(), msgs, ctx);
      REQUIRE(tree);
      SUBCASE("parse") {
        parser::ParserContext p_ctx(std::move(ctx));
        auto o_def = parser::parse<std::optional<Definition>>(
            *tree, p_ctx, parser::parseDefinition);
        const auto &symbols = p_ctx.symbols;

        std::vector<Path> pths = {};
        symbols.forAll([&pths](auto &pth, auto &) { pths.push_back(pth); });

        CHECK_SHOW(msgs.empty(), msgs, ctx);
        REQUIRE(o_def);
        const auto def = *o_def;
        std::string defs = show(def);
        CHECK(def.name == "increment");
        REQUIRE_MESSAGE(def.args.size() == 1, "correct number of args");
        CHECK(def.args[0].name == "x");

        SUBCASE("symbol table built by parser") {
          REQUIRE(pths.size() > 0);
          CHECK_ALL_MESSAGE(pths[0], std::vector({"", "increment", "x"}),
                            "expect definition of '/increment/x'");

          REQUIRE(pths.size() > 1);
          CHECK_ALL_MESSAGE(pths[1],
                            std::vector({"", "increment", "#pre", "x"}),
                            "expect definition of '/increment/#pre/x'");

          REQUIRE(pths.size() > 2);
          CHECK_ALL_MESSAGE(pths[2],
                            std::vector({"", "increment", "#pre", "#0", "x"}),
                            "expect definition of '/increment/#pre/#0/x'");

          REQUIRE(pths.size() > 3);
          std::string s = show(pths[3], 0, "/");
          CHECK_ALL_MESSAGE(
              pths[3],
              std::vector({"", "increment", "#pre", "#0", "#post", "x"}),
              "expect definition of '/increment/#pre/#0/#post/x'");

          REQUIRE(pths.size() > 4);
          CHECK_ALL_MESSAGE(pths[4], std::vector({"", "increment"}),
                            "expect definition of '/increment'");
        }
      }
    }
  }
}

TEST_CASE("function with post definitions") {
  Messages msgs;
  Context ctx = {msgs, "increment(x) = {y=x+1|-y>x}", "<filename>"};

  SUBCASE("tokenize") {
    Tokens toks = lex(ctx);
    CHECK_SHOW(msgs.empty(), msgs, ctx);

    SUBCASE("ast") {
      auto tree = ast::ast(toks, ctx, ast::parseDefinition);
      CHECK_SHOW(msgs.empty(), msgs, ctx);
      REQUIRE(tree);
      std::string treeS = show(*tree, ctx);
      SUBCASE("parse") {
        parser::ParserContext p_ctx(std::move(ctx));
        auto o_def = parser::parse<std::optional<Definition>>(
            *tree, p_ctx, parser::parseDefinition);
        CHECK_SHOW(msgs.empty(), msgs, ctx);
        REQUIRE(o_def);
        const auto def = *o_def;
        std::string defs = show(def);
        CHECK(def.name == "increment");
        REQUIRE_MESSAGE(def.args.size() == 1, "correct number of args");
        CHECK(def.args[0].name == "x");

        SUBCASE("symbol table built by parser") {
          const auto &symbols = p_ctx.symbols;

          std::vector<Path> pths = {};
          symbols.forAll([&pths](auto &pth, auto &) { pths.push_back(pth); });

          REQUIRE(pths.size() == 3);

          CHECK_ALL_MESSAGE(pths[0], std::vector({"", "increment", "x"}),
                            "expected definition of '/increment/x'");
          CHECK_ALL_MESSAGE(pths[1], std::vector({"", "increment", "y"}),
                            "expected definition of '/increment/y'");
          CHECK_ALL_MESSAGE(pths[2], std::vector({"", "increment"}),
                            "expected definition of '/increment'");
        }
      }
    }
  }
}

TEST_CASE("function multiple pre sections") {
  Messages msgs;
  Context ctx = {msgs, "is_even(x) = {x=0-|1}?{x=1-|0}?{y=x-2-|is_even(y)}",
                 "<filename>"};

  SUBCASE("tokenize") {
    Tokens toks = lex(ctx);
    CHECK_SHOW(msgs.empty(), msgs, ctx);

    SUBCASE("ast") {
      auto tree = ast::ast(toks, ctx, ast::parseDefinition);
      CHECK_SHOW(msgs.empty(), msgs, ctx);
      REQUIRE(tree);
      SUBCASE("parse") {
        parser::ParserContext p_ctx(std::move(ctx));
        auto o_def = parser::parse<std::optional<Definition>>(
            *tree, p_ctx, parser::parseDefinition);
        CHECK_SHOW(msgs.empty(), msgs, ctx);
        REQUIRE(o_def);
        const auto def = *o_def;
        std::string defs = show(def);
        CHECK(def.name == "is_even");
        REQUIRE_MESSAGE(def.args.size() == 1, "correct number of args");
        CHECK(def.args[0].name == "x");

        SUBCASE("symbol table built by parser") {
          const auto &symbols = p_ctx.symbols;

          std::vector<Path> pths = {};
          symbols.forAll([&pths](auto &pth, auto &) { pths.push_back(pth); });

          REQUIRE(pths.size() == 5);

          CHECK_ALL_MESSAGE(pths[0], std::vector({"", "is_even", "x"}),
                            "expect definition of '/is_even/x'");

          CHECK_ALL_MESSAGE(
              pths[1],
              std::vector<std::string>({"", "is_even", "#0", "#pre", "x"}),
              "expect definition of '/is_even/#0/#pre/x'");
          CHECK_ALL_MESSAGE(pths[2],
                            std::vector<std::string>(
                                {"", "is_even", "#1", "#0", "#pre", "x"}),
                            "expect definition of '/is_even/#1/#0/#pre/x'");
          CHECK_ALL_MESSAGE(pths[3],
                            std::vector<std::string>(
                                {"", "is_even", "#1", "#1", "#pre", "y"}),
                            "expect definition of '/is_even/#1/#1/#pre/y'");
          CHECK_ALL_MESSAGE(pths[4], std::vector<std::string>({"", "is_even"}),
                            "expect definition of '/is_even'");
        }
      }
    }
  }
}
