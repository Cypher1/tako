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
    CHECK_FALSE(toks.empty());
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
      const std::string toksS = show(toks, ctx);
      // INFO(toksS);
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
    REQUIRE(toks.size() == 5);
    CHECK(toks[0].type == +TokenType::Symbol);
    CHECK(toks[1].type == +TokenType::WhiteSpace);
    CHECK(toks[2].type == +TokenType::Operator);
    CHECK(toks[3].type == +TokenType::WhiteSpace);
    CHECK(toks[4].type == +TokenType::NumberLiteral);
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

TEST_CASE("simple expressions") {
  Messages msgs;
  Context ctx = {msgs, "32 * var", "<filename>"};

  SUBCASE("tokenize") {
    Tokens toks = lex(ctx);
    CHECK_SHOW(msgs.empty(), msgs, ctx);
    REQUIRE(toks.size() == 5);
    CHECK(toks[0].type == +TokenType::NumberLiteral);
    CHECK(toks[1].type == +TokenType::WhiteSpace);
    CHECK(toks[2].type == +TokenType::Operator);
    CHECK(toks[3].type == +TokenType::WhiteSpace);
    CHECK(toks[4].type == +TokenType::Symbol);
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
    REQUIRE(toks.size() == 7);
    CHECK(toks[0].type == +TokenType::NumberLiteral);
    CHECK(toks[1].type == +TokenType::WhiteSpace);
    CHECK(toks[2].type == +TokenType::Operator);
    CHECK(toks[3].type == +TokenType::WhiteSpace);
    CHECK(toks[4].type == +TokenType::Symbol);
    CHECK(toks[5].type == +TokenType::OpenParen);
    CHECK(toks[6].type == +TokenType::CloseParen);

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
    REQUIRE(toks.size() == 11);
    CHECK(toks[0].type == +TokenType::NumberLiteral);
    CHECK(toks[1].type == +TokenType::WhiteSpace);
    CHECK(toks[2].type == +TokenType::Operator);
    CHECK(toks[3].type == +TokenType::WhiteSpace);
    CHECK(toks[4].type == +TokenType::Symbol);
    CHECK(toks[5].type == +TokenType::OpenParen);
    CHECK(toks[6].type == +TokenType::Symbol);
    CHECK(toks[7].type == +TokenType::Comma);
    CHECK(toks[8].type == +TokenType::WhiteSpace);
    CHECK(toks[9].type == +TokenType::NumberLiteral);
    CHECK(toks[10].type == +TokenType::CloseParen);
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
    }
  }
}

TEST_CASE("simple expressions with parenthesis") {
  Messages msgs;
  Context ctx = {msgs, "(32 * var)", "<filename>"};

  SUBCASE("tokenize") {
    Tokens toks = lex(ctx);
    REQUIRE(toks.size() == 7);
    CHECK(toks[0].type == +TokenType::OpenParen);
    CHECK(toks[1].type == +TokenType::NumberLiteral);
    CHECK(toks[2].type == +TokenType::WhiteSpace);
    CHECK(toks[3].type == +TokenType::Operator);
    CHECK(toks[4].type == +TokenType::WhiteSpace);
    CHECK(toks[5].type == +TokenType::Symbol);
    CHECK(toks[6].type == +TokenType::CloseParen);
    CHECK_SHOW(msgs.empty(), msgs, ctx);

    SUBCASE("ast") {
      auto tree = ast::ast(toks, ctx, ast::parseValue);
      CHECK_SHOW(msgs.empty(), msgs, ctx);
      REQUIRE(tree);
      CHECK(tree->value.type == +TokenType::OpenParen);
      REQUIRE(tree->children.size() == 1);
      const auto &expr_root = tree->children[0];
      CHECK(expr_root.value.type == +TokenType::Operator);
      REQUIRE(expr_root.children.size() == 2);
      const auto &operands = expr_root.children;
      CHECK(operands[0].value.type == +TokenType::NumberLiteral);
      CHECK(operands[1].value.type == +TokenType::Symbol);
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
      std::string treeS = show(*tree, ctx);
      INFO(treeS);
      SUBCASE("parse") {
        parser::ParserContext p_ctx(std::move(ctx));
        auto o_def = parser::parse<std::optional<Definition>>(
            *tree, p_ctx, parser::parseDefinition);
        CHECK_SHOW(msgs.empty(), msgs, ctx);
        REQUIRE(o_def);
        const auto def = *o_def;
        std::string defs = show(def);
        INFO(defs);
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
          symbols.forAll(
              [&pths](auto &pth, auto &def) { pths.push_back(pth); });

          REQUIRE_MESSAGE(pths.size() == 1, "expect only a single definition");
          REQUIRE_MESSAGE(pths[0].size() == 2, "expect definition of 'two'");
          CHECK_MESSAGE(pths[0][0] == "", "expect definition of 'two'");
          CHECK_MESSAGE(pths[0][1] == "two", "expect definition of 'two'");
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
      std::string treeS = show(*tree, ctx);
      INFO(treeS);
      SUBCASE("parse") {
        parser::ParserContext p_ctx(std::move(ctx));
        auto o_def = parser::parse<std::optional<Definition>>(
            *tree, p_ctx, parser::parseDefinition);
        CHECK_SHOW(msgs.empty(), msgs, ctx);
        REQUIRE(o_def);
        const auto def = *o_def;
        std::string defs = show(def);
        INFO(defs);
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
        std::string defs = show(def);
        INFO(defs);
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
        std::string defs = show(def);
        INFO(defs);
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
        std::string valueS = show(val);
        INFO(valueS);
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
        std::string valueS = show(val);
        INFO(valueS);
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
      std::string treeS = show(*tree, ctx);
      INFO(treeS);
      SUBCASE("parse") {
        parser::ParserContext p_ctx(std::move(ctx));
        auto o_def = parser::parse<std::optional<Definition>>(
            *tree, p_ctx, parser::parseDefinition);
        CHECK_SHOW(msgs.empty(), msgs, ctx);
        REQUIRE(o_def);
        const auto def = *o_def;
        std::string defs = show(def);
        INFO(defs);
        CHECK(def.name == "interp");
        REQUIRE_MESSAGE(def.args.size() == 3, "correct number of args");
        CHECK(def.args[0].name == "a");
        CHECK(def.args[1].name == "b");
        CHECK(def.args[2].name == "first");

        SUBCASE("symbol table built by parser") {
          const auto &symbols = p_ctx.symbols;

          std::vector<Path> pths = {};
          symbols.forAll(
              [&pths](auto &pth, auto &def) { pths.push_back(pth); });

          const auto pthsStr = show(pths, 0, "\n");
          INFO(pthsStr);
          REQUIRE(pths.size() == 4);

          REQUIRE_MESSAGE(pths[0].size() == 3,
                          "expect definition of '/interp/a'");
          CHECK(pths[0][0] == "");
          CHECK(pths[0][1] == "interp");
          CHECK(pths[0][2] == "a");

          REQUIRE_MESSAGE(pths[1].size() == 3,
                          "expect definition of '/interp/b'");
          CHECK(pths[1][0] == "");
          CHECK(pths[1][1] == "interp");
          CHECK(pths[1][2] == "b");

          REQUIRE_MESSAGE(pths[2].size() == 3,
                          "expect definition of '/interp/first'");
          CHECK(pths[2][0] == "");
          CHECK(pths[2][1] == "interp");
          CHECK(pths[2][2] == "first");

          REQUIRE_MESSAGE(pths[3].size() == 2,
                          "expect definition of '/interp'");
          CHECK(pths[3][0] == "");
          CHECK(pths[3][1] == "interp");
        }
      }
    }
  }
}
