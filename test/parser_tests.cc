#define DOCTEST_CONFIG_IMPLEMENT_WITH_MAIN
#include "doctest.h"

#include <iostream>
#include <vector>
#include <algorithm>

#include "../src/lex.h"
#include "../src/ast.h"
#include "../src/parser.h"
#include "../src/show.h"

// TODO rapidcheck that all tokens from a lexed 'file' are inside the file.
// i.e. their location is 'in bounds' and the getString matches the input.getString.

TEST_CASE("empty string") {
  Messages msgs;
  Context ctx = {msgs, "", "<filename>"};

  SUBCASE("tokenize yields no tokens") {
    Tokens toks = lex(ctx);
    CHECK_MESSAGE(toks.empty(), toks[0].type);
    CHECK(msgs.empty());
    SUBCASE("ast should be an empty module") {
      Tree<Token> tree = ast(toks, ctx);
      CHECK_MESSAGE(tree.children.empty(), tree.children[0].value.type);
      CHECK(msgs.empty());
    }
  }
}

TEST_CASE("non-empty string") {
  Messages msgs;
  Context ctx = {msgs, " ", "<filename>"};

  SUBCASE("tokenize") {
    Tokens toks = lex(ctx);
    CHECK_FALSE(toks.empty());
    CHECK(msgs.empty());
    SUBCASE("ast") {
      Tree<Token> tree = ast(toks, ctx);
      CHECK_MESSAGE(tree.children.empty(), tree.children[0].value.type);
      CHECK(msgs.empty());
    }
  }
}

TEST_CASE("can lex a numeric literal") {
  Messages msgs;
  Context ctx = {msgs, "12", "<filename>"};

  SUBCASE("tokenize") {
    Tokens toks = lex(ctx);
    CHECK(toks.size() == 1);
    CHECK(toks[0].type == +TokenType::NumberLiteral);
    CHECK(msgs.empty());
    SUBCASE("ast") {
      Tree<Token> tree = ast(toks, ctx);
      CHECK(tree.children.size() == 1);
      CHECK(tree.children[0].value.type == +TokenType::NumberLiteral);
      CHECK_MESSAGE(tree.children[0].children.empty(), tree.children[0].children[0].value.type);
      CHECK(msgs.empty());
    }
  }
}

TEST_CASE("variable name") {
  Messages msgs;
  Context ctx = {msgs, "var", "<filename>"};

  SUBCASE("tokenize") {
    Tokens toks = lex(ctx);
    CHECK(toks.size() == 1);
    CHECK(toks[0].type == +TokenType::Symbol);
    CHECK(msgs.empty());
    SUBCASE("ast") {
      Tree<Token> tree = ast(toks, ctx);
      CHECK(tree.children.size() == 1);
      CHECK(tree.children[0].value.type == +TokenType::Symbol);
      CHECK(tree.children[0].children.size() == 0);
      CHECK(msgs.empty());
    }
  }
}

TEST_CASE("simple expressions") {
  Messages msgs;
  Context ctx = {msgs, "var * 32", "<filename>"};

  SUBCASE("tokenize") {
    Tokens toks = lex(ctx);
    CHECK(toks.size() == 5);
    CHECK(toks[0].type == +TokenType::Symbol);
    CHECK(toks[1].type == +TokenType::WhiteSpace);
    CHECK(toks[2].type == +TokenType::Operator);
    CHECK(toks[3].type == +TokenType::WhiteSpace);
    CHECK(toks[4].type == +TokenType::NumberLiteral);
    CHECK(msgs.empty());
    SUBCASE("ast") {
      Tree<Token> tree = ast(toks, ctx);
      CHECK(msgs.empty());
    }
  }
}

TEST_CASE("simple expressions") {
  Messages msgs;
  Context ctx = {msgs, "32 * var", "<filename>"};

  SUBCASE("tokenize") {
    Tokens toks = lex(ctx);
    CHECK(toks.size() == 5);
    CHECK(toks[0].type == +TokenType::NumberLiteral);
    CHECK(toks[1].type == +TokenType::WhiteSpace);
    CHECK(toks[2].type == +TokenType::Operator);
    CHECK(toks[3].type == +TokenType::WhiteSpace);
    CHECK(toks[4].type == +TokenType::Symbol);
    CHECK(msgs.empty());
    SUBCASE("ast") {
      Tree<Token> tree = ast(toks, ctx);
      CHECK(msgs.empty());
    }
  }
}

TEST_CASE("simple expressions with calls") {
  Messages msgs;
  Context ctx = {msgs, "32 * var()", "<filename>"};

  SUBCASE("tokenize") {
    Tokens toks = lex(ctx);
    CHECK(toks.size() == 7);
    CHECK(toks[0].type == +TokenType::NumberLiteral);
    CHECK(toks[1].type == +TokenType::WhiteSpace);
    CHECK(toks[2].type == +TokenType::Operator);
    CHECK(toks[3].type == +TokenType::WhiteSpace);
    CHECK(toks[4].type == +TokenType::Symbol);
    CHECK(toks[5].type == +TokenType::OpenParen);
    CHECK(toks[6].type == +TokenType::CloseParen);
    CHECK(msgs.empty());

    SUBCASE("ast") {
      Tree<Token> tree = ast(toks, ctx);
      CHECK(msgs.empty());
    }
  }
}

TEST_CASE("simple expressions with parenthesis") {
  Messages msgs;
  Context ctx = {msgs, "(32 * var)", "<filename>"};

  SUBCASE("tokenize") {
    Tokens toks = lex(ctx);
    CHECK(toks.size() == 7);
    CHECK(toks[0].type == +TokenType::OpenParen);
    CHECK(toks[1].type == +TokenType::NumberLiteral);
    CHECK(toks[2].type == +TokenType::WhiteSpace);
    CHECK(toks[3].type == +TokenType::Operator);
    CHECK(toks[4].type == +TokenType::WhiteSpace);
    CHECK(toks[5].type == +TokenType::Symbol);
    CHECK(toks[6].type == +TokenType::CloseParen);
    CHECK(msgs.empty());

    SUBCASE("ast") {
      Tree<Token> tree = ast(toks, ctx);
      CHECK(msgs.empty());
    }
  }
}
