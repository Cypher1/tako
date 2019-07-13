#define DOCTEST_CONFIG_IMPLEMENT_WITH_MAIN
#include "doctest.h"

#include <iostream>
#include <vector>
#include <algorithm>

#include "../src/lex.h"

TEST_CASE("empty string yields no tokens") {
  Messages msgs;
  Context ctx = {msgs, "", "<filename>"};

  Tokens toks = lex(ctx);
  CHECK(toks.empty());
  CHECK(msgs.empty());
}

TEST_CASE("non-empty string yields tokens") {
  Messages msgs;
  Context ctx = {msgs, " ", "<filename>"};

  Tokens toks = lex(ctx);
  CHECK_FALSE(toks.empty());
  CHECK(msgs.empty());
}

TEST_CASE("can lex a numeric literal") {
  Messages msgs;
  Context ctx = {msgs, "12", "<filename>"};

  Tokens toks = lex(ctx);
  CHECK(toks.size() == 1);
  CHECK(toks[0].type == +TokenType::NumberLiteral);
  CHECK(msgs.empty());
}

TEST_CASE("can lex a variable name") {
  Messages msgs;
  Context ctx = {msgs, "var", "<filename>"};

  Tokens toks = lex(ctx);
  CHECK(toks.size() == 1);
  CHECK(toks[0].type == +TokenType::Symbol);
  CHECK(msgs.empty());
}

TEST_CASE("can tokenize simple expressions") {
  Messages msgs;
  Context ctx = {msgs, "var * 32", "<filename>"};

  Tokens toks = lex(ctx);
  CHECK(toks.size() == 5);
  CHECK(toks[0].type == +TokenType::Symbol);
  CHECK(toks[1].type == +TokenType::WhiteSpace);
  CHECK(toks[2].type == +TokenType::Operator);
  CHECK(toks[3].type == +TokenType::WhiteSpace);
  CHECK(toks[4].type == +TokenType::NumberLiteral);
  CHECK(msgs.empty());
}

TEST_CASE("can tokenize simple expressions") {
  Messages msgs;
  Context ctx = {msgs, "32 * var", "<filename>"};

  Tokens toks = lex(ctx);
  CHECK(toks.size() == 5);
  CHECK(toks[0].type == +TokenType::NumberLiteral);
  CHECK(toks[1].type == +TokenType::WhiteSpace);
  CHECK(toks[2].type == +TokenType::Operator);
  CHECK(toks[3].type == +TokenType::WhiteSpace);
  CHECK(toks[4].type == +TokenType::Symbol);
  CHECK(msgs.empty());
}

TEST_CASE("can tokenize simple expressions") {
  Messages msgs;
  Context ctx = {msgs, "32 * var()", "<filename>"};

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
}
