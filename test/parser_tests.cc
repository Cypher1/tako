#define DOCTEST_CONFIG_IMPLEMENT_WITH_MAIN
#include "doctest.h"

#include <iostream>
#include <vector>
#include <algorithm>

#include "../src/lex.h"

TEST_CASE("empty string yields no tokens") {
  Messages msgs;
  Config config;
  Context ctx = {msgs, "", "<filename>"};

  Tokens toks = lex(ctx);
  CHECK(toks.empty());
  CHECK(msgs.empty());
}

TEST_CASE("non-empty string yields tokens") {
  Messages msgs;
  Config config;
  Context ctx = {msgs, " ", "<filename>"};

  Tokens toks = lex(ctx);
  CHECK_FALSE(toks.empty());
  CHECK(msgs.empty());
}

TEST_CASE("can lex a numeric literal") {
  Messages msgs;
  Config config;
  Context ctx = {msgs, "12", "<filename>"};

  Tokens toks = lex(ctx);
  CHECK_FALSE(toks.empty());
  CHECK(msgs.empty());
}

