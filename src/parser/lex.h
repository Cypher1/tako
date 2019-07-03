#pragma once
#ifndef LEX_H
#define LEX_H

#include <vector>
#include <string>

#include "../lib/enums.h"
#include "../util/util.h"
#include "../util/context.h"

BETTER_ENUM(
    TokenType,
    char,
    OpenParen,
    CloseParen,
    OpenBrace,
    CloseBrace,
    OpenBracket,
    CloseBracket,
    Declaration,
    SemiColon,
    PreCond,
    PostCond,
    SingleQuote,
    DoubleQuote,
    BackQuote,
    Dot,
    Comma,
    WhiteSpace,
    NumberLiteral,
    Operator,
    Symbol,
    Error
    );

struct Token {
  TokenType type = TokenType::Error;
  Location loc;
};

using Tokens = std::vector<Token>;

Tokens lex(Context &ctx);

#endif // #ifndef LEX_H
