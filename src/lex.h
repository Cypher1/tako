#ifndef LEX_H
#define LEX_H

#include <string>
#include <vector>

#include "context.h"
#include "enums.h"
#include "util.h"

BETTER_ENUM(TokenType, char, OpenParen, CloseParen, OpenBrace, CloseBrace,
            OpenBracket, CloseBracket, SemiColon, PreCond, PostCond,
            // SingleQuote,
            // DoubleQuote,
            // BackQuote,
            Dot, Comma, WhiteSpace, NumberLiteral, StringLiteral, Operator,
            Symbol, Error);

struct Token {
  TokenType type = TokenType::Error;
  Location loc;
};

using Tokens = std::vector<Token>;

Tokens lex(Context &ctx);

#endif // #ifndef LEX_H
