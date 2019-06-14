#pragma once
#ifndef AST_H
#define AST_H

#include <vector>
#include <string>

#include "../lib/enums.h"

BETTER_ENUM(
    TokenType,
    char,
    OpenParen,
    CloseParen,
    OpenBrace,
    CloseBrace,
    OpenBracket,
    CloseBracket,
    PreCond,
    PostCond,
    Definition,
    Dot,
    SingleQuote,
    DoubleQuote,
    BackQuote,
    NumberLiteral,
    Operator,
    Symbol,
    Error
    );

using Position = unsigned int;
using Offset = unsigned int;

struct Location {
  Position start;
  Offset length;
  std::string file;
};

struct Token {
  TokenType type;
  Location loc;
};

using Tokens = std::vector<Token>;

BETTER_ENUM(
    MessageType,
    char,
    Info,
    Warning,
    Error,
    Failure
    );

struct Message {
  MessageType type;
  std::string msg;
  Location loc;
};

using Messages = std::vector<Message>;

template<typename T>
struct Result {
  T value;
  Messages msgs;
};

template<typename T>
struct Tree {
  T value;
  std::vector<Tree<T>> children;
};

Result<Tokens> lex(std::string filename, std::string content);
Result<Tree<Token>> ast(Result<Tokens> toks, const std::string content, const std::string filename);

#endif // #ifndef AST_H
