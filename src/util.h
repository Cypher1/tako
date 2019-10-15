#ifndef UTIL_H
#define UTIL_H

#include <string>
#include <vector>

#include "enums.h"

struct Config {
  int width = 80;
  int height = 80;
};

using Position = unsigned int;
using Offset = unsigned int;

struct Location {
  Position start = 0;
  Offset length = 0;
  std::string file = "";
};

bool operator<(const Location &a, const Location &b);

const Location errorLocation = {0, 0, "<eof>"};

BETTER_ENUM(PassStep, char, Init, Lex, Ast, Parse, Check, Optimize, CodeGen,
            Final);

BETTER_ENUM(MessageType, char,
            // Used for potential issues with user code.
            Info, Warning, Error, Failure,
            // Used for (recoverable) errors in the compiler itself.
            // Unrecoverable errors should throw.
            InternalError);

struct Message {
  PassStep pass;
  MessageType type;
  std::string msg;
  Location loc;
};

using Messages = std::vector<Message>;

template <typename T> struct Tree;

template <typename T> using Forest = std::vector<Tree<T>>;

template <typename T> struct Tree {
  T value;
  Forest<T> children;

  Tree(T value) : Tree(value, {}){};
  Tree(T value, Forest<T> children) : value{value}, children{children} {}
};

#endif // #ifndef UTIL_H
