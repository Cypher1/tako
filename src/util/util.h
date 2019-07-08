#pragma once
#ifndef UTIL_H
#define UTIL_H

#include <vector>
#include <string>

#include "../lib/enums.h"

using Position = unsigned int;
using Offset = unsigned int;

struct Location {
  Position start=0;
  Offset length=0;
  std::string file="";
};

const Location errorLocation = {0, 0, "<file>?"};

BETTER_ENUM(
    PassStep,
    char,
    Init,
    Lex,
    Ast,
    Parse,
    Check,
    Optimize,
    CodeGen,
    Final
    );

BETTER_ENUM(
    MessageType,
    char,
    Info,
    Warning,
    Error,
    Failure
    );

struct Message {
  PassStep pass;
  MessageType type;
  std::string msg;
  Location loc;
};

using Messages = std::vector<Message>;

template<typename T>
struct Tree;

template<typename T>
using Forest = std::vector<Tree<T>>;

template<typename T>
struct Tree {
  T value;
  Forest<T> children;

  Tree(T value): Tree(value, {}) {};
  Tree(T value, Forest<T> children): value{value}, children{children} {}
};


#endif // #ifndef UTIL_H
