#pragma once
#ifndef UTIL_H
#define UTIL_H

#include <vector>
#include <string>

#include "../lib/enums.h"

using Position = unsigned int;
using Offset = unsigned int;

struct Location {
  Position start;
  Offset length;
  std::string file;
};

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

#endif // #ifndef UTIL_H
