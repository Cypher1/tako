#pragma once
#ifndef TOSTRING_H
#define TOSTRING_H

#include <string>

#include "ast.h"

std::string toString(const Location& loc, const std::string contents);
std::string toString(const Token& tok, const std::string& contents);
std::string toString(const Message& msg, const std::string& contents);
std::string toString(const Tree<Token>& tree, const std::string& contents, int depth=0);

#endif // #ifndef TOSTRING_H
