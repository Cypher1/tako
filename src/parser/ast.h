#pragma once
#ifndef AST_H
#define AST_H

#include <string>

#include "../lib/enums.h"
#include "../util/util.h"
#include "lex.h"

Tree<Token> ast(Tokens& toks, Messages& msgs, const std::string& content, const std::string& filename);

#endif // #ifndef AST_H
