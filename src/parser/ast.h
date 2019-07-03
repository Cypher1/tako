#pragma once
#ifndef AST_H
#define AST_H

#include <string>

#include "../lib/enums.h"
#include "../util/util.h"
#include "../util/context.h"
#include "lex.h"

Tree<Token> ast(Tokens& toks, Context &ctx);

#endif // #ifndef AST_H
