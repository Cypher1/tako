#pragma once
#ifndef PARSER_H
#define PARSER_H

#include "ast.h"

Module parse(const Tree<Token>& tree, Context &ctx);

#endif // #ifndef PARSER_H
