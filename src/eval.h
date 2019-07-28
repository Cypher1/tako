#ifndef EVAL_H
#define EVAL_H

#include <variant>
#include <optional>
#include <vector>
#include <string>

#include "enums.h"
#include "util.h"
#include "context.h"
#include "ast.h"

std::variant<int, std::string> eval(Value val);

#endif // #ifndef EVAL_H
