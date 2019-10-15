#ifndef EVAL_H
#define EVAL_H

#include <optional>
#include <string>
#include <variant>
#include <vector>

#include "ast.h"
#include "context.h"
#include "enums.h"
#include "util.h"

std::variant<int, std::string> eval(Value val);

#endif // #ifndef EVAL_H
