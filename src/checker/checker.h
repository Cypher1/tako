#pragma once
#ifndef CHECKER_H
#define CHECKER_H

#include "../util/util.h"
#include "../util/context.h"
#include "../parser/parser.h"

struct CheckedModule {

};

CheckedModule check(Module module, Context &ctx);
#endif // #ifndef CHECKER_H
