#include <iostream>
#include <fstream>
#include <sstream> //std::stringstream
#include <stdexcept> // TODO: Remove use of exceptions, instead use messages and fallback.
#include <algorithm>
#include <vector>
#include <unordered_map>
#include <sys/ioctl.h>
#include <cstdio>
#include <unistd.h>

#include "takoConfig.h"
#include "parser/ast.h"
#include "parser/parser.h"
#include "parser/toString.h"
#include "checker/checker.h"
#include "arg_parser/arg_parser.h"
#include "util.h"

const std::vector<Arg> args = {
  {'h', "help", "Prints this help message.", ""},
  {'V', "version", "Prints the version number.", ""},
  {'O', "", "Number of optimisation passes.", "level"},
  {'o', "out", "File to write results to.", "file"},
  {'i', "interactive", "Run interpreter.", ""},
  {'s', "step", "Stop after this step.", "last"},
};
void runCompiler(Context &ctx);

void info() {
  std::cerr << "tako - version " << tako_VERSION_MAJOR << "." << tako_VERSION_MINOR << "." << tako_VERSION_PATCH << "\n";
  std::cerr << "A compiler for ergonomic software verification\n";
}

int main(int argc, char* argv[]) {
  std::vector<std::string> targets;
  std::unordered_map<std::string, std::string> values;

  const std::string prog = argv[0];

  try {
    parseArgs(args, 1, argc, argv, targets, values);
  } catch (std::runtime_error er) {
    std::cerr << "Invalid command line argument: " << er.what() << "\n";
    return 1;
  }

  struct winsize w;
  ioctl(STDOUT_FILENO, TIOCGWINSZ, &w);
  height = w.ws_row;
  width = w.ws_col;

  if (argc < 2 || values.find("help") != values.end() || values.find("version") != values.end()){
    info();

    if (argc >= 2 && values.find("help") == values.end()) {
      return 1;
    }
    std::cerr << makeUsage(prog, args);
    return 1;
  }
  std::string out = "%.o";
  const auto out_it = values.find("out");
  if(out_it != values.end()) {
    out = out_it->second;
  }
  const auto last_step_it = values.find("step");
  PassStep last_step = PassStep::Final;
  if(last_step_it != values.end()) {
    const auto opt = PassStep::_from_string_nocase_nothrow(last_step_it->second.c_str());
    if(opt) {
      last_step = *opt;
      std::cerr << "Up to " << last_step << "\n";
    } else {
      std::cerr << "No known pass step named " << last_step_it->second << ".\n";
      return 1;
    }
  }
  Messages msgs;
  for(const auto filename : targets) {
    std::string this_out = out;
    this_out.replace(this_out.find('%'), 1, filename);

    std::ifstream inFile;
    inFile.open(filename);

    // TODO: use the file+stream natively using memmap.
    std::stringstream strStream;
    strStream << inFile.rdbuf();
    const std::string contents = strStream.str();

    std::cerr << "> " << filename << " -> " << this_out << "\n";
    Context ctx(msgs, contents, filename, PassStep::Init, last_step);

    runCompiler(ctx);
  }

  if(values.find("interactive") != values.end()) {
    info();
    std::string line;
    while(true) {
      std::cerr << "> ";
      if(!getline(std::cin, line) || line == ":q") {
        break;
      }
      Context ctx(msgs, line, "stdin", PassStep::Init, last_step);
      runCompiler(ctx);
    }
  }

  return 0;
}

void runCompiler(Context &ctx) {
  try {
    if(ctx.done()) {
      return;
    }

    Tokens toks = lex(ctx);
    if(ctx.done()) {
      std::cerr << "Lexed " << toks.size() << " tokens.\n";
      std::cerr << toString(toks, ctx) << "\n";
      return;
    }

    Tree<Token> tree = ast(toks, ctx);
    if(ctx.done()) {
      std::cerr << toString(tree.children, ctx, 0, "\n") << "\n";
      return;
    }

    Module module = parse(tree, ctx);
    if(ctx.done()) {
      std::cerr << toString(module, ctx, 0) << "\n";
      for(const auto msg : ctx.getMsgs()) {
        std::cerr << toString(msg, ctx, 2) << "\n";
      }
      return;
    }

    CheckedModule checked = check(module, ctx);
    if(ctx.done()) {
      return;
    }
  } catch (std::runtime_error er) {
    std::cerr << "Parser crashed with: " << er.what() << "\n";
  }
}
