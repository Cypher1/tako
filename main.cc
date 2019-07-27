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

#include "src/takoConfig.h"
#include "src/ast.h"
#include "src/parser.h"
#include "src/show.h"
#include "src/checker.h"
#include "src/arg_parser.h"
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
  std::cerr << "tako - version " << VERSION_STR << "\n";
  std::cerr << "A compiler for ergonomic software verification\n";
}

int main(int argc, char* argv[]) {
  std::vector<std::string> targets;
  std::unordered_map<std::string, std::string> values;

  const std::string prog = argv[0];

  try {
    parseArgs(args, 1, argc, argv, targets, values);
  } catch (const std::runtime_error& er) {
    std::cerr << "Invalid command line argument: " << er.what() << "\n";
    return 1;
  }

  struct winsize w;
  ioctl(STDOUT_FILENO, TIOCGWINSZ, &w);

  Config config;
  config.height = w.ws_row;
  config.width = w.ws_col;

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
    Context ctx(msgs, contents, filename, PassStep::Init, last_step, config);

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
      Context ctx(msgs, line, "stdin", PassStep::Init, last_step, config);
      // TODO: Run for a definition?
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
      std::cerr << show(toks, ctx) << "\n";
      return;
    }

    std::optional<Tree<Token>> tree = ast::ast(toks, ctx, ast::parseModule);
    if(!tree) {
      return;
    }
    if(ctx.done()) {
      std::cerr << show(tree->children, ctx, 0, "\n") << "\n";
      return;
    }

    Module module = parser::parse<Module>(*tree, ctx, parser::parseModule);
    if(ctx.done()) {
      std::cerr << show(module, 0) << "\n";
      for(const auto msg : ctx.getMsgs()) {
        std::cerr << show(msg, ctx, 2) << "\n";
      }
      return;
    }

    // CheckedModule checked;
    check(module, ctx);
    if(ctx.done()) {
      return;
    }
  } catch (const std::runtime_error& er) {
    std::cerr << "Parser crashed with: " << er.what() << "\n";
  }
}
