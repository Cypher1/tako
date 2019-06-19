#include <iostream>
#include <fstream>
#include <sstream> //std::stringstream
#include <vector>
#include <unordered_map>

#include "takoConfig.h"
#include "parser/ast.h"
#include "parser/parser.h"
#include "parser/toString.h"
#include "arg_parser/arg_parser.h"

const std::vector<Arg> args = {
  {'h', "help", "Prints this help message.", ""},
  {'V', "version", "Prints the version number.", ""},
  {'O', "", "Number of optimisation passes.", "level"},
  {'o', "out", "File to write results to.", "file"},
  {'i', "interative", "Run interpreter.", ""},
};

void runParser(std::string filename);

int main(int argc, char* argv[]) {
  std::vector<std::string> targets;
  std::unordered_map<std::string, std::string> values;

  const std::string prog = argv[0];
  parseArgs(args, 1, argc, argv, targets, values);

  if (argc < 2 || values.find("help") != values.end() || values.find("version") != values.end()){
    std::cerr << "tako - version " << tako_VERSION_MAJOR << "." << tako_VERSION_MINOR << "." << tako_VERSION_PATCH << "\n";
    std::cerr << "A compiler for ergonomic software verification\n";

    if (argc >= 2 && values.find("help") == values.end()) {
      return 1;
    }
    std::string usage = makeUsage(prog, args);
    return 1;
  }
  std::string out = "%.o";
  const auto out_it = values.find("out");
  if(out_it != values.end()) {
    out = out_it->second;
  }
  for(const auto file : targets) {
    std::string this_out = out;
    this_out.replace(this_out.find('%'), 1, file);
    std::cerr << "> " << file << " -> " << this_out << "\n";
    runParser(file);
  }
  return 0;
}

void runParser(std::string filename) {
  std::ifstream inFile;
  inFile.open(filename);

  // TODO: use the file+stream natively using memmap.
  std::stringstream strStream;
  strStream << inFile.rdbuf();
  std::string contents = strStream.str();

  Result<Tokens> toks = lex(contents, filename);
  std::cerr << "Got " << toks.value.size() << " tokens.\n";

  Result<Tree<Token>> tree = ast(toks, contents, filename);
  std::cerr << "AST\n";
  std::cerr << toString(tree.value.children, contents, filename) << " .\n";
  Result<Module> module = parse(tree, contents, filename);

  std::cerr << "Got " << module.value.values.size() << " top level values.\n";
  for(const auto& val : module.value.values) {
    std::cerr << "> " << val.name << "\n";
    std::cerr << toString(val.args, contents, filename, 1) << "\n";
    std::cerr << toString(val.scope, contents, filename, 1) << "\n";
  }

  std::cerr << "Errors:\n";
  for(const auto msg : toks.msgs) {
    std::cerr << toString(msg, contents, filename, 1) << "\n";
  }
}
