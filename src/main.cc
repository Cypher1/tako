#include <iostream>
#include <algorithm>
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

void runParser(std::string filename) {
  std::ifstream inFile;
  inFile.open(filename);

  std::stringstream strStream;
  strStream << inFile.rdbuf();
  std::string contents = strStream.str(); // Todo use the file+stream natively using memmap.

  Result<Tokens> toks = lex(contents, filename);
  std::cerr << "Got " << toks.value.size() << " tokens.\n";
  Result<Tree<Token>> tree = ast(toks, contents, filename);
  
  Result<Module> module = parse(tree, contents, filename);

  std::cerr << "Got " << module.value.names.size() << " top level names.\n";

  std::cerr << toString(tree.value, contents) << "\n";
  std::cerr << "Errors:\n";
  for(const auto msg : toks.msgs) {
    std::cerr << toString(msg, contents) << "\n";
  }
}

int main(int argc, char* argv[]) {
  std::vector<std::string> targets;
  std::unordered_map<std::string, std::string> values;

  const std::string prog = argv[0];
  parseArgs(args, 1, argc, argv, targets, values);

  if (argc < 2 || values.find("help") != values.end() || values.find("version") != values.end()){
    std::cerr << "tako - version " << tako_VERSION_MAJOR << "." << tako_VERSION_MINOR << "\n";
    std::cerr << "An ergonomic software verification language\n";

    if (argc >= 2 && values.find("help") == values.end()) {
      return 1;
    }
    std::string usage = makeUsage(prog, args);
    return 1;
  }
  for(const auto file : targets) {
    std::cerr << "> " << file << "\n";
    runParser(file);
  }
  return 0;
}
