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

struct Arg {
  char flag;
  std::string name;
  std::string description;
  std::string value;
};

const std::vector<Arg> args = {
  {'h', "help", "Prints this help message.", ""},
  {'V', "version", "Prints the version number.", ""},
  {'O', "", "Number of optimisation passes.", "level"},
  {'o', "out", "File to write results to.", "file"},
  {'i', "interative", "Run interpreter.", ""},
};

int main(int argc, char* argv[]) {
  std::vector<std::string> targets;
  std::unordered_map<std::string, std::string> values;

  const auto setValue = [&values, argc, argv](const std::string& name, unsigned int& ref) {
      for(const auto& arg : args) {
        if(arg.name != name && std::string("")+arg.flag != name) {
          continue;
        }
        if (arg.value.size()) {
          ref++;
          if (ref >= argc) {
            std::cerr << "Option " << arg.name << " needs an argument\n.";
            values["help"] = "";
            return;
          }
          values[arg.name] = argv[ref];
        } else {
          values[arg.name] = "";
        }
        return;
      }
      std::cerr << "Unexpected argument: '" << name << "'\n.";
      values["help"] = "";
  };

  const std::string prog = argv[0];
  for(unsigned int i = 1; i < argc; ++i) {
    std::string val = argv[i];
    if(val.size() < 1 || val[0] != '-') {
      targets.push_back(val);
      continue;
    }
    if(val.size() > 1 && val[1] == '-') {
      setValue(val.substr(2), i);
      continue;
    }
    for(unsigned int n=1; n < val.size(); n++) {
      setValue(val.substr(n, 1), i);
    }
  }

  if (argc < 2 || values.find("help") != values.end() || values.find("version") != values.end()){
    std::cerr << prog << " Version " << tako_VERSION_MAJOR << "." << tako_VERSION_MINOR << "\n";
    if (argc >= 2 && values.find("help") == values.end()) {
      return 1;
    }
    std::cerr << "Usage: " << prog << " [ options ] <targets>\n";
    for(const auto& arg : args) {
      std::stringstream s;
      s << "  -" << arg.flag;
      if (arg.name.size()) {
        s << " or --" << arg.name;
      }
      if (arg.value.size()) {
        s << " <" << arg.value << ">";
      }
      std::cerr << s.str();
      for(unsigned int k = s.str().size(); k < 22; k++) {
        std::cerr << " ";
      }
      std::cerr << arg.description << "\n";
    }
    return 1;
  }
  for(const auto file : targets) {
    std::cerr << "> " << file << "\n";
    runParser(file);
  }
  return 0;
}
