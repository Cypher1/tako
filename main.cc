#include <iostream>
#include <fstream>
#include <sstream> //std::stringstream

#include "ast.h"

void runParser(std::string filename) {
  std::ifstream inFile;
  inFile.open(filename);

  std::stringstream strStream;
  strStream << inFile.rdbuf();
  std::string contents = strStream.str(); // Todo use the file+stream natively using memmap.

  Result<Tokens> toks = lex(filename, contents);

  std::cout << "Got " << toks.value.size() << "\n";
}

int main(int argc, char* argv[]) {
  for(int i=0; i<argc; ++i) {
    std::cout << i << ": " << argv[i] << "\n";
    runParser(argv[i]);
  }
  return 0;
}
