#include <iostream>
#include <algorithm>
#include <fstream>
#include <sstream> //std::stringstream

#include "ast.h"

std::string toString(const Location& loc, const std::string contents) {
  std::stringstream o;
  size_t line = 1+std::count(contents.begin(), contents.begin()+loc.start, '\n');
  size_t col = loc.start - contents.rfind("\n", loc.start);
  o << "line " << line << " column " << col << "->" << loc.length << ": " << contents.substr(loc.start, loc.length);
  return o.str();
}

std::string toString(const Token& tok, const std::string& contents) {
  std::stringstream o;
  o << tok.type << "@" << toString(tok.loc, contents);
  return o.str();
}

std::string toString(const Message& msg, const std::string& contents) {
  std::stringstream o;
  o << msg.type << "@" << toString(msg.loc, contents) << ": " << msg.msg;
  return o.str();
}

std::string toString(const Tree<Token>& tree, const std::string& contents, int depth=0) {
  std::stringstream o;
  for(int i=0; i<depth; i++) {
    o << "  ";
  }
  o << toString(tree.value, contents) << "\n";
  for(const auto& child : tree.children) {
    o << toString(child, contents, depth+1);
  }
  return o.str();
}

void runParser(std::string filename) {
  std::ifstream inFile;
  inFile.open(filename);

  std::stringstream strStream;
  strStream << inFile.rdbuf();
  std::string contents = strStream.str(); // Todo use the file+stream natively using memmap.

  Result<Tokens> toks = lex(contents, filename);
  Result<Tree<Token>> tree = ast(toks, contents, filename);

  std::cout << "Got " << toks.value.size() << "\n";

  std::cout << toString(tree.value, contents) << "\n";
  std::cout << "Errors:\n";
  for(const auto msg : toks.msgs) {
    std::cout << toString(msg, contents) << "\n";
  }
}

int main(int argc, char* argv[]) {
  for(int i=1; i<argc; ++i) {
    std::cout << i << ": " << argv[i] << "\n";
    runParser(argv[i]);
  }
  return 0;
}
