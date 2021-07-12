#include <iostream>
#include <string>
#include <string>
#include <sstream>
namespace std{
template <typename T>
string to_string(const T& t){
  stringstream out;
  out << t;
  return out.str();
}
string to_string(const bool& t){
  return t ? "true" : "false";
}
}

int main(int argc, char* argv[]) {
  std::cout << ((std::to_string((std::stoi(([&argv](const int x){return argv[x];})(1))+1))+std::to_string("\n")));
  return 0;
}
