#include <iostream>
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
  const auto examples_hello_name_name = ([&argv](const int x){return argv[x];})(1);
  std::cout << ((std::to_string((std::to_string("Got ")+std::to_string([&argc](){return argc;}())))+std::to_string(" arguments.\n")));
  std::cout << ((std::to_string((std::to_string("Hello, ")+std::to_string(examples_hello_name_name)))+std::to_string("!\n")));
  return 0;
}
