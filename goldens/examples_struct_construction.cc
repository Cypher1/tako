#include <iostream>
#include <stdlib.h>
#include <string>
#include <sstream>

typedef struct {
  int32_t x;
  int32_t y;
} struct_x_i32_y_i32;

std::ostream& operator<<(std::ostream& os, const struct_x_i32_y_i32& t) {
  os << "struct(x=" << t.x << ", y=" << t.y << ")";
  return os;
}

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
  const auto examples_struct_construction_ob = ([](const auto x, const auto y){return struct_x_i32_y_i32{.x=x,.y=y};})(3, 5);
  std::cout << ((std::to_string("ob=")+(std::to_string(examples_struct_construction_ob) + "\n")));
  return 0;
}