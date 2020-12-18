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
struct x_i32_y_i32;
struct x_i32_y_i32 {int32_t x; int32_t y;};
std::ostream& operator<<(std::ostream& os, const struct x_i32_y_i32& t) { os << "struct(x=" << t.x << ", y=" << t.y << ")"; return os; }

int main(int argc, char* argv[]) {
  const auto examples_struct_construction_ob = [](const int32_t x, const int32_t y){return x_i32_y_i32{.x=x,.y=y};}(3, 5);
  std::cout << ((std::to_string((std::to_string("ob=")+std::to_string(examples_struct_construction_ob)))+std::to_string("\n")));
  return 0;
}
