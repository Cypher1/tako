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
struct x_i32_y_i32_z_i32;
struct x_i32_y_i32_z_i32{int32_t x; int32_t y;};
std::ostream& operator<<(std::ostream& os, const struct x_i32_y_i32_z_i32& t) { os << "struct(x=" << t.x << ", y=" << t.y << ", z=" << t.z << ")"; return os; }

int main(int argc, char* argv[]) {
  const auto examples_struct_construction_ob = [](const auto x, const auto y, const auto z){return x_i32_y_i32_z_i32{.x=x,.y=y,.z=z};}(2, 1, 3);
  std::cout << ((std::to_string((std::to_string("h=")+std::to_string(examples_struct_construction_ob)))+std::to_string("\n")));
  return 0;
}
