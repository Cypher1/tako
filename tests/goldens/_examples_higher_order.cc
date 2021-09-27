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
  const auto _examples_higher_order_apply = [&](
    const auto _examples_higher_order_apply_x,
    const auto _examples_higher_order_apply_f
  ) {
    return _examples_higher_order_apply_f(_examples_higher_order_apply_f(_examples_higher_order_apply_x));
  };
  std::cout << ((std::to_string((std::to_string((std::to_string(_examples_higher_order_apply(3, [&](  const auto _examples_higher_order___it___f_y) {  return (_examples_higher_order___it___f_y*2);}))+std::to_string("  ")))+std::to_string(_examples_higher_order_apply(1, [&](  const auto _examples_higher_order___it___f_y) {  return !(_examples_higher_order___it___f_y);}))))+std::to_string("\n")));
  return 0;
}
