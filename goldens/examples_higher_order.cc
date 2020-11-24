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
  const auto examples_higher_order_apply = [&](
    const auto examples_higher_order_apply_x,
    const auto examples_higher_order_apply_f
  ) {
    return examples_higher_order_apply_f(examples_higher_order_apply_f(examples_higher_order_apply_x));
  };
  std::cout << ((std::to_string((std::to_string((std::to_string(examples_higher_order_apply(3, [&](  const auto examples_higher_order__it__f_y) {  return (examples_higher_order__it__f_y*2);}))+std::to_string("  ")))+std::to_string(examples_higher_order_apply(1, [&](  const auto examples_higher_order__it__f_y) {  return !(examples_higher_order__it__f_y);}))))+std::to_string("\n")));
  return 0;
}
