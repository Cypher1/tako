#include <iostream>
#include <string>
#include <sstream>
namespace std{
template<typename T>
string to_string(T& t){
  stringstream out;
  out << t;
  return out.str();
}
}

int main(int argc, char *argv[]) {
  const auto examples_higher_order_apply = [&](const auto examples_higher_order_apply_x, const auto examples_higher_order_apply_f) {
    return examples_higher_order_apply_f(examples_higher_order_apply_f(examples_higher_order_apply_x));
  };
  std::cout << (((std::to_string(examples_higher_order_apply(3, [&](const auto examples_higher_order__it__f_y) {
    return (examples_higher_order__it__f_y * 2);
  })) + " ") + (examples_higher_order_apply(1, [&](const auto examples_higher_order__it__f_y) {
    return !(examples_higher_order__it__f_y);
  }) ? "true" : "false")) + "\n");
  return 0;
}