#include <iostream>
#include <string>
namespace std{
template<typename T>
string to_string(T& t){
  return string(t);
}
}

int main(int argc, char *argv[]) {
  const auto examples_higher_order_apply = [&](const auto examples_higher_order_apply_x, const auto examples_higher_order_apply_f) {
    return examples_higher_order_apply_f(examples_higher_order_apply_f(examples_higher_order_apply_x));
  };
  const auto examples_higher_order_f1 = [&](const auto examples_higher_order__it__f_y) {
    return (examples_higher_order__it__f_y * 2);
  };
  const auto examples_higher_order_f2 = [&](const auto examples_higher_order__it__f_y) {
    return !(examples_higher_order__it__f_y);
  };
  std::cout << (std::to_string(examples_higher_order_apply(3, examples_higher_order_f1)) + std::string(" ") + std::to_string(examples_higher_order_apply(1, examples_higher_order_f2) ? "true" : "false")) + "\n");
  return 0;
}