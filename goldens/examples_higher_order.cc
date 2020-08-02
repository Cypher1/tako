#include <iostream>
#include <string>

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
  return printf("%s\n", (std::to_string(examples_higher_order_apply(3, examples_higher_order_f1)) + std::string(" ") + (examples_higher_order_apply(1, examples_higher_order_f2) ? "true" : "false")).c_str());
}