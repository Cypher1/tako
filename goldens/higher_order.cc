#include <iostream>
#include <string>

int main(int argc, char* argv[]) {
  const auto apply = [&] (const auto apply_x, const auto apply_f) {
    return apply_f(apply_f(apply_x));
  };
  std::cout << ((std::to_string(apply(3, [&] (const auto f_1_y) { return f_1_y*2; } ))+"  ")+std::to_string(apply(1, [&] (const auto f_2_y) { return !f_2_y; }))) << "\n";
}
