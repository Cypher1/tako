
int main(int argc, char* argv[]) {
  const auto add = [&] (const int add_x, const int add_y) {
    return (add_x+add_y);
  };
  const auto add5 = [&] (const int add5_x) {
    return add(add5_1_x, 5);
  };
  return add5(2);
};
