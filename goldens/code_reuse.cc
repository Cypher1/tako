
int main(int argc, char* argv[]) {
  const auto add = [&] (const auto add_x, const auto add_y) {
    return (add_x+add_y);
  };
  const auto add5 = [&] (const auto add5_x) {
    return add(add5_x, 5);
  };
  return add5(2);
}
