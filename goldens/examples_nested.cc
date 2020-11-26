
int main(int argc, char* argv[]) {
  const auto examples_nested_x = ([&]() {
    const auto examples_nested_x_y = 3;
    return (examples_nested_x_y*2);
  })();
  return (examples_nested_x*5);
}
