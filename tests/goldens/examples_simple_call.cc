
int main(int argc, char* argv[]) {
  const auto examples_simple_call_x = [&](
    const auto examples_simple_call_x_y
  ) {
    return (examples_simple_call_x_y*3);
  };
  return examples_simple_call_x(2);
}
