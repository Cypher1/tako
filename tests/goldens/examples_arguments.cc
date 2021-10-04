int main(int argc, char* argv[]);

int main(int argc, char* argv[]) {
  const auto examples_arguments_x = [&](
    const auto examples_arguments_x_y
  ) {
    return (examples_arguments_x_y+1);
  };
  return examples_arguments_x(3);
}
