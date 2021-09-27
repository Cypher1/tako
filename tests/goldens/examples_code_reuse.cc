int main(int argc, char* argv[]);

int main(int argc, char* argv[]) {
  const auto examples_code_reuse_add = [&](
    const auto examples_code_reuse_add_x,
    const auto examples_code_reuse_add_y
  ) {
    return (examples_code_reuse_add_x+examples_code_reuse_add_y);
  };
  const auto examples_code_reuse_add5 = [&](
    const auto examples_code_reuse_add5_x
  ) {
    return examples_code_reuse_add(examples_code_reuse_add5_x, 5);
  };
  return examples_code_reuse_add5(2);
}
