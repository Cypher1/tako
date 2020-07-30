
int main(int argc, char* argv[]) {
  const auto examples_empty_def_args_x = [&] () {
    return 3;
  };
  return examples_empty_def_args_x();
}
