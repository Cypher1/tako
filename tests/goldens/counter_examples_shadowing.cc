int main(int argc, char* argv[]);

int main(int argc, char* argv[]) {
  const auto counter_examples_shadowing_x = 3;
  const auto counter_examples_shadowing_x = (counter_examples_shadowing_x+2);
  return counter_examples_shadowing_x;
}
