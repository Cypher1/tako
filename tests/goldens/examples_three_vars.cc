int main(int argc, char* argv[]);

int main(int argc, char* argv[]) {
  const auto examples_three_vars_x = 2;
  const auto examples_three_vars_y = 3;
  const auto examples_three_vars_z = (examples_three_vars_x+examples_three_vars_y);
  return ((examples_three_vars_x*examples_three_vars_y)*examples_three_vars_z);
}
