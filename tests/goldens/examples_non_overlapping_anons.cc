int main(int argc, char* argv[]);

int main(int argc, char* argv[]) {
  const auto examples_non_overlapping_anons_x = [&](
    const auto examples_non_overlapping_anons_x_it
  ) {
    return (examples_non_overlapping_anons_x_it+1);
  };
  return (examples_non_overlapping_anons_x(2)*examples_non_overlapping_anons_x(3));
}
