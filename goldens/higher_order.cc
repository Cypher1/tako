
int main(int argc, char* argv[]) {
  const auto apply = [&] (const int apply_x, const int apply_f) {
    return apply_f(apply_f(apply_x));
  };
  return ((apply(3, (3_f_y*2))+"  ")+apply(1, !(4_f_y)));
};
