
int main(int argc, char* argv[]) {
  const auto apply = [&] (const auto apply_x, const auto apply_f) {
    return apply_f(apply_f(apply_x));
  };
  return println(((apply(3, (3_it_4_f_y*2))+"  ")+apply(1, !(3_it_5_f_y))));
}
