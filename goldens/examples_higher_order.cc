
int main(int argc, char* argv[]) {
  const auto examples_higher_order_apply = [&] (const auto examples_higher_order_apply_x, const auto examples_higher_order_apply_f) {
    return examples_higher_order_apply_f(examples_higher_order_apply_f(examples_higher_order_apply_x));
  };
  return println(((examples_higher_order_apply(3, (examples_higher_order_?_it_?_f_y*2))+"  ")+examples_higher_order_apply(1, !(examples_higher_order_?_it_?_f_y))));
}
