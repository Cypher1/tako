
int main(int argc, char* argv[]) {
  const auto examples_mutual_recursion_is_even = [&](
    const auto examples_mutual_recursion_is_even_x
  ) {
    if((examples_mutual_recursion_is_even_x==0)) 1 else 
    throw 101;
  };
  const auto examples_mutual_recursion_is_odd = [&](
    const auto examples_mutual_recursion_is_odd_y
  ) {
    if((examples_mutual_recursion_is_odd_y==0)) 0 else 
    throw 101;
  };
  return examples_mutual_recursion_is_even(18);
}
