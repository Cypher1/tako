
int main(int argc, char* argv[]) {
  const auto x = [&] (const auto x_y) {
    return (x_y*3);
  };
  return x(2);
}
