
int main(int argc, char* argv[]) {
  const auto x = [&] (const int x_it) {
    return (x_it+1);
  };
  return (x(2)*x(3));
}
