
int main(int argc, char* argv[]) {
  const auto x = [&] (const int x_y) {
    return (x_y+1);
  };
  return x(3);
};
