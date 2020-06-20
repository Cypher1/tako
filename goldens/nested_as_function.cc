
int main(int argc, char* argv[]) {
  const auto x = [&] () {
    const int x_y = 3;
    return (x_y*2);
  };
  return (x()*5);
}
