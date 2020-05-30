
int main(int argc, char* argv[]) {
  const auto x = [&] () {
    return 3;
  };
  return x();
}
