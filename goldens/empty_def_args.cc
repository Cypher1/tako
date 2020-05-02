
int main(int argc, char* argv[]) {
  auto x = [&] () {
    return 3;
  };

  return x();
};
