
int main(int argc, char* argv[]) {
  const auto x = [&] () {
    return !("Not evaluated ever");
  };
  return (700+7);
};
