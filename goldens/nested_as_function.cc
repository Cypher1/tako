
int main(int argc, char* argv[]) {
  
    const int x_y = 3;
  auto x = [&] () {
    return (x_y*2);
  };
  return (x()*5);
};
