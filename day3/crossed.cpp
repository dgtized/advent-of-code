#include <fstream>
#include <cassert>

// g++ crossed.cpp && ./a.out input

int main(int argc, char **argv) {
  assert(argc == 2);

  std::ifstream input(argv[1]);
  std::string lineA, lineB;

  getline(input, lineA);
  getline(input, lineB);
  input.close();

  printf("%s\n%s\n", lineA.c_str(), lineB.c_str());

  std::pair<int,int> a(0,0);
  std::pair<int,int> b(0,1);
  std::pair<int,int> c(0,0);

  assert(a == c);
  assert(a != b);

  return 0;
}
