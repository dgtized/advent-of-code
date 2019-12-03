#include <fstream>
#include <sstream>
#include <cassert>
#include <vector>
#include <algorithm>
#include <cstdio>

// g++ crossed.cpp && ./a.out input

std::vector<std::pair<int, int>> trace(const std::string& s) {
  std::vector<std::pair<int, int>> path;

  std::string token;

  int x = 0;
  int y = 0;

  std::istringstream seq(s);
  while(std::getline(seq, token, ',')) {
    char direction;
    int distance;
    sscanf(token.c_str(), "%c%d", &direction, &distance);
    for(int i = 0; i < distance; i++) {
      if(direction == 'U') {
        y++;
      } else if(direction == 'D') {
        y--;
      } else if(direction == 'L') {
        x--;
      } else if(direction == 'R') {
        x++;
      }
      //printf("%d %d\n", x, y);
      path.push_back(std::make_pair(x, y));
    }
  }

  return path;
}

int main(int argc, char **argv) {
  assert(argc == 2);

  std::ifstream input(argv[1]);
  std::string lineA, lineB;

  getline(input, lineA);
  getline(input, lineB);
  input.close();

  printf("%s\n%s\n", lineA.c_str(), lineB.c_str());

  auto pathA = trace(lineA);
  auto pathB = trace(lineB);
  int min_manhatten = 0;
  int min_position = 0;

  printf("searching ...\n");
  for(auto it = pathA.begin(); it < pathA.end(); it++) {
    auto match = std::find(pathB.begin(), pathB.end(), *it);
    if(match != pathB.end()) {
      int manhattan = abs(it->first) + abs(it->second);
      if(manhattan < min_manhatten || min_manhatten == 0) {
        min_manhatten = manhattan;
      }
      int position = std::distance(pathA.begin(), it) + std::distance(pathB.begin(), match) + 2;
      if(position < min_position || min_position == 0) {
        min_position = position;
      }
      printf("match %d %d m%d p%d\n", it->first, it->second, manhattan, position);
    }
  }

  printf("minimum distance %d\n", min_manhatten);
  printf("minimum position %d\n", min_position);

  return 0;
}
