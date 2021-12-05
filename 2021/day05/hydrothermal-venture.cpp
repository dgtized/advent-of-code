#include <iostream>
#include <fstream>
#include <cstdio>
#include <string>
#include <regex>

// usage: g++ hydrothermal-venture.cpp -o hydrothermal-venture && ./hydrothermal-venture input

using namespace std;

struct Point {
  int x,y;
  Point(int x, int y) : x(x), y(y) {}
};

struct Line {
  Point p,q;
  Line(Point &p, Point &q) : p(p), q(q) {}
  Line(int x0, int y0, int x1, int y1) : p(Point(x0,y0)), q(Point(x1, y1)) {}
};

int main(int argc, char **argv) {
  ifstream file(argv[1]);

  int display[1000][1000];

  for(int i = 0; i < 1000; i++) {
    for(int j = 0; j < 1000; j++) {
      display[i][j] = 0;
    }
  }
  string line;
  cmatch match;
  while(getline(file, line)) {
    if(regex_match(line.c_str(), match, regex("([0-9]+),([0-9]+) -> ([0-9]+),([0-9]+)"))) {
      Line l(atoi(match[1].str().c_str()), atoi(match[2].str().c_str()),
             atoi(match[3].str().c_str()), atoi(match[4].str().c_str()));
    }
  }
}
