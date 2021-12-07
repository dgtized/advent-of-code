#include <fstream>
#include <iostream>
#include <cstdio>
#include <string>
#include <vector>
#include <regex>

// usage: g++ whale-treachery.cpp -o whale-treachery && ./whale-treachery input

using namespace std;

int main(int argc, char **argv) {
  ifstream file(argv[1]);

  vector<int> positions;
  string str;
  while(getline(file, str, ',')) {
    positions.push_back(atoi(str.c_str()));
  }

  for(auto v : positions) {
    cout << v << endl;
  }
}
