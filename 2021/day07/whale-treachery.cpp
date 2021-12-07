#include <fstream>
#include <iostream>
#include <cstdio>
#include <string>
#include <vector>
#include <regex>

// usage: g++ whale-treachery.cpp -o whale-treachery && ./whale-treachery input

using namespace std;

int cost(vector<int> &positions, int pos) {
  int cost = 0;
  for(auto v : positions) {
    cost += abs(pos-v);
  }
  return cost;
}

int min_cost(vector<int> positions) {
  sort(positions.begin(), positions.end());

  int a = positions.front();
  int b = positions.back();
  cout << "min_cost: " << a << " " << b << endl;
  int best_cost = 10000000;
  int best_idx = -1;
  for(int i = a; i <= b; i++) {
    int c = cost(positions, i);
    if(c < best_cost) {
      best_cost = c;
      best_idx = i;
    }
  };

  return best_idx;
}

int main(int argc, char **argv) {
  ifstream file(argv[1]);

  vector<int> positions;
  string str;
  while(getline(file, str, ',')) {
    positions.push_back(atoi(str.c_str()));
  }

  float average = 0.0;
  for(auto v : positions) {
    average += v;
  }
  average = (average / positions.size());

  cout << average << endl;
  cout << cost(positions, 2) << endl;

  int min = min_cost(positions);
  cout << "First Star: @" << min << " -> " << cost(positions, min) << endl;
}
