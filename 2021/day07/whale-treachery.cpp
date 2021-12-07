#include <algorithm>
#include <fstream>
#include <iostream>
#include <vector>

// usage: g++ whale-treachery.cpp -o whale-treachery && ./whale-treachery input

using namespace std;

int cost(vector<int> &positions, int pos) {
  int cost = 0;
  for(auto v : positions) {
    cost += abs(pos-v);
  }
  return cost;
}

// this works but overflows on input dataset, see racket solution for part2
long cost2(vector<int> &positions, int pos) {
  long cost = 0;
  for(auto v : positions) {
    long dist = abs(pos-v);
    cost += (dist * (dist + 1)/2);
  }
  return cost;
}

long min_cost(vector<int> positions) {
  sort(positions.begin(), positions.end());

  long best_cost = 10000000;
  long best_idx = -1;
  for(int i = positions.front(); i <= positions.back(); i++) {
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

  cout << cost2(positions, 2) << endl;
  cout << cost2(positions, 5) << endl;

  int min = min_cost(positions);
  cout << "First Star: @" << min << " -> " << cost(positions, min) << endl;
}
