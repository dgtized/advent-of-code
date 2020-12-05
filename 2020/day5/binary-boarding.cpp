#include <iostream>
#include <fstream>
#include <cstdio>

// g++ binary-boarding.cpp -o binary-boarding && ./binary-boarding input | ./missing_seat.sh

using namespace std;

int base(string s, int bounds) {
  //cout << s << endl;
  int lower = 0;
  int upper = bounds - 1;
  for(int i = 0; i < s.size(); i++) {
    char c = s[i];
    int mid = lower + ((upper - lower) / 2);
    if(c == 'F' || c == 'L') {// lower
      upper = mid;
    }
    else if(c == 'B' || c == 'R') {// upper
      lower = mid + 1;
    }
    //printf("%c %d %d %d\n", c, lower, upper, mid);
  }
  return lower;
}

int row(string pass) {
  return base(pass.substr(0,7), 128);
}

int column(string pass) {
  return base(pass.substr(7,10), 8);
}

int main(int argc, char **argv) {
  ifstream file(argv[1]);

  int max = 0;

  string pass;
  while(getline(file, pass)) {
    int r = row(pass);
    int c = column(pass);
    int seat = r * 8 + c;
    cout << pass << " " << r << " " << c << " " << seat << endl;

    if (seat > max)
      max = seat;
  }

  cerr << "Highest SeatID: " << max << endl;
}
