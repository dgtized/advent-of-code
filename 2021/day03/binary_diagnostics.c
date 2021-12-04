#include <stdio.h>
#include <string.h>

// gcc binary_diagnostics.c -o binary_diagnostics && ./binary_diagnostics input

int bitcheck(char lines[1000][60], int length, int pos) {
  int on = 0;
  int off = 0;
  for(int i = 0; i < length; i++) {
    if(lines[i][pos] == '0')
      off++;
    if(lines[i][pos] == '1')
      on++;
  }
  printf("%d %d\n", on, off);
  return (on > off);
}

int main(int argc, char **argv) {
  const char *filename = argv[1];
  char lines[1000][60];
  int length = 0;

  FILE *file = fopen(filename, "r");
  while(fgets(lines[length], 60, file)) {
    length++;
  }
  fclose(file);

  int width = strlen(lines[0])-1;

  int gamma = 0;
  int epsilon = 0;
  for(int j = 0; j < width; j++) {
    int bitvalue = 1 << (width-j-1);
    if(bitcheck(lines, length, j)) {
      gamma += bitvalue;
    } else {
      epsilon += bitvalue;
    }
  }

  printf("First Star: %d %d -> %d\n", gamma, epsilon, gamma * epsilon);
}
