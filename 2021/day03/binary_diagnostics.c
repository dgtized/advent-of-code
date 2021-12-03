#include <stdio.h>
#include <string.h>

// gcc binary_diagnostics.c -o binary_diagnostics && ./binary_diagnostics input

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
       int on = 0;
       int off = 0;
       for(int i = 0; i < length; i++) {
            char *line = lines[i];
            if(line[j] == '0')
                 off++;
            if(line[j] == '1')
                 on++;
       }
       int bitvalue = 1 << (width-j-1);
       //printf(" %d %d -> %d %d\n", on, off, j, bitvalue);
       if(on > off) {
            gamma += bitvalue;
       } else {
            epsilon += bitvalue;
       }
  }

  printf("First Star: %d %d -> %d\n", gamma, epsilon, gamma * epsilon);
}
