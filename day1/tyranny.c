#include <stdio.h>

int main(int argc, char **argv) {
     const char *filename = argv[1];
     FILE *file = fopen(filename, "r");
     int num = 0;
     int out = 0;

     int result = 0;
     while(fscanf(file, "%d\n", &num) != EOF) {
          out = num / 3 - 2;
          printf("%d -> %d\n", num, out);
          result += out;
     }
     printf("Sum: %d\n", result);

     fclose(file);
}
