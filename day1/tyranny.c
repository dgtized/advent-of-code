#include <stdio.h>

int fuel(int mass) {
     return mass / 3 - 2;
}

int rfuel(int mass) {
     int f = fuel(mass);;
     int t = f;
     while((f = fuel(f)) > 0) {
          t += f;
     }
     return t;
}

int main(int argc, char **argv) {
     const char *filename = argv[1];
     FILE *file = fopen(filename, "r");
     int num = 0;

     int result = 0;
     while(fscanf(file, "%d\n", &num) != EOF) {
          printf("%d -> %d\n", num, rfuel(num));
          result += rfuel(num);;
     }
     printf("Sum: %d\n", result);

     fclose(file);
}
