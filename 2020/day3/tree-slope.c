#include <stdio.h>
#include <string.h>

// gcc tree-slope.c -o tree-slope && ./tree-slope input

int main(int argc, char **argv) {
     const char *filename = argv[1];
     char lines[400][60];
     int height = 0;

     FILE *file = fopen(filename, "r");
     while(fgets(lines[height], 60, file)) {
          height++;
     }
     fclose(file);

     int width = strlen(lines[0])-1;

     printf("%d %d\n", width, height);

     int px = 0;
     int py = 0;
     int trees = 0;

     do {
          char curr = lines[py][px % width];
          //printf("%d %d %c\n", px, py, curr);
          px += 3;
          py += 1;

          if(curr == '#') {
               trees++;
          }
     } while(py < height);

     printf("trees: %d\n", trees);
}
