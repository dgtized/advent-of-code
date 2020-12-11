import java.io.IOException;
import java.io.File;
import java.nio.file.Files;
import java.util.List;
import java.util.Arrays;

public class SeatingSystem {
  public int width;
  public int height;

  public SeatingSystem(List<String> lines) {
    width = lines.get(0).length();
    height = lines.size();

    char [][] seating = new char[height][width];

    int y = 0;
    for(String line : lines) {
      for(int x = 0; x < line.length(); x++) {
        seating[y][x] = line.charAt(x);
      }
      y++;
    }

    int iter = 0;
    char[][] last;

    //display(seating);
    do {
      last = copy(seating);
      seating = step(seating);
      // System.out.println(iter);
      // display(seating);
      iter++;
    } while(!steady_state(seating, last));

    System.out.println("Occupied @ steady state: " + occupied(seating));
  }

  public char[][] copy(char [][] src) {
    return Arrays.stream(src).map(char[]::clone).toArray(char[][]::new);
  }

  public int occupied(char [][] seating) {
    int occupied = 0;
    for(int y = 0; y < height; y++) {
      for(int x = 0; x < width; x++) {
        if(seating[y][x] == '#')
          occupied++;
      }
    }
    return occupied;
  }

  public int neighbors(char [][] seating, int x, int y) {
    int occupied = 0;
    for(int ny = y - 1; ny <= y + 1; ny++) {
      for(int nx = x - 1; nx <= x + 1; nx++) {
        if(ny == y && nx == x) continue;
        if(ny < 0 || ny >= height) continue;
        if(nx < 0 || nx >= width) continue;

        if(seating[ny][nx] == '#') {
          occupied++;
        }
      }
    }
    return occupied;
  }

  public boolean steady_state(char [][] next, char [][] seating) {
    for(int y = 0; y < height; y++) {
      for(int x = 0; x < width; x++) {
        if(next[y][x] != seating[y][x]) {
          return false;
        }
      }
    }
    return true;
  }

  public char[][] step(char [][] seating) {
    char [][]next = new char[height][width];
    for(int y = 0; y < height; y++) {
      for(int x = 0; x < width; x++) {
        char cell = seating[y][x];
        int n = neighbors(seating, x, y);
        if(cell == 'L' && n == 0) {
          cell = '#';
        } else if(cell == '#' && n >= 4) {
          cell = 'L';
        }
        next[y][x] = cell;
      }
    }
    return next;
  }

  public void display(char [][] seating) {
    for(int y = 0; y < seating.length; y++) {
      System.out.println(seating[y]);
    }
  }

  public static void main(String args[]) {
    try {
      new SeatingSystem(Files.readAllLines(new File(args[0]).toPath()));
    } catch(IOException e) {
      e.printStackTrace();
    }
  }
}
