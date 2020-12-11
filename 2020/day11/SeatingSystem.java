import java.io.IOException;
import java.io.File;
import java.nio.file.Files;
import java.util.List;

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

    first_star(seating); // 2183
    second_star(seating); // 1990
  }

  public void first_star(char [][] seating) {
    char[][] last;

    do {
      last = seating;
      seating = step(seating);
    } while(!steady_state(seating, last));

    System.out.println("Star 1, Occupied @ steady state: " + occupied(seating));
  }

  public void second_star(char [][] seating) {
    char[][] last;

    do {
      last = seating;
      seating = step_line_of_sight(seating);
    } while(!steady_state(seating, last));

    System.out.println("Star 2, Occupied @ steady state: " + occupied(seating));
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

  public int neighbor_on_axis(char [][] seating, int x, int dx, int y, int dy) {
    x += dx;
    y += dy;
    if(x >= 0 && x < width &&
       y >= 0 && y < height) {
      char cell = seating[y][x];
      if(cell == '#') {
        return 1;
      } else if(cell == 'L')
        return 0;
      else {
        return neighbor_on_axis(seating, x, dx, y, dy);
      }
    } else {
      return 0;
    }
  }

  public int neighbors_in_sight(char [][] seating, int x, int y) {
    int occupied = 0;
    for(int dy = -1; dy <= 1; dy ++) {
      for(int dx = -1; dx <= 1; dx++) {
        if(dy == 0 && dx == 0) continue;
        occupied += neighbor_on_axis(seating, x, dx, y, dy);
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

  public char[][] step_line_of_sight(char [][] seating) {
    char [][]next = new char[height][width];

    for(int y = 0; y < height; y++) {
      for(int x = 0; x < width; x++) {
        char cell = seating[y][x];
        int n = neighbors_in_sight(seating, x, y);

        if(cell == 'L' && n == 0) {
          cell = '#';
        } else if(cell == '#' && n >= 5) {
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
