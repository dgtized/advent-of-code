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

    System.out.println("Star 1, Occupied @ steady state: " +
                       converge(seating, 4)); // 2183
    System.out.println("Star 2, Occupied @ steady state: " +
                       converge(seating, 5)); // 1990
  }

  public int converge(char [][] seating, int crowded) {
    char [][] last;
    do {
      last = seating;
      seating = step(seating, crowded);
    } while(!steady_state(seating, last));

    return occupied(seating);
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

  public char[][] step(char [][] seating, int crowded) {
    char [][]next = new char[height][width];
    for(int y = 0; y < height; y++) {
      for(int x = 0; x < width; x++) {
        char cell = seating[y][x];

        int n;
        if(crowded == 4) // first star
          n = neighbors(seating, x, y);
        else // second star
          n = neighbors_in_sight(seating, x, y);

        if(cell == 'L' && n == 0) {
          cell = '#';
        } else if(cell == '#' && n >= crowded) {
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
