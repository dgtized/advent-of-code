import java.io.File;
import java.io.IOException;
import java.nio.file.Files;

// javac FFT.java && java FFT input.test
public class FFT {

  public static void debug(int [] input) {
    for(int i = 0; i < input.length; i++) {
      System.out.printf("%2d", input[i]);
    }
    System.out.println();
  }

  public static int [] phase(int [] input) {
    int [] output = new int [input.length];
    return output;
  }

  public static void main(String args[]) {
    File file = new File(args[0]);
    try {
      byte[] bytes = Files.readAllBytes(file.toPath());
      int [] input = new int[bytes.length-1];
      for(int i = 0; i < bytes.length-1; i++) {
        input[i] = bytes[i] - 48;
      }
      for(int iters = 0; iters < 5; iters++) {
        debug(input);
        input = phase(input);
      }
    } catch(IOException e) {
      e.printStackTrace();
    }
  }
}
