import java.io.File;
import java.io.IOException;
import java.nio.file.Files;

// javac FFT.java && java FFT input.test
public class FFT {

  public static void debug(int input[]) {
    for(int i = 0; i < input.length; i++) {
      System.out.printf("%1d", input[i]);
    }
    System.out.println();
  }

  public static int[] phase(int input[]) {
    int [] output = new int [input.length];
    int [] pattern = new int []{0, 1, 0, -1};

    for(int base = 0; base < input.length; base++) {
      if(base > 0) {
        System.out.printf("%"+base*6+"s", " ");
      }
      int sum = 0;
      for(int digit = base; digit < input.length; digit++) {
        int offset = (base == 0 ? digit + 1 : (digit + 1) / (base+1));
        int pat = pattern[offset % 4];
        int out = input[digit];
        System.out.printf("%2d*%-2d+", out, pat);
        sum += pat * out;
      }
      sum = Math.abs(sum)%10;
      System.out.printf(" = %d\n", sum);
      output[base] = sum;
    }

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
