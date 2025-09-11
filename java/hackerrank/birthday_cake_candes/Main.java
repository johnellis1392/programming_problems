import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

public class Main {
  public void dump(final List<Integer> list) {
    var sb = new StringBuilder();
    sb.append("[");
    for (var i : list) sb.append(String.format("%d, ", i));
    sb.append("]");
    System.out.println(sb.toString());
  }

  public int birthdayCakeCandles(List<Integer> candles) {
    // dump(candles);
    int maxSize = 0, n = 0;
    for (var i : candles) {
      if (i > maxSize) {
        maxSize = i;
        n = 1;
      } else if (i == maxSize) {
        n++;
      }
    }
    return n;
  }

  public static Optional<List<Integer>> readInput(final String filename) {
    try (var r = new BufferedReader(new  FileReader(filename))) {
      var _n = Integer.parseInt(r.readLine());
      return Optional.of(
        Arrays.stream(
          r.readLine().split(" ")
        ).map(Integer::parseInt).collect(Collectors.toList())
      );
    } catch (Exception e) {
      e.printStackTrace();
      return Optional.empty();
    }
  }

  public static void  main(final String[] args) {
    var m = new Main();
    readInput("./input.txt")
      .map(m::birthdayCakeCandles)
      .ifPresent(System.out::println);
  }
}
