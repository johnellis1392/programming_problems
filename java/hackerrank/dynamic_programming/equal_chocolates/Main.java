import java.io.BufferedReader;
import java.io.FileReader;
import java.nio.BufferOverflowException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

public class Main {
  public record Input(int n, List<Integer> arr) {}

  public void dump(final List<Integer> arr) {
    var sb = new StringBuilder();
    sb.append("[");
    for (var i : arr) sb.append(i +  ", ");
    sb.append("]");
    System.out.println(sb.toString());
  }

  public int equal(List<Integer> arr) {
    // dump(arr);
    final int[] chocolates = new int[] { 1, 2, 5 };
    for (var ch : chocolates) {
      continue;
    }
    return 0;
  }

  public static Optional<List<Input>> readInput(final String filename) {
    try (var r = new  BufferedReader(new FileReader(filename))) {
      final int numTests = Integer.parseInt(r.readLine().trim());
      var tests = new ArrayList<Input>();
      for (int i = 0; i < numTests; i++) {
        final int n = Integer.parseInt(r.readLine().trim());
        final List<Integer> arr = Arrays.stream(r.readLine().trim().split(" "))
          .map(Integer::parseInt)
          .collect(Collectors.toList());
        tests.add(new Input(n, arr));
      }
      return Optional.of(tests);
    } catch (Exception e) {
      e.printStackTrace();
      return Optional.empty();
    }
  }

  public static void  main(final String[] args) {
    var m = new Main();
    readInput("./input.txt")
      .map(tests -> 
        tests.stream().map(input -> m.equal(input.arr)))
      .ifPresent(results -> { results.forEach(System.out::println); });
  }
}
