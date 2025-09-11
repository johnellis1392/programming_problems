import java.io.BufferedReader;
import java.io.FileReader;
import java.nio.BufferOverflowException;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

public class Main {
  public record Input(int n, List<Long> c) {}

  public void dump(final Long[] values) {
    var sb = new StringBuilder();
    sb.append("[");
    for (var i : values) sb.append(i + ", ");
    sb.append("]");
    System.out.println(sb.toString());
  }

  public Long getWays(final int n, final List<Long> coins) {
    Long[] values = new Long[n+1];
    Arrays.fill(values, 0L);
    values[0] = 0L;
    for (final Long coin : coins) {
      for (int i = 1; i <= n; i++) {
        int d = (int)(i-coin);
        if (d == 0) {
          values[i]++;
        } else if (d > 0) {
          values[i] += values[d];
          // values[i] += 1;
        }
        // System.out.printf("c=%d, values[%d] = %d\n", coin, i, values[i]);
        // System.out.printf("i=%d, c=%d\n", i, coin);
        // dump(values);
      }
    }
    dump(values);
    return values[n];
  }

  public static Optional<Input>  readInput(final String filename) {
    try (var r = new  BufferedReader(new FileReader(filename))) {
      final String[] values = r.readLine().trim().split(" ");
      final int n = Integer.parseInt(values[0]), m = Integer.parseInt(values[1]);
      final List<Long> c = Arrays.stream(r.readLine().trim().split(" "))
        .map(Long::parseLong)
        .collect(Collectors.toList());
      return Optional.of(
        new Input(n, c)
      );
    } catch (Exception e) {
      e.printStackTrace();
      return Optional.empty();
    }
  }

  public static void  main(final String[] args) {
    var m = new Main();
    readInput("./input.2.txt")
      .map(input -> m.getWays(input.n, input.c))
      .ifPresent(System.out::println);
  }
}
