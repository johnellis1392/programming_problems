import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.TreeSet;
import java.util.stream.Collectors;

public class IntPart {
  private static HashMap<Long, List<List<Long>>> cache = new HashMap<>();

  private static TreeSet<List<Long>> split(final Long n) {
    System.out.printf("split(%d)\n", n);
    if (n < 1) {
      System.out.println("n < 1");
      return new TreeSet<>();
    } else if (n == 1) {
      System.out.println("n == 1");
      return new TreeSet<List<Long>>() {{
        this.add(new ArrayList<Long>() {{
          this.add(n);
        }});
      }};
    }
    // else if (cache.containsKey(n)) return cache.get(n);
    else {
      System.out.println("else");
      var result = new TreeSet<List<Long>>() {{
        this.add(
          new ArrayList<Long>() {{ this.add(n); }}
        );
      }};

      // Generate Combinations
      Long i = 1L, x = n - i;
      while (i < n) {
        System.out.printf("i = %d, x = %d\n", i, x);
        for (var j : split(i)) {
          var k = new ArrayList<Long>();
          k.addAll(j);
          k.add(0, x);
          result.add(k);
        }
        i++;
        x = n - i;
      }

      // cache.put(n, result);
      return result;
    }
  }

  public static String part(final long n) {
    int range = 0; double average = 0.0, median = 0.0;

    return String.format(
      "Range: %d Average: %.02f Median: %.02f",
      range,
      average,
      median
    );
  }

  public static String dumps(final List<List<Long>> list) {
    var sb = new StringBuilder();
    sb.append("[");
    for (var i : list) {
      sb.append("[");
      for (var j : i)
        sb.append(String.format("%d, ", j));
      sb.append("], ");
    }
    sb.append("]");
    return sb.toString();
  }

  public static String dumps(final TreeSet<List<Long>> list) {
    return dumps(
      list.stream().collect(Collectors.toList())
    );
  }

  public static void main(final String[] args) {
    System.out.println("Starting...");

    // System.out.println(dumps(split(1L)));
    // System.out.println(dumps(split(2L)));
    System.out.println(dumps(split(3L)));
    // System.out.println(dumps(split(4L)));
    // System.out.println(dumps(split(5L)));
    // System.out.println(dumps(split(6L)));

    // System.out.println(part(2));
    // System.out.println(part(3));
    // System.out.println(part(4));
    // System.out.println(part(5));
  }
}
