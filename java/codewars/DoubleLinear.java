import java.util.SortedSet;
import java.util.TreeSet;
import java.util.Queue;
import java.util.LinkedList;

public class DoubleLinear {
  public static <A> void dumps(final A[] a) {
    var sb = new StringBuilder();
    sb.append("[");
    for (final A i : a) sb.append(String.format("%s, ", i));
    sb.append("]");
    System.out.println(sb.toString());
  }

  // This doesn't work
  public static int doubleLinear2(final int n) {
    var set = new TreeSet<Integer>();
    Queue<Integer> queue = new LinkedList<>();

    set.add(1);
    queue.add(1);
    int i = 0;
    while (i < n) {
      final int x = queue.remove();
      final int y = x * 2 + 1;
      final int z = x * 3 + 1;
      set.add(y); queue.add(y);
      set.add(z); queue.add(z);
      i++;
    }

    var result = set.toArray(new Integer[set.size()]);
    dumps(result);
    return result[n].intValue();
  }

  // This works
  public static int doubleLinear(final int n) {
    if (n == 0) return 1;
    SortedSet<Integer> u = new TreeSet<>();
    u.add(1);
    for(int i = 0; i < n; i++) {
      int x = u.first();
      u.add(x * 2 + 1);
      u.add(x * 3 + 1);
      u.remove(x);
    }
    return u.first();
  }

  public static void test(final int input, final int expected) {
    final int output = doubleLinear(input);
    System.out.printf(
      "input = %d, expected = %d, output = %d, \t%d == %d => %s\n",
      input,
      expected, output,
      expected, output,
      Boolean.toString(expected == output)
    );
  }

  public static void main(final String[] args) {
    // test(10, 22);
    // test(20, 57);
    // test(30, 91);
    // test(50, 175);
    test(100, 447);
    // test(4750, 59464);
    // [1, 3, 4, 7, 9, 10, 13, 15, 19, 21, 22, 27, ...]
    // [0, 1, 2, 3, 4,  5,  6,  7,  8,  9, 10, 11, ]
  }
}
