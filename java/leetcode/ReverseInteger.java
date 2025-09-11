import java.util.ArrayDeque;

public class ReverseInteger {
  public static final boolean SHOW_ALL_TESTS = true;

  public void dump(final ArrayDeque<Integer> q) {
    System.out.println(q);
  }

  public int reverse(final int x) {
    try {
      int sign = Integer.signum(x);
      Integer y = Integer.valueOf(Math.abs(x));
      var q = new ArrayDeque<Integer>();
      while (y != 0L) {
        q.add(y % 10);
        y /= 10;
      }

      // dump(q);
      y = 0;
      while (!q.isEmpty()) {
        Integer z = q.remove();
        y = Math.addExact(Math.multiplyExact(y, 10), z);
        // System.out.printf("z=%d, y=%d\n", z, y);
      }

      return Math.multiplyExact(y, sign);
    } catch (ArithmeticException e) {
      return 0;
    }
  }

  public static void  main(final String[] args) {
    final var main = new ReverseInteger();
    final var tests = new int[][] {
      {123, 321},
      {-123, -321},
      {120, 21},
      {1534236469, 0}
    };

    for (var i = 0; i < tests.length; i++) {
      var input = tests[i][0];
      var expected = tests[i][1];
      var actual = main.reverse(input);
      if (expected == actual && ReverseInteger.SHOW_ALL_TESTS) {
        System.out.printf("SUCCESS: reverse(%d) == %d\n", input, actual);
      } else if (expected != actual) {
        System.out.printf("FAILURE: input=%d, expected=%d, actual=%d\n", input, expected, actual);
      }
    }
  }
}
