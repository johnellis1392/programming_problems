import java.util.ArrayDeque;

public class IsPalindrome {
  public record T(Integer input, Boolean expected) {}

  public boolean isPalindrome(final int x) {
    if (x < 0) return false;
    var q = new ArrayDeque<Integer>();
    var y = x;
    while (y > 0) {
      q.add(y % 10);
      y /= 10;
    }
    while (!q.isEmpty()) {
      if (q.size() <= 1) return true;
      Integer a = q.removeFirst(), b = q.removeLast();
      if (a != b) return false;
    }
    return true;
  }

  public static void main(final String[] args) {
    var main = new IsPalindrome();
    var tests = new T[] {
      new T(0, true),
      new T(121, true),
      new T(-121, false),
      new T(10, false)
    };
    for (var test : tests) {
      var actual = main.isPalindrome(test.input);
      if (actual == test.expected) {
        System.out.printf("SUCCESS: input=%d, expected=%s\n", test.input, Boolean.toString(test.expected));
      } else {
        System.out.printf("FAILURE: input=%d, expected=%s, actual=%s\n", test.input, Boolean.toString(test.expected), Boolean.toString(actual));
      }
    }
  }
}
