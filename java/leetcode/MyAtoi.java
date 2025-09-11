import java.math.BigInteger;

public class MyAtoi {
  public record Test(
    String input,
    Integer expected
  ) {}

  public int myAtoi(final String s) {
    var i = 0;
    while (
      i < s.length() &&
      Character.isWhitespace(s.charAt(i))
    ) i++;

    int sign;
    if (s.charAt(i) == '-') {
      sign = -1;
      i++;
    } else if (s.charAt(i) == '+') {
      sign = 1;
      i++;
    } else if (!Character.isDigit(s.charAt(i))) {
      return 0;
    } else {
      sign = 1;
    }

    Long result = 0L;
    while (
      i < s.length() &&
      Character.isDigit(s.charAt(i))
    ) {
      var v = (int)(s.charAt(i) - '0');
      result = result * 10 + v;
      i++;
      System.out.printf("result=%d\n", result);
    }
    // return result * sign;
    if (sign > 0) {
      return (int)(result % ((long)Math.pow(2, 31)-1));
    } else {
      return (int)((-1 * result) % ((long)Math.pow(-2, 31)));
    }
  }

  public static void main(final String[] args) {
    var main = new MyAtoi();
    final var tests = new Test[] {
      // new Test("42", 42),
      // new Test("   -42", -42),
      // new Test("4193 with words", 4193),
      // new Test("words and 987", 0),
      new Test("-91283472332", 0),
    };

    for (var test : tests) {
      var actual = main.myAtoi(test.input);
      if (actual == test.expected) {
        System.out.printf("SUCCESS: expected=%d, actual=%d\n", test.expected, actual);
      } else {
        System.out.printf("FAILURE: expected=%d, actual=%d\n", test.expected, actual);
      }
    }
  }
}
