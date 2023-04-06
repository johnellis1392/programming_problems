import java.util.HashMap;

public class RomanToInteger {
  public record Test(String input, Integer expected) {}

  /* *********************************** */
  /* ************ Solution 1 *********** */
  /* *********************************** */
  final HashMap<Character, Integer> values = new HashMap<>() {{
    this.put('I', 1);
    this.put('V', 5);
    this.put('X', 10);
    this.put('L', 50);
    this.put('C', 100);
    this.put('D', 500);
    this.put('M', 1000);
  }};

  public boolean lte_v1(final Character a, final Character b) {
    return this.values.get(a) <= this.values.get(b);
  }

  public int romanToInt_v1(final String s) {
    if (s.length() == 0) return 0;
    Character
      prev = s.charAt(s.length()-1),
      curr;
    Integer result = this.values.get(prev);
    for (int i = s.length()-2; i>=0; i--) {
      curr = s.charAt(i);
      if (this.lte_v1(prev, curr)) {
        result += this.values.get(curr);
        prev = curr;
      } else {
        result -= this.values.get(curr);
      }
    }
    return result;
  }

  /* *********************************** */
  /* ************ Solution 2 *********** */
  /* *********************************** */

  public int romanToInt(final String s) {
    int result = 0, prev = 0, curr = 0;
    for (int i = s.length()-1; i>=0; i--) {
      switch (s.charAt(i)) {
        case 'I': curr = 1; break;
        case 'V': curr = 5; break;
        case 'X': curr = 10; break;
        case 'L': curr = 50; break;
        case 'C': curr = 100; break;
        case 'D': curr = 500; break;
        case 'M': curr = 1000; break;
      }
      if (curr < prev) {
        result -= curr;
      } else {
        result += curr;
        prev = curr;
      }
    }
    return result;
  }

  public void run() {
    final var tests = new Test[] {
      new Test("VIII", 8),
      new Test("IV", 4),
      new Test("LVIII", 58),
      new Test("MCMXCIV", 1994)
    };
    for (var test : tests) {
      var actual = this.romanToInt(test.input);
      if (actual == test.expected) {
        System.out.printf("SUCCESS: input='%s', output=%d\n", test.input, actual);
      } else {
        System.out.printf("FAILURE: input='%s', expected=%d, actual=%d\n", test.input, test.expected, actual);
      }
    }
  }

  public static void main(final String[] args) {
    new RomanToInteger().run();
  }
}