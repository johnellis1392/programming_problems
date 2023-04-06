public class Main {
  public record Test(Integer input, Boolean expected) {}

  public boolean solution(final int n) {
    final int m = (int)((Math.log10(n)+1)/2);
    int i = n, a = 0, b = 0;
    for (int k = 0; k < m; k++) { a += i%10; i /= 10; }
    while (i > 0) { b += i%10; i /= 10; }
    return a == b;
  }

  public void run() {
    final var tests = new Test[] {
      new Test(1230, true),
      new Test(239017, false),
      new Test(134008, true),
      new Test(10, false)
    };
    for (final var test : tests) {
      final var actual = this.solution(test.input);
      if (test.expected.equals(actual)) {
        System.out.printf("SUCCESS: input=%d, actual=%s\n", test.input, actual);
      } else {
        System.out.printf("FAILURE: input=%d, expected=%s, actual=%s\n", test.input, test.expected, actual);
      }
    }
  }

  public static void main(final String[] args) {
    new Main().run();
  }
}
