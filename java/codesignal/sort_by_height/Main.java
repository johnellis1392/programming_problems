public class Main {
  public record Test(int[] input, int[] expected) {}

  public int[] solution(int[] input) {
    for (int i = 0; i < input.length-1; i++) {
      if (input[i] == -1) continue;
      int minIndex = i;
      for (int j = i+1; j < input.length; j++) {
        if (input[j] == -1) continue;
        if (input[j] < input[minIndex]) minIndex = j;
      }
      if (minIndex != i) {
        int temp = input[i];
        input[i] = input[minIndex];
        input[minIndex] = temp;
      }
    }
    return input;
  }

  public boolean equal(final int[] a, final int[] b) {
    if (a.length != b.length) return false;
    for (int i = 0; i < a.length; i++) {
      if (a[i] != b[i]) return false;
    }
    return true;
  }

  public String dumps(final int[] arr) {
    var sb = new StringBuilder();
    sb.append("[");
    for (var i : arr) sb.append(String.format("%d, ", i));
    sb.append("]");
    return sb.toString();
  }

  public void run() {
    final var tests = new Test[] {
      new Test(
        new int[] {-1, 150, 190, 170, -1, -1, 160, 180},
        new int[] {-1, 150, 160, 170, -1, -1, 180, 190}
      )
    };
    for (final var test : tests) {
      var actual = this.solution(test.input);
      if (!this.equal(actual, test.expected)) {
        System.err.printf("FAILURE:\n - expected=%s\n - actual=%s\n", this.dumps(test.expected), this.dumps(actual));
      } else {
        System.out.printf("SUCCESS\n");
      }
    }
  }

  public static void main(final String[] args) {
    new Main().run();
  }
}
