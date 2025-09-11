import java.util.ArrayList;

public class Main {
  public void dumps(final ArrayList<String> input) {
    var sb = new StringBuilder();
    sb.append("[");
    for (final String s : input) sb.append(String.format("%s, ", s));
    sb.append("]");
    System.out.println(sb.toString());
  }

  public String[] solution(String[] input) {
    int maxLength = 0;
    var set = new ArrayList<String>();
    for (int i = 0; i < input.length; i++) {
      System.out.println(" - i = " + i);
      System.out.println(input[i]);
      if (input[i].length() > maxLength) {
        System.out.println("Check 1");
        maxLength = input[i].length();
        set.clear();
        set.add(input[i]);
      } else if (input[i].length() == maxLength) {
        System.out.println("Check 2");
        set.add(input[i]);
      }
      this.dumps(set);
    }
    return set.toArray(new String[set.size()]);
  }

  public void dumps(final String[] input) {
    var sb = new StringBuilder();
    sb.append("[");
    for (final String s : input) sb.append(String.format("%s, ", s));
    sb.append("]");
    System.out.println(sb.toString());
  }

  public static void main(final String[] args) {
    var m = new Main();

    m.dumps(m.solution(new String[] { "aba", "aa", "ad", "vcd", "aba" }));
  }
}
