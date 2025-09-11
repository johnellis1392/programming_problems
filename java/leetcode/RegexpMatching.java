import java.util.ArrayDeque;
import java.util.Arrays;
import java.util.Stack;

public class RegexpMatching {
  public record T(String in, String p, Boolean exp) {}

  public boolean isMatch(final String s, final String p) {
    var sq = new ArrayDeque<Character>();
    var pq = new ArrayDeque<Character>();
    for (var c : s.toCharArray()) sq.add(Character.valueOf(c));
    for (var c : p.toCharArray()) pq.add(Character.valueOf(c));

    while (sq.size() > 0 && pq.size() > 0) {
      Character sc = sq.remove(), pc = pq.remove();
      if (pq.size() > 0 && pq.peek() == '*') {
        pq.remove();
        if (pc == '.') {
          while(sq.size() > 0 && sq.peek() != pq.peek()) sq.remove();
        } else if (sc != pc) {
          continue;
        } else {
          while (sq.size() > 0 && sq.peek() == pc) sq.remove();
        }
      } else if (pc == '.') {
        continue;
      } else if (sc != pc) {
        return false;
      } else {
        continue;
      }
    }

    return sq.isEmpty() && pq.isEmpty();
  }

  public static void main(String[] args) {
    var main = new RegexpMatching();
    var tests = new T[] {
      new T("aa", "a", false),
      new T("aa", "a*", true),
      new T("ab", ".*", true),
      new T("mississippi", "mis*is*p*.", false),
      new T("ab", ".*c", false),
      new T("aaa", "a*a", true)
    };
    for (var test : tests) {
      var actual = main.isMatch(test.in, test.p);
      if (actual != test.exp) {
        System.out.printf("FAILURE: %s, actual=%s\n", test, Boolean.toString(actual));
      } else {
        System.out.println("SUCCESS: " + test);
      }
    }
  }
}