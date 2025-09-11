import java.util.HashSet;
import java.util.Set;

public class LongestNonRepeatingSubstring {
  public static void dumps(final Set<Character> set) {
    var sb = new StringBuilder();
    sb.append("{");
    for (var c : set) sb.append(String.format("%c, ", c));
    sb.append("}");
    System.out.println(sb.toString());
  }

  public static int lengthOfLongestSubstring(final String s) {
    var seen = new HashSet<Character>();
    var max = 0;
    for (int i = 0; i < s.length(); i++) {
      seen.add(s.charAt(i));
      for (int j = i+1; j < s.length(); j++)
        if (seen.contains(s.charAt(j))) break;
        else seen.add(s.charAt(j));
      max = Math.max(max, seen.size());
      seen.clear();
    }
    return max;
  }

  public static void test(final String input) {
    var result = lengthOfLongestSubstring(input);
    System.out.printf("lengthOfLongestSubstring(%s) = %d\n", input, result);
  }

  public static  void main(final String[] args) {
    test("abcabcbb");
  }
}
