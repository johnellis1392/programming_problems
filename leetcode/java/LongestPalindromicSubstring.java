import java.util.List;
import java.util.ArrayList;

public class LongestPalindromicSubstring {
  public static boolean isPalindrome(final List<Character> list) {
    for (int i = 0; i < list.size() / 2; i++)
      if (list.get(i) != list.get(list.size() - 1 - i))
        return false;
    return true;
  }

  public static String join(final List<Character> list) {
    var sb = new StringBuilder();
    for (var c : list) sb.append(c);
    return sb.toString();
  }

  public static String longestPalindrome(final String input) {
    var result = "";
    var list = new ArrayList<Character>();
    for (int i = 0; i < input.length(); i++) {
      list.add(input.charAt(i));
      for (int j = i+1; j < input.length(); j++) {
        list.add(input.charAt(j));
        if (isPalindrome(list) && list.size() > result.length())
          result = join(list);
      }
      list.clear();
    }
    return result;
  }

  public static void test(final String input) {
    System.out.println(longestPalindrome(input));
  }

  public static void main(final String[] args) {
    test("babad");
    test("cbbd");
  }
}
