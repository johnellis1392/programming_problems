import java.util.SortedSet;
import java.util.TreeSet;
import java.util.List;
import java.util.stream.Collectors;
import java.util.ArrayList;
import java.util.Arrays;

public class SinglePermutations {
  public static String toString(final char[] cs) {
    var sb = new StringBuffer();
    for (final char c : cs) sb.append(c);
    return sb.toString();
  }

  public static void swap(char[] arr, final int i, final int j) {
    var temp = arr[i];
    arr[i] = arr[j];
    arr[j] = temp;
  }

  public static SortedSet<String> permutations(String s) {
    var cs = s.toCharArray();
    final var n = s.length();
    var indexes = new int[n];
    Arrays.fill(indexes, 0);
    var result = new TreeSet<String>();
    result.add(toString(cs));

    var i = 0;
    while (i < n) {
      if (indexes[i] < i) {
        swap(cs, i % 2 == 0 ? 0 : indexes[i], i);
        result.add(toString(cs));
        indexes[i]++;
        i = 0;
      } else {
        indexes[i] = 0;
        i++;
      }
    }

    return result;
  }

  public static List<String> singlePermutations(String s) {
    return permutations(s)
      .stream()
      .collect(Collectors.toList());
  }

  public static String dumps(final List<String> list) {
    if (list == null) return "null";
    var sb = new StringBuilder();
    sb.append("[");
    for (final var item : list) sb.append(String.format("%s, ", item.toString()));
    sb.append("]");
    return sb.toString();
  }

  public static void assertEquals(
    final List<String> expected,
    final List<String> actual
  ) {
    final boolean equal = expected.equals(actual);
    System.out.printf(
      " - %s == %s \t=> %s\n",
      dumps(expected),
      dumps(actual),
      Boolean.toString(equal)
    );
  }

  public static void main(final String[] args) {

    assertEquals( new ArrayList<String>(Arrays.asList("a")),
                  singlePermutations("a").stream().sorted().collect(Collectors.toList()) );

    assertEquals( new ArrayList<String>(Arrays.asList("ab","ba")),
                  singlePermutations("ab").stream().sorted().collect(Collectors.toList()) );

    assertEquals( new ArrayList<String>(Arrays.asList("aabb", "abab", "abba", "baab", "baba", "bbaa")),
                  singlePermutations("aabb").stream().sorted().collect(Collectors.toList()) );
  }
}
