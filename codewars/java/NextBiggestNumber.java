import java.util.ArrayList;
import java.util.Arrays;
import java.util.SortedSet;
import java.util.Stack;
import java.util.TreeSet;
import java.util.Collections;
import java.util.List;
import java.util.function.Consumer;

public class NextBiggestNumber {
  private static <A> void swap(A[] c, final int i, final int j) {
    var temp = c[i];
    c[i] = c[j];
    c[j] = temp;
  }

  // private static SortedSet<String> permutations(final String input) {
  //   var arr = input.toCharArray();
  //   final var n = arr.length;
  //   SortedSet<String> result = new TreeSet<>();
  //   var indexes = new int[n];
  //   for (var i = 0; i < n; i++) indexes[i] = 0;
  //   result.add(toString(arr));
  //   var i = 0;
  //   while (i < n) {
  //     if (indexes[i] < i) {
  //       swap(arr, i % 2 == 0 ? 0 : indexes[i], i);
  //       result.add(toString(arr));
  //       indexes[i]++;
  //       i = 0;
  //     } else {
  //       indexes[i] = 0;
  //       i++;
  //     }
  //   }
  //   return result;
  // }

  // public static long nextBiggerNumber(final long n) {
  //   return permutations(Long.toString(n))
  //     .stream()
  //     .map(Long::valueOf)
  //     .dropWhile(v -> v <= n)
  //     .findFirst()
  //     .get();
  // }

  private static void assertEquals(final long expected, final long actual) {
    System.out.printf(
      "expected %s == actual %s => %s\n",
      expected,
      actual,
      expected == actual
    );
  }

  private static String toString(final char[] cs) {
    StringBuilder b = new StringBuilder();
    for (final char c : cs) b.append(c);
    return b.toString();
  }

  public static Long[] split(final long n) {
    var stack = new Stack<Long>();
    long i = n;
    while (i > 0) {
      stack.push(i % 10);
      i /= 10;
    }
    var result = new Long[stack.size()];
    int j = 0;
    while (!stack.empty()) result[j++] = stack.pop();
    return result;
  }

  public static long build(final Long[] arr) {
    long result = 0;
    for (int i = 0; i < arr.length; i++)
      result = result * 10 + arr[i];
    return result;
  }

  // public static long nextBiggerNumber(final long n) {
  //   var arr = split(n);
  //   final var m = arr.length;
  //   outer:
  //   for (int i = m - 1; i >= 0; i--) {
  //     for (int j = i - 1; j >= 0; j--) {
  //       if (arr[i] > arr[j]) {
  //         swap(arr, i, j);
  //         break outer;
  //       }
  //     }
  //   }
  //   var result = build(arr);
  //   return result;
  // }

  private static Long nextPermutation(final Long input) {
    var arr = split(input);
    final var n = arr.length;
    var indexes = new int[n];
    Arrays.fill(indexes, 0);

    var i = 0;
    Long result = Long.MAX_VALUE;
    while (i < n) {
      if (indexes[i] < i) {
        swap(arr, i % 2 == 0 ? 0 : indexes[i], i);
        var perm = build(arr);
        if (input < perm && perm < result) result = perm;
        indexes[i]++;
        i = 0;
      } else {
        indexes[i] = 0;
        i++;
      }
    }

    return result.equals(Long.valueOf(Long.MAX_VALUE)) ? -1 : result;
  }

  public static long nextBiggerNumber(final long n) {
    return nextPermutation(n);
  }

  public static void test(final long expected, final long input) {
    final long actual = nextBiggerNumber(input);
    System.out.printf("Input = %d: ", input);
    assertEquals(expected, actual);
  }

  public static void test(final long input) {
    System.out.printf("input = %d, output = %d\n", input, nextBiggerNumber(input));
  }

  public static void main(final String[] args) {
    test(21, 12);
    test(531, 513);
    test(2071, 2017);
    test(441, 414);
    test(414, 144);
    test(19009, 10990);
    test(123456789);
    test(1234567890);
    test(9876543210L);
    test(9999999999L);
  }
}
