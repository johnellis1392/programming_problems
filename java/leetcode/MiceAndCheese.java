public class MiceAndCheese {
  public record Test(int[] in1, int[] in2, int k, int out) {}

  public String dumps(final int[] a) {
    var sb = new StringBuilder();
    sb.append("[");
    for (var i : a) sb.append(String.format("%d, ", i));
    sb.append("]");
    return sb.toString();
  }

  public void dump(final int[] a) {
    System.out.println(dumps(a));
  }

  public void combinations_v1(final int n, final int k) {
    var c = new int[k+2];
    for (int i = 0; i < k; i++) c[i] = i;
    c[k] = n; c[k+1] = 0; int j;
    while (true) { 
      dump(c);
      j = 0;
      while(c[j]+1 == c[j+1]) {
        c[j] = j+1;
        j = j+1;
      }
      if (j < k) c[j] = c[j]+1;
      else break;
    }
    System.out.println("Done.");
  }

  // This combination algorithm taken from this Baeldung link:
  // https://www.baeldung.com/java-combinations-algorithm
  public void combinations(final int n, final int k) {
    int[] c = new int[k];
    for (int i = 0; i < k; i++) c[i] = i;
    while (c[k-1] < n) {
      dump(c);
      int t = k - 1;
      while (t != 0 && c[t] == n-k+t) t--;
      c[t]++;
      for (int i = t+1; i<k; i++) c[i] = c[i-1]+1;
    }
  }

  // Try a recursive solution. Try every combination of elements
  // And recurse to try combinations of the two arrays;
  public int miceAndCheese_helper(
    final int[][] a,
    final int k,
    final int n0,
    final int i,
    final int r
  ) {
    if (i >= a[0].length) {
      if (n0 < k) return -1;
      else return r;
    } else if (n0 >= k) {
      return miceAndCheese_helper(a, k, n0, i+1, r+a[1][i]);
    } else {
       return Math.max(
        miceAndCheese_helper(a, k, n0+1, i+1, r+a[0][i]),
        miceAndCheese_helper(a, k, n0, i+1, r+a[1][i])
       );
    }
  }

  public int miceAndCheese_v1(final int[] in1, final int[] in2, int k) {
    int[][] a = new int[][]{ in1, in2 };
    return this.miceAndCheese_helper(a, k, 0, 0, 0);
  }

  public int miceAndCheese_v2(final int[] in1, final int[] in2, final int k) {
    int[] indices = new int[k];
    for (int i = 0; i < k; i++) indices[i] = i;

    final int n = in1.length,
              m = in2.length;
    int result = 0;
    // Generate all combinations of indices
    while (indices[k-1] < n) {
      
      // Calculate new sum
      // dump(indices);
      int s = 0;
      for (int i=0, j=0; j<m;) {
        if (i < k && j == indices[i]) {
          s += in1[indices[i++]];
          j++;
        } else {
          s += in2[j++];
        }
      }
      // System.out.printf("s=%d\n", s);
      result = Math.max(result, s);
      
      int t = k - 1;
      while (t != 0 && indices[t] == n-k+t) t--;
      indices[t]++;
      for (int i = t+1; i<k; i++) indices[i] = indices[i-1]+1;
    }

    return result;
  }

  // Solution 3
  // I realized here that I can memoize the whole in2 computation by
  // pre-calculating the sum of the second array and just selectively
  // subtracting the elements corresponding to the chosen indices for
  // the current combination from the sum and adding the in1 values.
  // It's faster because we can remove a factor of n, but the time-complexity
  // is still the complexity of the combinations algorithm multiplied by k.
  // According to the StackOverflow article I saw it in, the combinations
  // time is O(choose(n, k) * k), so the resultant complexity is probably
  // O(choose(n, k) * k^2) what with the second k-iteration.
  public int miceAndCheese_v3(final int[] in1, final int[] in2, final int k) {
    int result = 0,
        in2Sum = 0;
    for (int i : in2) in2Sum += i;
    if (k == 0) return in2Sum;

    int[] indices = new int[k];
    for (int i = 0; i < k; i++) indices[i] = i;

    final int n = in1.length;
    while (indices[k-1] < n) {
      int sum = in2Sum;
      for (int i = 0; i < k; i++) {
        sum = sum - in2[indices[i]] + in1[indices[i]];
      }
      result = Math.max(result, sum);

      int t = k-1;
      while (t != 0 && indices[t] == n-k+t) t--;
      indices[t]++;
      for (int i = t+1; i<k; i++) indices[i] = indices[i-1]+1;
    }
    return result;
  }

  // Solution 4
  // Greedy. I don't know if this will work, because there are potentially
  // some use-cases where a greedy algorithm will skip valid outcomes, but
  // it's worth a shot.
  //
  // ...it fails on case 3. Greedy algorithm does not work.
  public int miceAndCheese(final int[] in1, final int[] in2, final int k) {
    int[] indices = new int[k];
    for (int i = 0; i < k; i++) indices[i] = i;
    for (int i = k; i < in1.length; i++) {
      int minIndex = -1;
      for (int j = 0; j < k; j++) {
        if (in1[indices[j]] < in1[i] && (minIndex == -1 || in1[indices[j]] < in1[indices[minIndex]]))
          minIndex = j;
      }
      if (minIndex != -1) indices[minIndex] = i;
    }
    dump(indices);
    
    int j = 0;
    int result = 0, result2 = 0;
    for (int i = 0; i < in2.length; i++) {
      if (j < k && i == indices[j]) {
        System.out.printf("j=%d, i=%d\n", j, i);
        j++;
        result += in1[i];
      } else {
        result2 += in2[i];
      }
    }
    System.out.printf("result1=%d, result2=%d\n", result, result2);
    return result + result2;
  }

  public static void runTests() {
    var m = new MiceAndCheese();
    var tests = new Test[] {
      new Test(new int[]{1,1,3,4}, new int[]{4,4,1,1}, 2, 15),
      new Test(new int[]{1,1}, new int[]{1,1}, 2, 2),
      new Test(new int[]{1,4,4,6,4}, new int[]{6,5,3,6,1}, 1, 24),
      // new Test(
      //   new int[]{54,59,94,87,32,10,55,44,21,73,12,70,89,49,13,34,78,20,20,75,90,44,48,74,78,32,70,76,79,49,50,69},
      //   new int[]{78,43,69,22,32,67,65,38,51,4,21,27,82,61,12,85,62,60,67,16,22,3,5,16,13,35,13,41,72,85,20,54},
      //   17,
      //   2053
      // )
    };
    for (var t : tests) {
      var a = m.miceAndCheese(t.in1, t.in2, t.k);
      if (a != t.out) {
        System.out.printf("FAILURE: exp=%d, actual=%d\n", t.out, a);
      } else {
        System.out.println("SUCCESS");
      }
    }
    System.out.println("Done.");
  }

  public static void main(String[] args) {
    runTests();
    // new MiceAndCheese().combinations(5, 3);
  }
}
