using System;
using System.Text;

public class RemoveElementClass {
  public struct Test {
    public int[] nums;
    public int val;
    public int k;
    public int[] exp;
    public Test(int[] nums, int val, int k, int[] exp) {
      this.nums = nums;
      this.val = val;
      this.k = k;
      this.exp = exp;
    }
  }

  public static Test[] tests = new Test[] {
    new Test(new int[]{3,2,2,3}, 3, 2, new int[]{2, 2}),
    new Test(new int[]{0,1,2,2,3,0,4,2}, 2, 5, new int[]{0,1,4,0,3}),
    // new Test(new int[]{}, 0, 0, new int[]{}),
  };

  public static int RemoveElement2(int[] nums, int val) {
    int k = nums.Length;
    int i = 0;
    while (i < k) {
      if (nums[i] == val) {
        int temp = nums[i];
        for (int j = i; j < k-1; j++)
          nums[j] = nums[j+1];
        nums[k-1] = temp;
        k--;
      } else {
        i++;
      }
    }
    return k;
  }

  public static int RemoveElement(int[] nums, int val) {
    int k = nums.Length;
    int i = 0;
    while (i < k) {
      if (nums[i] == val) {
        int temp = nums[i];
        nums[i] = nums[k-1];
        nums[k-1] = temp;
        k--;
      } else {
        i++;
      }
    }
    return k;
  }

  public static bool Equals(int[] a, int[] b, int k) {
    if (a.Length < k || b.Length < k) return false;
    for (int i = 0; i < k; i++)
      if (a[i] != b[i]) return false;
    return true;
  }

  public static string f(int[] n) {
    var sb = new StringBuilder();
    sb.Append("[");
    for (int i = 0; i < n.Length; i++) {
      if (i != 0) sb.Append(", ");
      sb.Append(n[i]);
    }
    sb.Append("]");
    return sb.ToString();
  }

  public static void Main(string[] args) {
    Console.WriteLine("Running...");
    foreach (var test in tests) {
      var actual = RemoveElement(test.nums, test.val);
      if (actual == test.k && Equals(test.nums, test.exp, test.k)) {
        Console.WriteLine("Success");
      } else {
        Console.WriteLine(
          String.Format(
            "Failure: {0} != {1}, {2} != {3}",
            actual, test.k, f(test.nums), f(test.exp)
          )
        );
      }
    }
  }
}