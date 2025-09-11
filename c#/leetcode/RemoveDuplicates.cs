using System;
using System.Text;

public class RemoveDuplicatesSolution {
  public struct Test {
    public int[] nums;
    public int k;
    public int[] exp;
    public Test(int[] nums, int k, int[] exp) {
      this.nums = nums;
      this.k = k;
      this.exp = exp;
    }
  }

  public static Test[] tests = new Test[] {
    new Test(new int[]{1, 2, 3}, 3, new int[]{1, 2, 3}),
    new Test(new int[]{1, 1, 2}, 2, new int[]{1, 2}),
    new Test(new int[]{0, 0, 1, 1, 1, 2, 2, 3, 3, 4}, 5, new int[]{0, 1, 2, 3, 4}),
  };

  public static int RemoveDuplicates(int[] nums) {
    int k = nums.Length;
    int i = 0;
    while (i < k-1) {
      if (nums[i] == nums[i+1]) {
        var temp = nums[i];
        for (int j = i; j < k - 1; j++) {
          nums[j] = nums[j+1];
        }
        nums[k-1] = temp;
        k--;
      } else {
        i++;
      }
    }
    return k;
  }

  public static bool equals(int[] nums, int[] exp, int k) {
    for (int i = 0; i < k; i++) {
      if (nums[i] != exp[i]) {
        return false;
      }
    }
    return true;
  }

  public static string toString(int[] a) {
    var s = new StringBuilder();
    s.Append("[");
    for (int i = 0; i < a.Length; i++) {
      if (i != 0) s.Append(", ");
      s.Append(a[i].ToString());
    }
    s.Append("]");
    return s.ToString();
  }

  public static void Main(String[] args) {
    Console.WriteLine("Running...");
    foreach (var test in tests) {
      var res = RemoveDuplicates(test.nums);
      if (res == test.k && equals(test.nums, test.exp, test.k)) {
        Console.WriteLine("Success");
      } else {
        Console.WriteLine(
          String.Format(
            "Failure: {0} != {1}, {2} != {3}",
            res,
            test.k,
            toString(test.nums),
            toString(test.exp)
          )
        );
      }
    }
  }
}