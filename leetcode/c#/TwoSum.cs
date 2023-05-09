using System;
using System.Collections.Generic;
using System.Text;

public struct Test {
  public int[] nums;
  public int target;
  public int[] exp;
  public Test(int[] nums, int target, int[] exp) {
    this.nums = nums;
    this.target = target;
    this.exp = exp;
  }
}

public class TwoSum {
  private static Test[] tests = {
    new Test(new int[]{2, 7, 11, 15}, 9, new int[]{0, 1}),
    new Test(new int[]{3, 2, 4}, 6, new int[]{1, 2}),
    new Test(new int[]{3, 3}, 6, new int[]{0, 1}),
  };

  private String ToString(int[] v) {
    var sb = new StringBuilder();
    sb.Append("[");
    foreach (var i in v) {
      sb.Append(i + ", ");
    }
    sb.Append("]");
    return sb.ToString();
  }

  public int[] twoSum(int[] nums, int target) {
    var m = new Dictionary<int, int>();
    for (int i = 0; i < nums.Length; i++) {
      if (m.TryGetValue(nums[i], out int v)) {
        return new int[] {v, i};
      } else {
        m[target - nums[i]] = i;
      }
    }
    return new int[] {};
  }

  public bool Equals(int[] a, int[] b) {
    if (a.Length != b.Length)
      return false;
    for (int i = 0; i < a.Length; i++)
      if (a[i] != b[i])
        return false;
    return true;
  }

  public void RunTest(Test test) {
    int[] actual = twoSum(test.nums, test.target);
    if (Equals(actual, test.exp)) {
      Console.WriteLine("Success");
    } else {
      Console.WriteLine(
        String.Format("Failure: {0} != {1}", ToString(actual), ToString(test.exp))
      );
    }
  }

  public static void Main(string[] args) {
    Console.WriteLine("Hello, World!");
    var t = new TwoSum();
    foreach (var test in tests) {
      t.RunTest(test);
    }
  }
}