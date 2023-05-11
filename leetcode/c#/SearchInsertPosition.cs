using System;

public class SearchInsertPositionSolution {
  public struct Test {
    public int[] nums;
    public int target;
    public int exp;
    public Test(int[] nums, int target, int exp) {
      this.nums = nums;
      this.target = target;
      this.exp = exp;
    }
  }

  public static Test[] tests = new Test[] {
    new Test(new int[]{1,3,5,6}, 5, 2),
    new Test(new int[]{1,3,5,6}, 2, 1),
    new Test(new int[]{1,3,5,6}, 7, 4),
  };

  public static int SearchInsert(int[] nums, int target) {
    int i = 0, j = nums.Length;
    while (i < j) {
      int mid = (i + j) / 2;
      if (nums[mid] == target) {
        return mid;
      } else if (nums[mid] < target) {
        i = mid + 1;
      } else {
        j = mid;
      }
    }
    return i;
  }

  public static void Main(string[] args) {
    Console.WriteLine("Running...");
    foreach (var test in tests) {
      var result = SearchInsert(test.nums, test.target);
      if (result == test.exp) {
        Console.WriteLine("Success");
      } else {
        Console.WriteLine(
          String.Format("Failure: {0} != {1}", result, test.exp)
        );
      }
    }
  }
}