public class TwoSum {
  public static int[] twoSum(final int[] nums, final int target) {
    for (int i = 0; i < nums.length; i++) {
      for (int j = i+1; j < nums.length; j++)
        if (nums[i] + nums[j] == target)
          return new int[] { i, j };
    }
    return null;
  }

  public static String dumps(final int[] nums) {
    var sb = new StringBuilder();
    sb.append("[");
    for (var i : nums) sb.append(String.format("%d, ", i));
    sb.append("]");
    return sb.toString();
  }

  public static void test(final int[] nums, final int target) {
    System.out.println(
      dumps(
        twoSum(nums, target)
      )
    );
  }

  public static void main(final String[] args) {
    test(new int[] { 2, 7, 11, 15 }, 9);
  }
}