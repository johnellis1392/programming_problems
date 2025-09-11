public class TwoSum {
  public static int[] twoSum(final int[] nums, final int target) {
    for (int i = 0; i < nums.length; i++) {
      for (int j = i+1; j < nums.length; j++) {
        if (nums[i] + nums[j] == target)
          return new int[] { nums[i], nums[j] };
      }
    }
    return null;
  }

  public static void main(final String[] args) {
    System.out.println(
      twoSum(new int[] { 2, 7, 11, 15 }, 9)
    );
  }
}
