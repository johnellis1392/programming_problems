public class MedianOfTwoSortedArrays {
  public static double findMedianSortedArrays(int[] nums1, int[] nums2) {
    int[] arr = new int[nums1.length+nums2.length];
    int i = 0, j = 0, k = 0;
    while(i < nums1.length && j < nums2.length) {
      if(nums1[i] < nums2[j]) arr[k++] = nums1[i++];
      else arr[k++] = nums2[j++];
    }

    if (i < nums1.length) for (; i < nums1.length;) arr[k++] = nums1[i++];
    else if (j < nums2.length) for (; j < nums2.length;) arr[k++] = nums2[j++];

    System.out.println(dumps(arr));
    var n = (int)(arr.length/2);
    if (arr.length % 2 == 0) return (arr[n] + arr[n-1]) / 2.0;
    else return arr[n];
  }

  public static String dumps(final int[] nums) {
    var sb = new StringBuilder();
    sb.append("[");
    for (var i : nums) sb.append(String.format("%d, ", i));
    sb.append("]");
    return sb.toString();
  }

  public static void test(final int[] nums1, final int[] nums2) {
    System.out.printf(
      "result = %f\n",
      findMedianSortedArrays(nums1, nums2)
    );
  }

  public static void  main(final String[] args) {
    // test(new int[] { 1, 3 }, new int[] { 2, 4 });
    test(new int[] {}, new int[] { 2, 3 });
  }
}
