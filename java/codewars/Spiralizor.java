import java.util.Arrays;

public class Spiralizor {
  public static int[][] makeArray(final int size) {
    var arr = new int[size][size];
    for (int i = 0; i < size; i++)
      for (int j = 0; j < size; j++)
        arr[i][j] = 0;
    return arr;
  }

  public static void debug(final int[][] arr) {
    System.out.println("\n --- DEBUG ---");
    System.out.printf("SIZE = %dx%d\n", arr.length, arr[0].length);
    System.out.println(dumps(arr));
    System.out.println(" --- END DEBUG ---\n\n");
  }

  public static int[][] spiralize(final int size) { // size = 10
    var arr = makeArray(size); //10x10
    int i, j, n, offset;

    for (offset = 0; offset < (int)(size/2); offset += 2) { // offset = 2
      n = size - offset; // n = 6

      // Skip
      // System.out.printf(" *** Offset = %d\n", offset);
      if (
        arr[offset][offset+1] == 1 ||
        arr[offset+1][offset+1] == 1 ||
        arr[offset+1][offset] == 1
      ) { 
        // System.out.printf(" *** SKIPPING: offset = %d\n", offset);
        break;
      }

      for (i = offset, j = offset; j < n; j++) arr[i][j] = 1; // i = 2, j = 2, n = 6
      for (i = offset, j = size-1-offset; i < n; i++) arr[i][j] = 1; // i = 2, j = 7
      for (i = size-1-offset, j = size-1-offset; j >= offset; j--) arr[i][j] = 1; // i = 7, j = 7
      for (i = size-1-offset, j = offset; i >= offset+2; i--) arr[i][j] = 1; // i = 7, j = 2
      if (
        arr[offset+3][offset+1] != 1 &&
        arr[offset+3][offset+2] != 1 &&
        arr[offset+2][offset+1] != 1
      ) arr[offset+2][offset+1] = 1;
      // debug(arr);
    }

    // Edge case
    // if (size % 2 == 0) {
    //   // Size is even, so center is a square, so fill like so:
    //   // 1 1
    //   // 0 1
    //   offset = (int)(size/2)-1;
    //   // System.out.printf("size = %d, offset = %d\n", size, offset);
    //   arr[offset][offset] = 1;
    //   arr[offset][offset+1] = 1;
    //   arr[offset+1][offset+1] = 1;
    // } else {
    //   // Size is odd, so center is a single point, so fill it in
    //   offset = (int)(size/2);
    //   arr[offset][offset] = 1;
    // }

    debug(arr);
    return arr;
  }
  
  public static String dumps(final int[][] arr) {
    var sb = new StringBuilder();
    sb.append("[");
    for (int i = 0; i < arr.length; i++) {
      sb.append("[");
      for (int j = 0; j < arr[i].length; j++)
        sb.append(String.format("%d, ", arr[i][j]));
      sb.append("]\n");
    }
    sb.append("]");
    return sb.toString();
  }
  
  public static void assertEquals(
    final int[][] expected,
    final int[][] actual
  ) {
    final boolean equal = expected.equals(actual);
    System.out.printf(
      " ---\n%s\n==\n%s\n=> %s\n",
      dumps(expected),
      dumps(actual),
      Boolean.toString(equal)
    );
  }

  public static void main(final String[] args) {
    // assertEquals(
    //   new int[][] {
    //     { 1, 1, 1, 1, 1 },
    //     { 0, 0, 0, 0, 1 },
    //     { 1, 1, 1, 0, 1 },
    //     { 1, 0, 0, 0, 1 },
    //     { 1, 1, 1, 1, 1 }
    //   },  
    //   spiralize(5)
    // );

    for (int i = 5; i < 12; i++) spiralize(i);
    // spiralize(10);

    // assertEquals(
    //   new int[][] {
    //           { 1, 1, 1, 1, 1, 1, 1, 1 },
    //           { 0, 0, 0, 0, 0, 0, 0, 1 },
    //           { 1, 1, 1, 1, 1, 1, 0, 1 },
    //           { 1, 0, 0, 0, 0, 1, 0, 1 },
    //           { 1, 0, 1, 0, 0, 1, 0, 1 },
    //           { 1, 0, 1, 1, 1, 1, 0, 1 },
    //           { 1, 0, 0, 0, 0, 0, 0, 1 },
    //           { 1, 1, 1, 1, 1, 1, 1, 1 },
    //   },
    //   spiralize(8)
    // );
  }
}
