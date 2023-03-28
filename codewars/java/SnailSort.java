
import java.util.function.*;

public class SnailSort {
    public static int[][] rotate(int[][] arr) {
        final int nrows = arr.length;
        final int ncols = arr[0].length;
        int[][] result = new int[ncols][nrows];

        for (int i = 0; i < ncols; i++)
            for (int j = 0; j < nrows; j++)
                result[i][j] = arr[j][ncols - i - 1];

        return result;
    }

    public static void copy(int[] a, int[] b, int offset) {
        for (int i = 0; i < b.length; i++)
            a[i+offset] = b[i];
    }

    public static Function<int[][], int[]> head = (a) -> a[0];
    public static Function<int[][], Integer> size = (a) -> a.length * a[0].length;

    public static int[][] rest(int[][] a) {
        int[][] b = new int[a.length-1][a[0].length];
        for (int i = 0; i < a.length-1; i++)
            for (int j = 0; j < a[0].length; j++)
                b[i][j] = a[i+1][j];
        return b;
    }

    // public static String dumps(int[][] a) {
    //     StringBuilder s = new StringBuilder();
    //     s.append("[");
    //     for (int i = 0; i < a.length; i++) {
    //         s.append("[");
    //         for (int j = 0; j < a[i].length; j++)
    //             s.append(String.format("%d, ", a[i][j]));
    //         s.append("], ");
    //     }
    //     s.append("]");
    //     return s.toString();
    // }


    public static int[] snailSort(int[][] arr) {
        final int n = arr.length * arr[0].length;
        int[] result = new int[n];
        int[][] a = arr;
        int offset = 0;
        while (size.apply(a) != 1) {
            int[] h = head.apply(a);
            copy(result, h, offset);
            offset += h.length;
            a = rotate(rest(a));
        }
        result[n-1] = a[0][0];
        return result;
    }

    // public static void dump(final int[][] arr) {
    //     System.out.print("[");
    //     for (int i = 0; i < arr.length; i++) {
    //         System.out.print("[");
    //         for (int j = 0; j < arr[i].length; j++)
    //             System.out.printf("%d, ", arr[i][j]);
    //         System.out.print("]\n");
    //     }
    //     System.out.println("]");
    // }

    // public static void dump(final int[] arr) {
    //     System.out.print("[");
    //     for (int i = 0; i < arr.length; i++)
    //         System.out.printf("%d, ", arr[i]);
    //     System.out.print("]\n");
    // }

    public static void main(final String[] args) {
        int[][] input = new int[][] {
            {1,2,3},
            {4,5,6},
            {7,8,9}
        };
        snailSort(input);
        // dump(snailSort(input));
    }
}