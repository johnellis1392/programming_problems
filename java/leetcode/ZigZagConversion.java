import java.util.Arrays;

public class ZigZagConversion {
  public Character[][] genArray(final String s, final int n) {
    var numCols = 0;
    var i = s.length();
    var j = 0;
    while (i > 0) {
      if (j == 0) {
        numCols++;
        i -= n;
        j++;
      } else if (j < n)  {
        numCols++;
        i -= 1;
        j++;
      } else {
        j = 0;
      }
    }
    var result = new Character[n][numCols];
    for (var a : result) Arrays.fill(a, '\0');
    return result;
  }

  public String dumps(final Character[][] arr) {
    var sb = new StringBuilder();
    sb.append(String.format("Size = (%d, %d)\n", arr.length, arr[0].length));
    sb.append("[");
    for (var i : arr) {
      sb.append("[");
      for (var j : i) {
        if (j == '\0') sb.append("'\\0', ");
        else sb.append(j + ", ");
      }
      sb.append("]\n");
    }
    sb.append("]");
    return sb.toString();
  }

  public void drawString(Character[][] arr, final String s) {
    final var n = s.length();
    final var nRows = arr.length;
    boolean reverse = false;
    int i = 0, row = 0, col = 0;
    while (i < n) {
      char c = s.charAt(i);
      i++;
      if (row == 0) reverse = false;
      if (reverse) {
        arr[row][col] = c;
        row = Math.max(0, row-1);
        col += 1;
      } else {
        arr[row][col] = c;
        row++;
        if (row >= nRows) {
          row = Math.max(0, row-2);
          col++;
          reverse = true;
        }
      }
    }
  }

  public String renderString(final Character[][] arr) {
    var sb = new StringBuilder();
    for (int i = 0; i < arr.length; i++) {
      for (int j = 0; j < arr[i].length; j++) {
        if (arr[i][j] != '\0') sb.append(arr[i][j]);
      }
    }
    return sb.toString();
  }

  public String convert(final String s, final int n) {
    var arr = this.genArray(s, n);
    this.drawString(arr, s);
    // System.out.println(dumps(arr));
    return this.renderString(arr);
  }

  public static void main(final String[] args) {
    final var main = new ZigZagConversion();
    System.out.println(main.convert("PAYPALISHIRING", 3));
    System.out.println(main.convert("PAYPALISHIRING", 4));
    System.out.println(main.convert("ABCD", 2));
  }
}
