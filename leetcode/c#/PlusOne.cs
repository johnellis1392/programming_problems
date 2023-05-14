using System;
using System.Text;
using System.Collections.Generic;

public class PlusOneSolution {
  public struct Test {
    public int[] i;
    public int[] o;
    public Test(int[] i, int[] o) {
      this.i = i;
      this.o = o;
    }
  }

  public static Test[] tests = new [] {
    new Test(new []{1,2,3}, new []{1,2,4}),
    new Test(new []{4,3,2,1}, new []{4,3,2,2}),
    new Test(new []{9}, new []{1,0}),
  };

  public static int[] PlusOne(int[] digits) {
    var l = new List<int>();
    int c = 1, i = digits.Length-1;
    while (i >= 0) {
      l.Insert(0, (digits[i] + c) % 10);
      c = (digits[i] + c) / 10;
      i--;
    }
    if (c > 0) l.Insert(0, c);
    return l.ToArray();
  }

  public static int[] PlusOne2(int[] digits) {
    for (int i = digits.Length - 1; i >= 0; i--) {
      if (digits[i] == 9) {
        digits[i] = 0;
      } else {
        digits[i]++;
        return digits;
      }
    }
    if (digits[0] > 0) return digits;
    else {
      var a = new int[digits.Length+1];
      a[0] = 1;
      Array.Copy(digits, 0, a, 1, digits.Length);
      return a;
    }
  }

  public static bool equals(int[] a, int[] b) {
    if (a.Length != b.Length) return false;
    for (int i = 0; i < a.Length; i++)
      if (a[i] != b[i])
        return false;
    return true;
  }

  public static string format(int[] a) {
    var sb = new StringBuilder();
    sb.Append("[");
    for (int i = 0; i < a.Length; i++) {
      if (i != 0) sb.Append(", ");
      sb.Append(a[i]);
    }
    sb.Append("]");
    return sb.ToString();
  }

  public static void Main(string[] args) {
    Console.WriteLine("Running...");
    foreach (var t in tests) {
      var r = PlusOne(t.i);
      if (equals(r, t.o)) {
        Console.WriteLine("Success");
      } else {
        Console.WriteLine(
          String.Format("Failure: {0} != {1}", format(r), format(t.o))
        );
      }
    }
  }
}