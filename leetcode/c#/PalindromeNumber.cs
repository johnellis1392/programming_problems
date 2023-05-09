using System;

public class PalindromeNumber {
  public struct Test {
    public int input;
    public bool output;
    public Test(int input, bool output) {
      this.input = input;
      this.output = output;
    }
  }

  public static Test[] tests = new Test[]{
    new Test(5, true),
    new Test(121, true),
    new Test(-121, false),
    new Test(10, false),
  };

  public bool IsPalindrome(int x) {
    if (x < 0) return false;
    var s = x.ToString();
    var n = s.Length;
    for (int i = 0; i < n/2; i++) {
      if (s[i] != s[n - i - 1]) return false;
    }
    return true;
  }

  public static void Main(string[] args) {
    Console.WriteLine("Running...");
    var m = new PalindromeNumber();
    foreach (var t in tests) {
      var actual = m.IsPalindrome(t.input);
      if (actual == t.output) {
        Console.WriteLine("Success");
      } else {
        Console.WriteLine(String.Format("Failure: {0} != {1}", actual, t.output));
      }
    }
  }
}