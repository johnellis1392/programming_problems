using System;

public class StrStrSolution {
  public struct Test {
    public string haystack;
    public string needle;
    public int exp;
    public Test(string haystack, string needle, int exp) {
      this.haystack = haystack;
      this.needle = needle;
      this.exp = exp;
    }

    public (string, string, int) tuple {
      get {
        return (
          this.haystack,
          this.needle,
          this.exp
        );
      }
    }
  }

  public static Test[] tests = new Test[] {
    new Test("sadbutsad", "sad", 0),
    new Test("leetcode", "leeto", -1),
    new Test("a", "a", 0),
    new Test("ba", "a", 1),
  };

  public static int strStr(string haystack, string needle) {
    if (needle.Length == 0 || needle.Length > haystack.Length)
      return -1;
    int n = haystack.Length, m = needle.Length;
    for (int i = 0; i < n-m+1; i++)
      if (haystack.Substring(i, m) == needle)
        return i;
    return -1;
  }

  // Apparently you can also just do this
  public static int _strStr(string haystack, string needle) => haystack.IndexOf(needle);

  public static void Main(string[] args) {
    Console.WriteLine("Running...");
    foreach (var test in tests) {
      var (haystack, needle, exp) = test.tuple;
      var result = strStr(haystack, needle);
      if (result == exp) {
        Console.WriteLine("Success");
      } else {
        Console.WriteLine(
          String.Format(
            "Failure: {0} != {1}",
            result, exp
          )
        );
      }
    }
  }
}