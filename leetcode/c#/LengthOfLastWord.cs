using System;

public class LengthOfLastWordSolution {
  public struct Test {
    public string s;
    public int o;
    public Test(string s, int o) {
      this.s = s;
      this.o = o;
    }
  }

  public static Test[] tests = new Test[] {
    new Test("Hello World", 5),
    new Test("   fly me   to   the moon  ", 4),
    new Test("luffy is still joyboy", 6),
  };

  public static int LengthOfLastWord(string s) {
    var v = s.TrimEnd().Split(' ', StringSplitOptions.RemoveEmptyEntries);
    return v.Length > 0 ? v[v.Length - 1].Length : 0;
  }

  public static void Main(string[] args) {
    Console.WriteLine("Running...");
    foreach (var t in tests) {
      var res = LengthOfLastWord(t.s);
      if (res == t.o) {
        Console.WriteLine("Success");
      } else {
        Console.WriteLine(
          String.Format(
            "Failure: {0} != {1}",
            res, t.o
          )
        );
      }
    }
  }
}