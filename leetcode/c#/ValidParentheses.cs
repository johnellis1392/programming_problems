using System;
using System.Collections.Generic;

public class ValidParentheses {
  struct Test {
    public string input;
    public bool output;
    public Test(string input, bool output) {
      this.input = input;
      this.output = output;
    }
  }

  static Test[] tests = new Test[] {
    new Test("()", true),
    new Test("()[]{}", true),
    new Test("(]", false),
    new Test("]", false),
  };

  static bool isValid(string s) {
    var stack = new Stack<char>();
    for (int i = 0; i < s.Length; i++) {
      if (s[i] == '(' || s[i] == '[' || s[i] == '{') {
        stack.Push(s[i]);
      } else if (stack.Count == 0) {
        return false;
      } else if (
        (s[i] == ')' && stack.Peek() == '(') ||
        (s[i] == ']' && stack.Peek() == '[') ||
        (s[i] == '}' && stack.Peek() == '{')
      ) {
        stack.Pop();
      } else {
        return false;
      }
    }
    return stack.Count == 0;
  }

  public static void Main(string[] args) {
    Console.WriteLine("Running...");
    foreach (var test in tests) {
      var result = isValid(test.input);
      if (result == test.output) {
        Console.WriteLine("Success");
      } else {
        Console.WriteLine(
          String.Format(
            "Failure: {0} != {1}",
            result,
            test.output
          )
        );
      }
    }
  }
}