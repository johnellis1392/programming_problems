object LongestUniqueSubstring {
  case class T(in: String, out: Int)

  // Solution 1: Pretty idiomatic, but fails 4th test case;
  // clearing the set messes with the counting.
  def lengthOfLongestSubstring_v1(s: String): Int = {
    def f(in: Set[Char], rest: List[Char], n: Int): Int = {
      rest match {
        case Nil => Math.max(in.size, n)
        case head :: tail if in.contains(head) => f(in.empty + head, tail, Math.max(in.size, n))
        case head :: tail => f(in + head, tail, n)
      }
    }
    f(Set.empty, s.toList, 0)
  }

  // Solution 2: Working solution
  def lengthOfLongestSubstring(s: String): Int = {
    def f(in: String, rest: List[Char], n: Int): Int = {
      rest match {
        case Nil => Math.max(in.size, n)
        case head :: tail if in.contains(head) => f(in.substring(in.indexOf(head)+1)+head, tail, Math.max(in.size, n))
        case head :: tail => f(in + head, tail, n)
      }
    }
    f("", s.toList, 0)
  }

  // Solution 3: From LeetCode; a simple array-based method, minimal memory use.
  // This is the fastest and leanest solution on LeetCode right now apparently.
  def lengthOfLongestSubstring_v3(s: String): Int = {
    val visited = Array.fill(128)(-1)
    var left = 0
    var length = 0
    s.indices.foreach { right => 
      val c = s(right).toInt
      left = (visited(c) + 1) max left
      visited(c) = right
      length = (right - left + 1) max length
    }
    length
  }

  def main(args: Array[String]): Unit = {
    List(
      T("abcabcbb", 3),
      T("bbbbb", 1),
      T("pwwkew", 3),
      T("dvdf", 3),
    ) foreach { case T(in, out) =>
      val actual = lengthOfLongestSubstring(in)
      println(s"${if(actual.equals(out)) "SUCCESS" else "FAILURE"} - ${actual} should equal ${out}")
    }
  }
}
