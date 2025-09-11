object TwoSum {
  case class T(in: (Array[Int], Int), out: Array[Int])

  implicit class ArrayToString(a: Array[Int]) {
    def str(): String = s"[${a.map(_.toString).mkString(", ")}]"
  }

  // Solution 1: Allows for repeated elements, ie fails second
  // case with the technically valid answer [0, 0]
  def twoSum_v1(nums: Array[Int], target: Int): Array[Int] = {
    val ns = nums.zipWithIndex
    val indices = ns.map({ case (a, i) => (target - a) -> i }).toMap
    for ((b, i) <- ns)
      if (indices contains b)
        return Array(i, indices(b))
    return Array()
  }

  // Solution 2: Iterative solution which kinda sucks, cuz it's not functional
  def twoSum_v2(nums: Array[Int], target: Int): Array[Int] = {
    var m = scala.collection.mutable.Map[Int, Int]()
    for ((a, i) <- nums.zipWithIndex) {
      if (m contains a)
        return Array(m(a), i)
      else
        m.addOne((target - a) -> i)
    }
    println(m)
    return Array()
  }

  // Solution 3: Recursive Method
  def twoSum(nums: Array[Int], target: Int): Array[Int] = {
    def f(nums: List[(Int, Int)], m: Map[Int, Int]): Array[Int] = {
      nums match {
        case Nil => Array()
        case (a, i) :: _ if (m contains a) => Array(m(a), i)
        case (a, i) :: rest => f(rest, m + ((target - a) -> i))
      }
    }
    f(nums.zipWithIndex.toList, Map())
  }

  // Saw this on LeetCode; clever use of pattern matching.
  // Though I think this would technically throw if it reaches
  // the end of this because of an index out of bounds or some
  // math exception.
  def twoSum_v4(nums: Array[Int], target: Int): Array[Int] = {
    def f(i: Int, map: Map[Int, Int]): Array[Int] = {
      map.get(target - nums(i)) match {
        case Some(j) => Array(i, j)
        case None => f(i+1, map + (nums(index) -> i))
      }
    }
    f(0, Map.empty)
  }

  def main(args: Array[String]): Unit = {
    List(
      T((Array(2, 7, 11, 15), 9), Array(0, 1)),
      T((Array(3, 2, 4), 6), Array(1, 2)),
      T((Array(3, 3), 6), Array(0, 1))
    ) foreach { case T((a, target), exp) =>
      val actual = twoSum(a, target)
      println(s"${if(java.util.Arrays.equals(actual, exp)) "SUCCESS" else "FAILURE"} - ${actual.str()} should equal ${exp.str()}")
    }
  }
}