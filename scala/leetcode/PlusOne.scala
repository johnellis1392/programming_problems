object PlusOne {
  def tests: Array[(Array[Int], Array[Int])] = Array(
    (Array[Int](1, 2, 3), Array[Int](1, 2, 4)),
    (Array[Int](4, 3, 2, 1), Array[Int](4, 3, 2, 2)),
    (Array[Int](9), Array[Int](1, 0))
  )

  def plusOne(digits: Array[Int]): Array[Int] = {
    def f(v: List[Int], c: Int): List[Int] = (v, c) match {
      case (Nil, 0) => Nil
      case (Nil, 1) => List(1)
      case (h :: t, 0) => h :: t
      case (h :: t, 1) if h < 9 => h + 1 :: t
      case (h :: t, 1) => 0 :: f(t, 1)
      case _ => Nil
    }
    f(digits.toList.reverse, 1).reverse.toArray
  }

  def format(v: Array[Int]): String = s"[${v.map(_.toString).mkString(", ")}]"

  implicit class ArrayEq(a: Array[Int]) {
    def aeq(b: Array[Int]): Boolean = 
      if (a.length != b.length) return false
      else a.zip(b).forall(_ == _)
  }

  def main(args: Array[String]): Unit = {
    println("Running...")
    tests foreach {
      (i, o) =>
        val r = plusOne(i)
        if (r.aeq(o)) {
          println("Success")
        } else {
          println(s"Failure: ${format(r)} != ${format(o)}")
        }
    }
  }
}