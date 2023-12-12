import scala.collection.mutable.HashMap

object Day12 {
  def readInput(input: String): List[(List[String], List[Int])] = {
    input.split("\n")
      .map { line => line.trim() }
      .filter(!_.isEmpty())
      .map { line =>
        val Array(a, b) = line.split(" ")
        (a.map(_.toString()).toList, b.split(",").map(_.toInt).toList)
      }
      .toList
  }

  def dump(input: List[(List[String], List[Int])]): Unit = {
    input
      .map { case (a, b) => s"${a.mkString} ${b.mkString(",")}" }
      .foreach(println)
    println()
  }

  def valid(values: List[String], i: Int): Boolean = {
    (values, i) match {
      case (Nil, 0) => true
      case ("." :: _, 0) => true
      case ("?" :: _, 0) => true
      case ("#" :: _, 0) => false
      case ("#" :: vs, i) => valid(vs, i - 1)
      case ("?" :: vs, i) => valid(vs, i - 1)
      case _ => false
    }
  }

  def arrangements(
    values: List[String],
    numbers: List[Int],
    cache: HashMap[(List[String], List[Int]), Long] = HashMap()
  ): Long = {
    if (cache.contains((values, numbers))) {
      val res = cache.get((values, numbers)).get
      return res
    }

    val res = (values, numbers) match {
      case (Nil, Nil) => 1
      case (vs, Nil) if vs.forall(_ != "#") => 1
      case (_, Nil) => 0
      case (Nil, _) => 0
      case ("." :: vs, ns) => arrangements(vs, ns, cache)
      case (vs@("#" :: _), n :: ns) if valid(vs, n) => 
        vs.drop(n) match {
          case "." :: vvs => arrangements(vvs, ns, cache)
          case "?" :: vvs => arrangements(vvs, ns, cache) // "?" must be a "."
          case Nil if ns.length == 0 => 1
          case _ => 0
        }
      case ("?" :: vs, ns) => 
        arrangements("." :: vs, ns, cache) + arrangements("#" :: vs, ns, cache)
      case _ => 0
    }
    cache.put((values, numbers), res)
    // println(s"cache value $res for input values=${values.mkString}, ns=${numbers.mkString(",")}")
    res
  }

  def part1(input: String): Long = {
    readInput(input)
      .map { case (vs, ns) => arrangements(vs, ns) }
      .sum
  }

  def intercalate[A](a: List[A], b: A): List[A] = {
    a match {
      case head :: Nil => head :: Nil
      case head :: rest => head :: b :: intercalate(rest, b)
      case Nil => Nil
    }
  }

  def part2(input: String): Long = {
    readInput(input)
      .map { case (vs, ns) => 
        (
          intercalate(
            (for (_ <- 0 until 5) yield vs).toList,
            List("?")
          ).flatten,
          (for (_ <- 0 until 5) yield ns).toList.flatten
        )
      }.map { case (vs, ns) => 
        val res = arrangements(vs, ns)
        // println(s"#arrangements = $res")
        res
      }
      .sum
  }

  def main(args: Array[String]): Unit = {
    val debug = false
    val filename = "input.txt"
    val input = 
      if (debug) 
        """
          ???.### 1,1,3
          .??..??...?##. 1,1,3
          ?#?#?#?#?#?#?#? 1,3,1,6
          ????.#...#... 4,1,1
          ????.######..#####. 1,6,5
          ?###???????? 3,2,1
        """.trim()
      else io.Source.fromFile(filename).getLines().mkString("\n")
    println(s"2023 Day 12, Part 1: ${part1(input)}")
    println(s"2023 Day 12, Part 2: ${part2(input)}")
  }

  // def main(args: Array[String]): Unit = {
  //   // val vs = "?###????????".map(_.toString).toList
  //   // val ns = List(3, 2, 1)
  //   // println(arrangements(vs, ns))
  //   val vs = (for (_ <- 0 until 5) yield List("#", "?")).toList
  //   println(intercalate(vs, List("?")).flatten)
  // }
}
