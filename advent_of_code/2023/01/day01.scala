
def part1(input: String): Long =
  input.trim().split("\n").map(_.trim).filter(_.nonEmpty).map:
    s => s.filter(_.isDigit)
  .map { s => List(s.head, s.last).mkString.toLong }
  .sum


def transform(s: String): String =
  if s == "" then ""
  else if s.startsWith("one") then   "1" + transform(s.tail)
  else if s.startsWith("two") then   "2" + transform(s.tail)
  else if s.startsWith("three") then "3" + transform(s.tail)
  else if s.startsWith("four") then  "4" + transform(s.tail)
  else if s.startsWith("five") then  "5" + transform(s.tail)
  else if s.startsWith("six") then   "6" + transform(s.tail)
  else if s.startsWith("seven") then "7" + transform(s.tail)
  else if s.startsWith("eight") then "8" + transform(s.tail)
  else if s.startsWith("nine") then  "9" + transform(s.tail)
  else if s.head.isDigit then s.head + transform(s.tail)
  else transform(s.tail)


def part2(input: String): Long =
  input.trim().split("\n").map(_.trim()).filter(_.nonEmpty).map:
    line =>
      val v = transform(line)
      List(v.head, v.last).mkString.toLong
  .sum


@main def main() =
  val debug = false
  val input = if debug
    then """
    1abc2
    pqr3stu8vwx
    a1b2c3d4e5f
    treb7uchet
    """
    else io.Source.fromFile("input.txt").getLines().mkString("\n")
  
  println(s"2023 Day 1, Part 1: ${part1(input)}")
  println(s"2023 Day 1, Part 2: ${part2(input)}")
