package aoc2015


val vowels = Set('a', 'e', 'i', 'o', 'u')

extension(c: Char)
  def isVowel: Boolean = vowels.contains(c)

def part1(input: String): Long =
  def check(s: List[Char], numVowels: Int, numDoubles: Int): Boolean =
    s match
      case Nil => numVowels >= 3 && numDoubles >= 1
      case 'a' :: 'b' :: _ => false
      case 'c' :: 'd' :: _ => false
      case 'p' :: 'q' :: _ => false
      case 'x' :: 'y' :: _ => false
      case a :: b :: rest =>
        val vs = if a.isVowel then numVowels + 1 else numVowels
        val ds = if a == b then numDoubles + 1 else numDoubles
        check(b :: rest, vs, ds)
      case a :: Nil =>
        val vs = if a.isVowel then numVowels + 1 else numVowels
        check(Nil, vs, numDoubles)
  input.trim().split("\n").map(_.trim()).filter(_.nonEmpty).filter:
    line =>
      check(line.toList, 0, 0)
  .size

def part2(input: String): Long =
  def check1(s: List[Char]): Boolean =
    s match
      case Nil => false
      case _ :: Nil => false
      case a :: b :: rest if rest.containsSlice(List(a, b)) => true
      case _ :: b :: rest => check1(b :: rest)

  def check2(s: List[Char]): Boolean =
    s match
      case Nil => false
      case _ :: Nil => false
      case _ :: _ :: Nil => false
      case a :: b :: c :: rest if a == c => true
      case _ :: b :: c :: rest => check2(b :: c :: rest)

  input.trim().split("\n").map(_.trim()).filter(_.nonEmpty).filter:
    line =>
      check1(line.toList) && check2(line.toList)
  .size

@main def main() =
  val debug = false
  val input = if debug
    then """
    ugknbfddgicrmopn
    aaa
    jchzalrnumimnmhp
    haegwjzuvuyypxyu
    dvszwmarrgswjxmb
    """
    else io.Source.fromFile("input.txt").getLines().mkString("\n")
  println(s"2015 Day 5, Part 1: ${part1(input)}")
  println(s"2015 Day 5, Part 2: ${part2(input)}")
