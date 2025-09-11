package aoc2015

val _vowels = Set('a', 'e', 'i', 'o', 'u')
extension(self: Char)
  def isVowel = _vowels.contains(self.toLower)

extension(self: String)
  def hasDouble: Boolean =
    {
      for
        i <- self.indices.init
        if self(i) == self(i + 1)
      yield self(i)
    }.nonEmpty

  def naughty = Set("ab", "cd", "pq", "xy").exists(self.contains(_))

  def nice =
    self.filter(_.isVowel).size >= 3 &&
    self.hasDouble &&
    !self.naughty

  def hasRepeat =
    {
      for
        i <- self.indices.init
        s = s"${self(i)}${self(i+1)}"
        if self.slice(i + 2, self.size).contains(s)
      yield s
    }.nonEmpty

  def hasTriple =
    {
      for
        i <- self.indices.init.init
        if self(i) == self(i+2)
      yield s"${self(i)}${self(i+1)}${self(i+2)}"
    }.nonEmpty

  def nicer = self.hasRepeat && self.hasTriple


def part1(input: Array[String]): Long = input.count(_.nice)

def part2(input: Array[String]): Long = input.count(_.nicer)

def main =
  val input = io.Source.fromFile("input/aoc2015/day05.input.txt").getLines().toArray
  println(s"2015 Day 5, Part 1: ${part1(input)}")
  println(s"2015 Day 5, Part 2: ${part2(input)}")
