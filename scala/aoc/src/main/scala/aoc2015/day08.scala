package aoc2015


def part1(input: Array[String]): Long =
  def escapeLength(l: List[Char]): Long =
    l match
      case Nil => 0
      case '"' :: Nil => 0
      case '"' :: rest => escapeLength(rest)
      case '\\' :: '\\' :: rest => 1 + escapeLength(rest)
      case '\\' :: '"' :: rest => 1 + escapeLength(rest)
      case '\\' :: 'x' :: _ :: _ :: rest => 1 + escapeLength(rest)
      case _ :: rest => 1 + escapeLength(rest)
  input.map:
    line =>
      line.length() - escapeLength(line.toList)
  .sum

def part2(input: Array[String]): Long =
  def escapeLength(l: List[Char]): Long =
    l match
      case Nil => 0
      case '"' :: rest => 2 + escapeLength(rest)
      case '\\' :: rest => 2 + escapeLength(rest)
      case _ :: rest => 1 + escapeLength(rest)
  input.map:
    line => (2 + escapeLength(line.toList)) - line.length()
  .sum

@main def main =
  val input = io.Source.fromFile("input.txt").getLines()
    .map(_.trim()).filter(_.nonEmpty)
    .toArray
  println(s"2015 Day 8, Part 1: ${part1(input)}")
  println(s"2015 Day 8, Part 2: ${part2(input)}")
