package aoc2023

import scala.math.max


case class Roll(r: Int, g: Int, b: Int):
  override def toString() = s"$r red, $g green, $b blue"

case class Game(id: Int, rolls: Array[Roll]):
  override def toString() = s"Game $id: ${rolls.map(_.toString()).mkString("; ")}"
  def valid(rmax: Int, gmax: Int, bmax: Int) =
    rolls.forall:
      case Roll(r, g, b) => r <= rmax && g <= gmax && b <= bmax


def readInput(input: String): Array[Game] =
  val game_re = "^Game (\\d+): (.*)$".r
  input.trim().split("\n").map(_.trim()).filter(_.nonEmpty).map:
    case game_re(id, game_s) =>
      val rolls = game_s.split("; ").map:
        roll_s =>
          var r = 0
          var g = 0
          var b = 0
          roll_s.split(", ").foreach:
            roll => 
              roll.split(" ") match
                case Array(n, "red") => r = n.toInt
                case Array(n, "green") => g = n.toInt
                case Array(n, "blue") => b = n.toInt
          Roll(r, g, b)
      Game(id.toInt, rolls)


def part1(input: String): Int =
  readInput(input).filter:
    game => game.valid(12, 13, 14)
  .map:
    case Game(id, _) => id
  .sum


def part2(input: String): Int =
  readInput(input).map:
    case Game(_, rolls) =>
      rolls.foldLeft((0, 0, 0)):
        case ((rmax, gmax, bmax), Roll(r, g, b)) =>
          (max(rmax, r), max(gmax, g), max(bmax, b))
  .map:
    (r, g, b) => r * g * b
  .sum


@main def main() =
  val debug = false
  val input = if debug
    then """
      Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
      Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
      Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
      Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
      Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green
    """
    else io.Source.fromFile("input.txt").getLines().mkString("\n")
  println(s"2023 Day 2, Part 1: ${part1(input)}")
  println(s"2023 Day 2, Part 2: ${part2(input)}")
