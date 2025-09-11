package aoc2022

import scala.math.{min, max}
import scala.collection.mutable.HashSet
import scala.collection.mutable.HashMap


object Valve:
  type Id = String

case class Valve(id: Valve.Id, rate: Long, conns: Array[String]):
  override def toString(): String =
    s"Valve($id, rate=$rate, conns=[${conns.mkString(",")}])"


def readInput(input: String): Map[Valve.Id, Valve] =
  val line_re = "^Valve (\\w+) has flow rate=(\\d+); tunnels? leads? to valves? (.*)$".r
  input.trim().split("\n").map(_.trim()).filter(_.nonEmpty).map:
    case line_re(id, rate, conns) => 
      id -> Valve(id, rate.toLong, conns.split(", ").toArray)
  .toMap


def part1(input: String): Long =
  val valves = readInput(input)
  val maxTime = 30
  val start = "AA"
  
  val cache = HashMap[(Valve, Int, Set[Valve]), Long]()
  
  def walk(valve: Valve, time: Int, activeValves: Set[Valve]): Long =
    if time >= maxTime then 0L
    else if cache.contains((valve, time, activeValves)) then
      cache((valve, time, activeValves))
    else 
      val newActiveValves = if valve.rate == 0L || activeValves.contains(valve)
        then activeValves
        else activeValves + valve
      val pressureRelease = activeValves.map(_.rate).sum
      val maxPressureRelease = valve.conns
        .map(id => walk(valves(id), time + 1, newActiveValves))
        .max
      val res = pressureRelease + maxPressureRelease
        
      cache((valve, time, activeValves)) = res
      res
  
  walk(valves(start), 1, Set())


def part2(input: String): Long =
  0L


@main def main() =
  val filename = "input.txt"
  given debug: Boolean = true
  val testInput = """
  Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
  Valve BB has flow rate=13; tunnels lead to valves CC, AA
  Valve CC has flow rate=2; tunnels lead to valves DD, BB
  Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE
  Valve EE has flow rate=3; tunnels lead to valves FF, DD
  Valve FF has flow rate=0; tunnels lead to valves EE, GG
  Valve GG has flow rate=0; tunnels lead to valves FF, HH
  Valve HH has flow rate=22; tunnel leads to valve GG
  Valve II has flow rate=0; tunnels lead to valves AA, JJ
  Valve JJ has flow rate=21; tunnel leads to valve II
  """
  val input = if debug
    then testInput
    else io.Source.fromFile(filename).getLines().mkString("\n")

  println(s"2022 Day 16, Part 1: ${part1(input)}")
  println(s"2022 Day 16, Part 2: ${part2(input)}")
