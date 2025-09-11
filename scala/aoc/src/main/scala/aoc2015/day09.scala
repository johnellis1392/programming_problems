package aoc2015

import scala.collection.mutable.PriorityQueue
import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet
import scala.util.boundary

def part1(input: Array[((String, String), Long)]): Long =
  val neighbors = HashMap[String, HashSet[(String, Long)]]()
  input.foreach:
    case ((a, b), time) =>
      neighbors(a) = HashSet()
      neighbors(b) = HashSet()
  for ((source, dest), dist) <- input do
    neighbors(source) += ((dest, dist))
    neighbors(dest) += ((source, dist))
  
  val queue = PriorityQueue.empty[(String, Long, Set[String])](
    Ordering.by((_: (String, Long, Set[String]))._2).reverse
  )

  val source = "Faerun"
  val dest = "Straylight"
  queue += ((source, 0L, Set.empty))
  var res = Long.MaxValue
  while queue.nonEmpty do
    val (node, d, visited) = queue.dequeue()
    if node == dest then
      // println(s"d=$d, visited=$visited")
      res = math.min(res, d)
    else
      for (neighbor, dd) <- neighbors(node) if !visited.contains(neighbor) do
        queue += ((neighbor, d + dd, visited + node))
  res

@main def main =
  val line_re = "^([a-zA-Z]+) to ([a-zA-Z]+) = (\\d+)$".r
  val debug = true
  val input = if debug
    then """
    London to Dublin = 464
    London to Belfast = 518
    Dublin to Belfast = 141
    """
    else io.Source.fromFile("input.txt").getLines().mkString("\n")

  val values = input.split("\n").map(_.trim()).filter(_.nonEmpty).map:
    case line_re(source, dest, dist) =>
      ((source, dest), dist.toLong)
  val res1 = part1(values)
  // 217 too high
  // 64 is wrong
  println(s"2015 Day 9, Part 1: $res1")
