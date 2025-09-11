package aoc2015

import scala.collection.mutable.HashMap

val testInput = """
123 -> x
456 -> y
x AND y -> d
x OR y -> e
x LSHIFT 2 -> f
y RSHIFT 2 -> g
NOT x -> h
NOT y -> i
"""

sealed trait Target
case class Const(v: Long) extends Target:
  override def toString() = s"$v"
case class Wire(name: String) extends Target:
  override def toString() = s"$name"

sealed trait Circuit:
  val sink: String
case class Send(v: Target, sink: String) extends Circuit:
  override def toString() = s"$v -> $sink"
case class And(left: Target, right: Target, sink: String) extends Circuit:
  override def toString() = s"$left AND $right -> $sink"
case class Or(left: Target, right: Target, sink: String) extends Circuit:
  override def toString() = s"$left OR $right -> $sink"
case class LShift(left: Target, right: Target, sink: String) extends Circuit:
  override def toString() = s"$left LSHIFT $right -> $sink"
case class RShift(left: Target, right: Target, sink: String) extends Circuit:
  override def toString() = s"$left RSHIFT $right -> $sink"
case class Not(target: Target, sink: String) extends Circuit:
  override def toString() = s"NOT $target -> $sink"

def parseInput(input: String): Array[Circuit] =
  val send_re = "^([a-z0-9]+) -> ([a-z]+)$".r
  val and_re = "^([a-z0-9]+) AND ([a-z0-9]+) -> ([a-z]+)$".r
  val or_re = "^([a-z0-9]+) OR ([a-z0-9]+) -> ([a-z]+)$".r
  val lshift_re = "^([a-z0-9]+) LSHIFT (\\d+) -> ([a-z]+)$".r
  val rshift_re = "^([a-z0-9]+) RSHIFT (\\d+) -> ([a-z]+)$".r
  val not_re = "^NOT ([a-z0-9]+) -> ([a-z]+)$".r
  def parseTarget(s: String): Target =
    s.toLongOption match
      case None => Wire(s)
      case Some(v) => Const(v)
  input.trim().split("\n").map(_.trim()).filter(_.nonEmpty).map:
    case send_re(target, sink) => Send(parseTarget(target), sink)
    case and_re(left, right, sink) => And(parseTarget(left), parseTarget(right), sink)
    case or_re(left, right, sink) => Or(parseTarget(left), parseTarget(right), sink)
    case lshift_re(left, right, sink) => LShift(parseTarget(left), parseTarget(right), sink)
    case rshift_re(left, right, sink) => RShift(parseTarget(left), parseTarget(right), sink)
    case not_re(target, sink) => Not(parseTarget(target), sink)
  .toArray

def evalTarget(t: Target)(using circuitMap: Map[String, Circuit], cache: HashMap[String, Long]): Long =
  t match
    case Const(v) => v
    case Wire(name) => 
      if cache.contains(name) then cache(name)
      else eval(circuitMap(name))

def eval(c: Circuit)(using circuitMap: Map[String, Circuit], cache: HashMap[String, Long]): Long =
  if cache.contains(c.sink) then
    cache(c.sink)
  else
    val res = c match
      case Send(v, _) => evalTarget(v)
      case And(left, right, _) => evalTarget(left) & evalTarget(right)
      case Or(left, right, _) => evalTarget(left) | evalTarget(right)
      case LShift(left, right, _) => evalTarget(left) << evalTarget(right)
      case RShift(left, right, _) => evalTarget(left) >> evalTarget(right)
      case Not(target, _) => ~evalTarget(target)
    cache(c.sink) = res
    res

def part1(circuits: Array[Circuit]): Long =
  given circuitMap: Map[String, Circuit] = circuits.map(c => c.sink -> c).toMap
  given cache: HashMap[String, Long] = HashMap()
  val a = circuitMap("a")
  eval(a)

def part2(circuits: Array[Circuit], b: Long): Long =
  val circuitMap = HashMap[String, Circuit](circuits.map(c => c.sink -> c):_*)
  circuitMap("b") = Send(Const(b), "b")
  given Map[String, Circuit] = circuitMap.toMap
  given HashMap[String, Long] = HashMap()
  val a = circuitMap("a")
  eval(a)

@main def main =
  val debug = false
  val input = if debug
    then testInput
    else io.Source.fromFile("input.txt").getLines().mkString("\n")
  val circuits = parseInput(input)
  val res1 = part1(circuits)
  println(s"2015 Day 7, Part 1: $res1")
  val res2 = part2(circuits, res1)
  println(s"2015 Day 7, Part 2: $res2")
