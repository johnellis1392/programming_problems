package aoc2023

import scala.collection.mutable
import scala.collection.mutable.ArrayDeque
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashSet


sealed trait Pulse
case object High extends Pulse
case object Low extends Pulse


case class Event(
  val id: Module.Id,
  val sender: Module.Id,
  val pulse: Pulse
)



object Module:
  type Id = String

sealed trait Module:
  val id: Module.Id
  val targets: Array[Module.Id]
  val sources: ArrayBuffer[Module.Id]
  def receive(event: Event): Option[Pulse]

object Button:
  val Id: Module.Id = "button"


object Broadcaster:
  val Id: Module.Id = "broadcaster"

case class Broadcaster(
  val targets: Array[Module.Id],
  val sources: ArrayBuffer[Module.Id] = ArrayBuffer()
) extends Module:
  override val id: Module.Id = Broadcaster.Id
  override def toString(): String = 
    s"broadcaster -> ${targets.mkString(", ")}"

  override def receive(event: Event): Option[Pulse] =
    Some(event.pulse)



object FlipFlop:
  sealed trait State
  case object On extends State
  case object Off extends State

case class FlipFlop(
  val id: Module.Id,
  val targets: Array[Module.Id],
  val sources: ArrayBuffer[Module.Id] = ArrayBuffer()
) extends Module:
  import FlipFlop._

  private var state: State = Off

  override def toString(): String =
    s"%$id -> ${targets.mkString(", ")} // sources=${sources.mkString(", ")}"

  override def receive(event: Event): Option[Pulse] =
    val Event(_, _, pulse) = event
    (state, pulse) match
      case (_, High) =>
        None // Ignore
      case (Off, Low) => 
        state = On
        Some(High)
      case (On, Low) =>
        state = Off
        Some(Low)



case class Conjunction(
  val id: Module.Id,
  val targets: Array[Module.Id],
  val sources: ArrayBuffer[Module.Id] = ArrayBuffer(),
  val mem: mutable.HashMap[Module.Id, Pulse] = mutable.HashMap()
) extends Module:

  override def toString(): String =
    s"&$id -> ${targets.mkString(", ")} // sources=${sources.mkString(", ")}"

  override def receive(event: Event): Option[Pulse] =
    val Event(_, sender, pulse) = event
    mem(sender) = pulse
    val allHighs = mem.values.foldLeft(true) { (a, v) => a && v == High }
    if allHighs
      then Some(Low)
      else Some(High)



case class Sink(
  val id: Module.Id,
  val targets: Array[Module.Id] = Array(),
  val sources: ArrayBuffer[Module.Id] = ArrayBuffer()
) extends Module:
  override def receive(event: Event): Option[Pulse] = None
  override def toString(): String =
    s"sink $id // sources=${sources.mkString(", ")}"



def readInput(input: String): Map[Module.Id, Module] =
  val broadcaster_re = "^broadcaster -> (.*)$".r
  val flipflop_re = "^%(\\w+) -> (.*)$".r
  val conjunction_re = "^&(\\w+) -> (.*)$".r

  val modules = ArrayBuffer[Module]()
  val ids = HashSet[Module.Id]()

  input.trim().split("\n").map(_.trim()).filter(_.nonEmpty).foreach {
    case broadcaster_re(targets) =>
      val targetIds = targets.split(", ")
      ids.addAll(targetIds)
      modules += Broadcaster(targetIds)
    case flipflop_re(id, targets) =>
      val targetIds = targets.split(", ")
      ids.addAll(targetIds)
      modules += FlipFlop(id, targetIds)
    case conjunction_re(id, targets) =>
      val targetIds = targets.split(", ")
      ids.addAll(targetIds)
      modules += Conjunction(id, targetIds)
  }

  modules.foreach(m => ids.remove(m.id))
  ids.foreach(id => modules += Sink(id))
  val moduleMap = modules.map(m => m.id -> m).toMap
  
  // Assign sources
  for module <- moduleMap.values
      targetId <- module.targets
    do moduleMap(targetId) match
      case Conjunction(_, _, sources, mem) => 
        mem += (module.id -> Low)
        sources += module.id
      case m => 
        m.sources += module.id

  moduleMap



def dumpModuleTree(modules: Map[Module.Id, Module]): Unit =
  val visited = HashSet[Module.Id]()
  val queue = ArrayDeque[Module.Id]()
  queue += Broadcaster.Id
  while queue.nonEmpty do
    val curr = queue.removeHead()
    if !visited.contains(curr) then
      val m = modules(curr)
      println(m)
      queue.addAll(m.targets)
      visited += curr



def part1(input: String): Long =
  val modules = readInput(input)
  val queue = ArrayDeque[Event]()
  val warmupSteps = 1000

  var lows = 0L
  var highs = 0L

  for _ <- 0 until warmupSteps do
    queue += Event(Broadcaster.Id, Button.Id, Low)
    lows += 1

    while queue.nonEmpty do
      val event@Event(id, sender, pulse) = queue.removeHead()
      if modules.contains(id) then
        val m = modules(id)
        m.receive(event) match
          case Some(High) =>
            highs += m.targets.size.toLong
            for targetId <- m.targets do
              queue += Event(targetId, id, High)
          case Some(Low) => 
            lows += m.targets.size.toLong
            for targetId <- m.targets do
              queue += Event(targetId, id, Low)
          case None => // Skip
  
  lows * highs


def gcd(a: Long, b: Long): Long = if (b == 0L) a.abs else gcd(b, a % b)
def lcm(a: Long, b: Long): Long = (a * b).abs / gcd(a, b)


def part2(input: String): Long =
  val modules = readInput(input)
  val queue = ArrayDeque[Event]()
  val start = modules("rx")

  // Find dependencies to look for
  // In my input, rx <- &jq <- (&vr, &nl, &lr, &gt)
  // So get the sources for &jq to calculate the cycle,
  // cache the cycle number where the conjunction's dependencies
  // evaluate to High, then get the lcm to calculate the
  // correct cycle number.
  val cache = mutable.HashMap[Module.Id, Long]()
  for m <- modules(start.sources.head).sources do modules(m) match
    case Conjunction(id, targets, sources, mem) => cache(id) = 0
    case _ =>

  var cacheDirty = false
  def tryCache(m: Module, pulse: Pulse, cycle: Long): Unit =
    (m, pulse) match
      case (Conjunction(id, _, _, _), High) =>
        if cache.contains(id) && cache(id) == 0 then
          cache(id) = cycle
          cacheDirty = true
      case _ => // Skip

  def cacheFull = cache.values.map(_ != 0).reduce(_ && _)

  def dumpCache: Unit = println(cache.map((id, cycle) => s"$id=$cycle").mkString(", "))

  var running = true
  var cycle = 0L
  while running do
    cycle += 1L
    queue += Event(Broadcaster.Id, Button.Id, Low)
    while queue.nonEmpty do
      val event@Event(id, sender, pulse) = queue.removeHead()
      val m = modules(id)
      m.receive(event) match
        case Some(result) =>
          tryCache(m, result, cycle)
          for targetId <- m.targets do
            queue += Event(targetId, id, result)
        case None => // Skip

    if cacheDirty then
      dumpCache
      cacheDirty = false
    if cacheFull then
      running = false

  cache.values.reduce(lcm)



@main def main() =
  val filename = "input.txt"
  val debug = false
  val testInput = """
  |broadcaster -> a, b, c
  |%a -> b
  |%b -> c
  |%c -> inv
  |&inv -> a
  """.stripMargin

  val testInput2 = """
  |broadcaster -> a
  |%a -> inv, con
  |&inv -> b
  |%b -> con
  |&con -> output
  """.stripMargin
  val input = 
    if debug then testInput2
    else io.Source.fromFile(filename).getLines().mkString("\n")
  
  println(s"2023 Day 20, Part 1: ${part1(input)}")
  println(s"2023 Day 20, Part 2: ${part2(input)}")
