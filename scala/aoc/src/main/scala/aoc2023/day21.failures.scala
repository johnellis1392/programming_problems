package aoc2023

/**
 * This file contains all my failed attempts for this problem
 * before I found out about polynomial fitting (whatever that is...)
 */

import scala.collection.mutable.HashSet
import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayDeque


// Rote solution; bad time-complexity
def part2_1(input: String): Long =
  val (grid, start) = readInput(input)
  val queue = ArrayDeque[aoc2015.Point]()
  val points = HashSet[aoc2015.Point]()
  val steps = 26501365
  points += start
  for _ <- 0 until steps do
    queue.addAll(points)
    points.clear()
    while queue.nonEmpty do
      val curr = queue.removeHead()
      for p <- curr.neighbors do
        val p2 = grid.reframe(p)
        if grid(p2) != R then
          points += p
  points.size


// Using sets; this also works, but is still
// too slow, and runs the risk of a heap overflow
// because of the storage space involved.
def part2_2(input: String): Long =
  val (grid, start) = readInput(input)
  val steps = 26501365

  var set1 = HashSet[aoc2015.Point]()
  var frontier1 = HashSet[aoc2015.Point]()
  var set2 = HashSet[aoc2015.Point]()
  var frontier2 = HashSet[aoc2015.Point]()

  frontier1 += start
  for _ <- 0 until steps do
    for p <- frontier1 
        point <- p.neighbors 
        if grid(grid.reframe(point)) != R && !set2.contains(point)
      do frontier2 += point
      
    set1.addAll(frontier1)
    frontier1.clear()

    var s = set1
    set1 = set2
    set2 = s

    var f = frontier1
    frontier1 = frontier2
    frontier2 = f

  set1.union(frontier1).size


// Another working solution; cuts down number of
// stored points and improves time, but still not
// good enough to work.
def part2_3(input: String, steps: Int = 6): Long =
  val (grid, start) = readInput(input)

  var prev1 = HashSet[aoc2015.Point]()
  var curr1 = HashSet[aoc2015.Point]()
  var total1 = 1L
  var prev2 = HashSet[aoc2015.Point]()
  var curr2 = HashSet[aoc2015.Point]()
  var total2 = 0L

  curr1 += start
  for _ <- 0 until steps do
    // total1 += curr1.size.toLong
    for p <- curr1
        n <- p.neighbors
        if grid(grid.reframe(n)) != R && !prev2.contains(n)
      do curr2 += n

    total2 += curr2.size.toLong

    prev1.clear()
    prev1.addAll(curr1)
    curr1.clear()

    val c = curr1
    curr1 = curr2
    curr2 = c

    val p = prev1
    prev1 = prev2
    prev2 = p

    val t = total1
    total1 = total2
    total2 = t

  total1


// Also works; also bad time
def part2(input: String, steps: Int): Long =
  val (grid, start) = readInput(input)

  var prev = HashSet[aoc2015.Point]()
  var curr = HashSet[aoc2015.Point]()
  var next = HashSet[aoc2015.Point]()

  var evens = 1L
  var odds = 0L

  curr += start
  for step <- 1 to steps do
    for p <- curr
        n <- p.neighbors
        if !prev.contains(n) && grid(grid.reframe(n)) != R
      do next += n
    
    if step % 2 == 0
      then evens += next.size.toLong
      else odds += next.size.toLong
    
    val temp = prev
    prev = curr
    curr = next
    next = temp
    next.clear()
    prev.addAll(curr)

  if steps % 2 == 0
    then evens
    else odds
