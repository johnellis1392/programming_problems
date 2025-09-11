package aoc2023

import scala.concurrent.{Future, Await, ExecutionContext}
import scala.collection.immutable.NumericRange
import scala.math.min
import scala.concurrent.duration.Duration
import scala.collection.mutable.ArrayBuffer

val testInput = """
seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4
"""

case class RangeMap(source: NumericRange[Long], dest: NumericRange[Long]):
  override def toString(): String =
    s"source=$source, dest=$dest"
  def contains(i: Long) = source.contains(i)
  def transform(i: Long) =
    if contains(i)
    then i - source.start + dest.start
    else i

case class ResourceMap(source: String, dest: String, ranges: Array[RangeMap]):
  override def toString(): String =
    s"$source-to-$dest:\n${ranges.map(_.toString()).mkString("\n")}"

def readInput(input: String): (Array[Long], Array[ResourceMap]) =
  val configs = input.trim().split("\n\n").map(_.trim()).filter(_.nonEmpty)
  val seeds = configs.head.stripPrefix("seeds: ").split("\\s+").map(_.toLong)
  val resources = configs.tail.map:
    config =>
      val lines = config.split("\n").map(_.trim()).filter(_.nonEmpty)
      val Array(sourceId, _, destId) = lines.head.stripSuffix(" map:").split("-")
      val rangeMaps = lines.tail.map:
        line =>
          val Array(dest, source, len) = line.split("\\s+").map(_.toLong)
          RangeMap((source until (source + len)), (dest until (dest + len)))
      ResourceMap(sourceId, destId, rangeMaps)
  (seeds, resources)


def part1(input: String): Long =
  val (startSeeds, resources) = readInput(input)
  resources.foldLeft(startSeeds):
    case (seeds, ResourceMap(_, _, ranges)) =>
      seeds.map:
        seed =>
          ranges.find(range => range.contains(seed)) match
            case Some(range) => range.transform(seed)
            case None => seed
  .min


// Concurrent solution; runs real slow
def part2_concurrent(input: String): Long =
  val (startSeeds, resources) = readInput(input)
  val seedRanges = startSeeds.toList.grouped(2).toList.map(vs => vs(0) until vs(0) + vs(1)).toList

  given ExecutionContext = ExecutionContext.global

  def divideRange(startRange: NumericRange[Long], chunkSize: Int): List[NumericRange[Long]] =
    var range = startRange
    val res = ArrayBuffer[NumericRange[Long]]()
    while range.nonEmpty do
      val (left, right) = range.splitAt(chunkSize)
      res += left
      range = right
    res.toList

  def process(seedRange: NumericRange[Long]): Future[Long] =
    Future:
      seedRange.map:
        seed =>
          resources.foldLeft(seed):
            case (currSeed, ResourceMap(_, _, ranges)) =>
              ranges.find(range => range.contains(currSeed)) match
                case Some(range) => range.transform(currSeed)
                case None => currSeed
      .min

  var res = Long.MaxValue
  val chunkSize = 100_000
  for seedRange <- seedRanges do
    val f = Future.sequence:
      divideRange(seedRange, chunkSize).map:
        range =>
          process(range)
    val temp = Await.result(f, Duration.Inf)
    res = min(res, temp.min)
    
  res


extension(range: NumericRange[Long])
  def contains(o: NumericRange[Long]) = range.start <= o.start && o.end <= range.end
  def overlaps(o: NumericRange[Long]) = range.contains(o.start) || range.contains(o.end)
  // def cut(o: NumericRange[Long]) =
  //   if range == o then range
  //   else if range.start == o.start
  //     if range.end < o.end then
  //       ???
    


def part2(input: String): Long =
  val (startSeeds, resources) = readInput(input)

  0L


@main def main() =
  val debug = false
  val input = if debug
    then testInput
    else io.Source.fromFile("input.txt").getLines().mkString("\n")
  println(s"2023 Day 5, Part 1: ${part1(input)}")
  println(s"2023 Day 5, Part 2: ${part2(input)}")
