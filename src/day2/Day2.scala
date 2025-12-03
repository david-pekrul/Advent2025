package day2

import java.util.logging.Logger
import scala.annotation.tailrec

object Day2 {


  def main(args: Array[String]): Unit = {

    val start = System.nanoTime()
//        val rangeInputs = helpers.Helpers.readFile("src/day2/test.txt").head.split(",").toSeq
    val rangeInputs = helpers.Helpers.readFile("src/day2/day2.txt").head.split(",").toSeq

    val ranges = rangeInputs.map(parseRange)
    val narrowRanges = ranges.map(Day2Range.narrow).filter(x => x.low < x.high)

    val part1 = narrowRanges.map(_.part1()).sum
    println(s"Part 1: $part1")


    val part2 = sumDupesOfAnyLength(ranges)
    println(s"Part 2: $part2")

    val end = System.nanoTime() - start;
    println(s"Time: ${end / 1000.0 / 1000.0}ms")
  }

  def sumDupesOfAnyLength(ranges: Seq[Day2Range]): Long = {
    val maxRange = ranges.map(_.high).max
    val dupes = genDupes(maxRange)

    val dupesInRanges = dupes.filter(dupe => {
      ranges.exists(r => {
        if r.contains(dupe) then {
//          println(s"\t$r contains $dupe")
          true
        } else {
          false
        }

      })
    })

    dupesInRanges.sum
  }

  def genDupes(max: Long): Seq[Long] = {
    val maxLength = max.toString.length
    val topHalf = max.toString.take(maxLength - maxLength / 2).toInt

    val startingValues = Range(0, topHalf + 1)
      .filter(x => (x.toString + x.toString).toLong <= max)

    val dupes = startingValues.flatMap(v => dupeIt(v,max))
    dupes.toSet.toSeq
  }

  def dupeIt(input: Int, max: Long): Seq[Long] = {

    if(input == 0) {
      return Seq.empty
    }
    val shiftFactor = Math.pow(10, Math.floor(Math.log10(input))+1).toLong

    @tailrec
    def _dupeIt(collect: Seq[Long]): Seq[Long] = {
      val last = collect.last
      if last > max then {
        collect
      }
      else {
        val next = last * shiftFactor + input
        _dupeIt(collect :+ next)
      }
    }

    val start = input.toString + input.toString
    _dupeIt(Seq(start.toLong))
  }

  def shiftOver(input: Long): Long = {
    input * (Math.pow(10, Math.floor(Math.log10(input)))).toLong
  }

  def parseRange(line: String): Day2Range = {
    val regex = """(\d+)-(\d+)""".r
    val regex(low, high) = line
    Day2Range(low.toLong, high.toLong)
  }

  case class Day2Range(low: Long, high: Long) {

    private lazy val lowLength = Math.floor(Math.log10(low)).intValue
    private lazy val highLength = Math.floor(Math.log10(high)).intValue

    def part1(): Long = {
      val startBottomHalf = low.toString.takeRight(lowLength / 2 + 1).toInt
      val startTopHalf = low.toString.take(lowLength - lowLength / 2).toInt
      val endBottomHalf = high.toString.takeRight(highLength / 2 + 1).toInt
      val endTopHalf = high.toString.take(highLength - highLength / 2).toInt

      val possibles = Range(startTopHalf, endTopHalf + 1).toSeq
        .map(_.toString)
        .map(x => x + x)
        .map(_.toLong)
        .filter(_ <= high)
        .filter(_ >= low)

      possibles.sum
    }

    def contains(dupe: Long): Boolean = {
      low <= dupe && dupe <= high
    }
  }

  object Day2Range {

    def narrow(input: Day2Range): Day2Range = {
      Day2Range(forceEvenDigitLength(input.low, true), forceEvenDigitLength(input.high, false))
    }

    def forceEvenDigitLength(input: Long, isLowEnd: Boolean): Long = {
      val inputLength = input.toString.length
      if (inputLength % 2 == 0) {
        return input
      }

      if (isLowEnd) {
        val exponent = inputLength
        Math.pow(10, exponent).toLong
      } else {
        val exponent = inputLength - 1
        Math.pow(10, exponent).toLong - 1
      }
    }
  }
}


