package day3

import scala.annotation.tailrec

object Day3 {
  def main(args: Array[String]): Unit = {
    val start = System.nanoTime()
//    val inputLines = helpers.Helpers.readFile("src/day3/test.txt")
    val inputLines = helpers.Helpers.readFile("src/day3/day3.txt")

    val batteries = inputLines.map(_.toCharArray.map(_.toString.toInt).toSeq)

    val part1 = batteries.map(x => maxJoltage(x)).sum
    val part2 = batteries.map(x => maxJoltage(x,12)).sum

    println(s"Part 1: $part1")
    println(s"Part 2: $part2")

    val end = System.nanoTime() - start
    println(s"Time: ${end/1000.0/1000.0}ms")
  }

  def maxJoltage(remainingBattery: Seq[Int], maxSize: Int = 2): Long = {

    val start = remainingBattery.take(maxSize)
    val _remain = remainingBattery.drop(maxSize)

    def tryAddDigit(current: Seq[Int], next: Int): Seq[Int] = {
      val pairs = (current :+ next).sliding(2,1)
      val replaced = pairs.foldLeft((Seq[Int](),false))((acc,next) => {
        val alreadyScrunched = acc._2
        val current = acc._1
        if alreadyScrunched then {
          (current :+ next.last, alreadyScrunched)
        } else if (next.head < next.last) {
          (current :+ next.last, true)
        } else {
          (current :+ next.head, false)
        }
      })._1

      replaced
    }

    val result = _remain.foldLeft(start)((acc,next) => {
      tryAddDigit(acc,next)
    })

    toNum(result)
  }

  def toNum(x: Seq[Int]): Long = {
    x.foldLeft(0L)((acc, next) => {
      acc * 10 + next
    })
  }
}
