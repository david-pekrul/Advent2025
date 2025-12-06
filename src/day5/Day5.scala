package day5

import scala.annotation.tailrec

object Day5 {
  def main(args: Array[String]): Unit = {
    val day = this.getClass.getPackageName
    val input = helpers.Helpers.readFile(this)
//    val input = helpers.Helpers.readTestFile(this)

    val ranges = input.takeWhile(_.nonEmpty).map(_.split("-")).map(s => s.map(_.toLong).toSeq)
    val ids = input.dropWhile(_.nonEmpty).filter(_.nonEmpty).map(_.toLong)

    val combinedRanges = combineRanges(ranges).sortBy(_.head).distinct
    val overlap = combinedRanges.sliding(2,1).find{case Seq(Seq(a,b),Seq(c,d)) => b >= c}
    if(overlap.isDefined) {
      throw Exception(s"OVERLAP DETECTED! ${overlap.get}")
    }
    val part1 = filterIds(combinedRanges, ids)
    println(s"Part 1: $part1")

    val part2 = countFresh(combinedRanges)
    println(s"Part 2: $part2")
  }


  def filterIds(ranges: Seq[Seq[Long]], ids: Seq[Long]): Long = {

    val goodIds = ids.filter(id => {
      ranges.exists(range => rangeContainsId(range,id))
    })
    goodIds.size
  }

  def countFresh(ranges: Seq[Seq[Long]]): Long = {
    ranges.map(range => (range.last - range.head) + 1).sum
  }

  def rangeContainsId(range: Seq[Long], id: Long): Boolean = {
    range.head <= id && id <= range.last
  }


  @tailrec
  def combineRanges(currentRanges: Seq[Seq[Long]]): Seq[Seq[Long]] = {

    val currentOverlapOpt = currentRanges.to(LazyList).map { case Seq(a,b) => {
      Seq(a,b) -> currentRanges.find{case Seq(c,d) => !(a == c && b == d) &&
      {
        (a <= c && b >= c) // a------c----b----d
        ||
        (a <= c && b >= d) // a----c---d-----b
      }
      }
    }}.find(_._2.isDefined).map(a => a._1 -> a._2.get)

    if (currentOverlapOpt.isEmpty) {
      return currentRanges //no more overlaps
    }
    val (x,y) = currentOverlapOpt.get

    val replacement = currentOverlapOpt.map{case (Seq(a,b),Seq(c,d)) => Seq(Math.min(a,c),Math.max(b,d))}.get
    val nextRanges = currentRanges.filter(a => a != x && a != y) :+ replacement

    combineRanges(nextRanges)
  }
}
