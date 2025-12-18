package day11

import scala.annotation.tailrec

object Day11 {
  def main(args: Array[String]): Unit = {
    val start = System.nanoTime()
    //    val input = helpers.Helpers.readFile("src/day11/test2.txt")
    val input = helpers.Helpers.readFile(this)

    val data = input.map(line => {
      val split = line.split(":")
      val from = split.head.trim
      val tos = split.last.split(" ").map(_.trim)

      from -> tos.toSet
    }).toMap

    val part1 = solvePart1(data)
    println(s"Part 1: $part1")

    val part2 = solvePart2(data)
    println(s"Part 2: $part2")

    val end = System.nanoTime() - start
    println(s"Time: ${end / 1000.0 / 1000.0}ms")
  }

  def solvePart1(data: Map[String, Set[String]]): Long = {

    val destBackToSources = data.toSeq.flatMap { case (source, dests) => {
      dests.map(d => (d, source))
    }
    }.groupMap(_._1)(_._2)


    @tailrec
    def _run(frontEdge: Map[String, Long], counts: Map[String, Long]): Map[String, Long] = {
      if (frontEdge.isEmpty) {
        return counts
      }

      val updatedCounts = frontEdge.foldLeft(counts)((currentCounts, nextNode) => {
        currentCounts.updatedWith(nextNode._1)(x => Some(x.getOrElse(0L) + nextNode._2))
      })

      val nextFrontEdge = frontEdge.foldLeft(Map[String, Long]())((acc, next) => {
        val upStream = destBackToSources.get(next._1)
        if (upStream.isDefined) {
          upStream.get.foldLeft(acc)((acc2, next2) => {
            val thing = acc2.updatedWith(next2)(x => Some(x.getOrElse(0L) + next._2))
            thing
          })
        } else {
          acc
        }
      })

      _run(nextFrontEdge, updatedCounts)
    }

    val result = _run(Map(("out", 1L)), Map.empty)

    result("you")
  }

  def solvePart2(data: Map[String, Set[String]]): Long = {

    val server = "srv"
    val output = "out"
    val fastFour = "fft"
    val dac = "dac"

    val destBackToSources = data.toSeq.flatMap { case (source, dests) => {
      dests.map(d => (d, source))
    }
    }.groupMap(_._1)(_._2)


    @tailrec
    def _run(frontEdge: Map[String, Long], counts: Map[String, Long], upstreamEnd: String, downstreamEnd: String): Map[String, Long] = {
      if (frontEdge.isEmpty) {
        return counts
      }

      val updatedCounts = frontEdge.foldLeft(counts)((currentCounts, nextNode) => {
        currentCounts.updatedWith(nextNode._1)(x => Some(x.getOrElse(0L) + nextNode._2))
      })

      val nextFrontEdge = frontEdge.filter(n => n._1 != upstreamEnd)
        .foldLeft(Map[String, Long]())((acc, next) => {
          val upStream = destBackToSources.get(next._1)
          if (upStream.isDefined) {
            upStream.get
              .foldLeft(acc)((acc2, next2) => {
                val thing = acc2.updatedWith(next2)(x => Some(x.getOrElse(0L) + next._2))
                thing
              })
          } else {
            acc
          }
        })

      _run(nextFrontEdge, updatedCounts, upstreamEnd, downstreamEnd)
    }

    //upstream to downstream
    val srvFromFFT = _run(Map((fastFour, 1L)), Map.empty, server, fastFour)
    val fftFromDac = _run(Map((dac, 1L)), Map.empty, fastFour, dac)
    val outFromDac = _run(Map((output, 1L)), Map.empty, fastFour, output)

    val srvfft = srvFromFFT.get("svr")
    val fftDac = fftFromDac.get("fft")
    val outDac = outFromDac.get("dac")

    //svr -> dac -> fft -> out
    val answer = srvfft.get * fftDac.get * outDac.get
    answer
  }


}
