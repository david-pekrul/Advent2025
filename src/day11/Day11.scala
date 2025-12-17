package day11

object Day11 {
  def main(args: Array[String]): Unit = {
    //    val input = helpers.Helpers.readTestFile(this)
    val input = helpers.Helpers.readFile(this)

    val data = input.map(line => {
      val split = line.split(":")
      val from = split.head.trim
      val tos = split.last.split(" ").map(_.trim)

      from -> tos.toSet
    }).toMap


    val part1 = solvePart1(data)
    println(s"Part 1: $part1")
  }

  def solvePart1(data: Map[String, Set[String]]): Long = {

    val destBackToSources = data.toSeq.flatMap { case (source, dests) => {
      dests.map(d => (d, source))
    }
    }.groupMap(_._1)(_._2)


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


}
