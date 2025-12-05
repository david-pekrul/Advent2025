package day4

import helpers.{Coord, Vector}

import scala.annotation.tailrec

object Day4 {
  def main(args: Array[String]): Unit = {
    val start = System.nanoTime()
//    val input = helpers.Helpers.readFile("src/day4/test.txt")
    val input = helpers.Helpers.readFile("src/day4/day4.txt")

    val coordToChar = parse(input)

    val part1 = findPart1(coordToChar)
    println(s"Part 1: $part1")
    val part2 = findPart2(coordToChar)
    println(s"Part 2: $part2")

    val end = System.nanoTime() - start
    println(s"Time: ${end / 1000.0 / 1000.0}ms")
  }

  def parse(lines: Seq[String]) : Set[Coord] = {
    val coordToChar = lines.zipWithIndex.flatMap { case (line, y) => {
      line.toCharArray.zipWithIndex.map { case (char, x) => {
        Coord(x, y) -> char
      }
      }
    }
    }.toMap.filter(_._2 == '@').keys.toSet

    coordToChar
  }

  def findPart1(coords: Set[Coord]): Long = {
    coords.toSeq.count{  coord => {
      coord.eightNeighbors().count(n => coords.contains(n)) < 4
    }}
  }


  def findPart2(coords: Set[Coord]): Long = {

    val coordToNeighbors = coords.map(coord => {
      coord -> coord.eightNeighbors().intersect(coords)
    }).toMap

    @tailrec
    def _removeAndCount(remaining: Map[Coord,Set[Coord]], possibleNexts: Set[Coord] = Set(), count: Long = 0): Long = {



      val entryToRemoveOpt = possibleNexts
        .find(possibleNext => remaining(possibleNext).size < 4).map(p => p -> remaining(p))
        .orElse(
          remaining.toSet.find{ case (coord, neighbors) => {
            neighbors.size < 4
         }}
        )

      if(entryToRemoveOpt.isEmpty) {
        return count
      }

      val (coord, neighbors) = entryToRemoveOpt.get

      val coordRemovedFromNeighbors = neighbors.foldLeft(remaining)((acc,next) => {
        acc.updatedWith(next)(neighborsNeighborsOpt => Some(neighborsNeighborsOpt.get.filter(_ != coord)))
      })

      val nextPossibles = possibleNexts.filter(_ != coord) ++ coordRemovedFromNeighbors(coord)

      val nextRemaining = coordRemovedFromNeighbors.filter(_._1 != coord)

      //recursive call
      _removeAndCount(nextRemaining, nextPossibles, count+1)
    }

    _removeAndCount(coordToNeighbors)
  }
}
