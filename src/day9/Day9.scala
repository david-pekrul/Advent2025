package day9

import helpers.{Coord, Helpers, Vector}

object Day9 {

  val breakPoint = Set(Coord(11, 1), Coord(2, 5))

  def main(args: Array[String]): Unit = {
    val start = System.nanoTime()
    //    val input = Helpers.readTestFile(this)
    val input = Helpers.readFile(this)

    val points = input.map(line => {
      line.split(",").map(_.toInt).toSeq
    })

    val part1 = solvePart1(points)
    println(s"Part 1: $part1")

    val part2 = solvePart2(points)
    println(s"Part 2: $part2")

    val end = System.nanoTime() - start
    println(s"Time: ${end / 1000.0 / 1000.0}ms")
  }

  def solvePart1(points: Seq[Seq[Int]]): Long = {
    points.map(_.map(_.toLong)).zipWithIndex.map { case (point, idx) => {
      val maxAreaForPoint = points.drop(idx + 1).foldLeft(0L)((acc, other) => {
        val area = (Math.abs(point.head - other.head) + 1) * (Math.abs(point.last - other.last) + 1)
        if (area > acc) {
          area
        } else {
          acc
        }
      })
      maxAreaForPoint
    }
    }.max
  }

  def solvePart2(points: Seq[Seq[Int]]): Long = {

    val coords = points.map { case Seq(x, y) => Coord(x, y) }

    val lineSegments = coords.zipWithIndex
      .map { case (coordA, idxA) => {
        val coordB = coords((idxA + 1) % coords.size)
        coordA.vectorTo(coordB) match {
          case Vector(x, 0) => {
            Seq(coordA, coordB).sortBy(_.x).sliding(2, 1).toSeq.head -> Vector.RIGHT //horizontal
          }
          case Vector(0, y) => {
            Seq(coordA, coordB).sortBy(_.y).sliding(2, 1).toSeq.head -> Vector.DOWN //vertical
          }
        }
      }
      }.groupMap(_._2)(_._1)

    val (overallMax, overallMaxCoords) = coords.zipWithIndex.foldLeft((0L, Set[Coord]())) { case ((currentMaxSize, currentMaxPair), (coordA, aIndex)) => {

//      println(s"aIndex = $aIndex \t currentMaxSize: $currentMaxSize \t $currentMaxPair")

      val (maxForA, maxPairforA) = coords.drop(aIndex + 1).foldLeft((currentMaxSize, currentMaxPair)) { case ((currentMaxForA, currentSetForA), coordB) => {
        val (minX, maxX) = if (coordA.x < coordB.x) {
          (coordA.x, coordB.x)
        } else {
          (coordB.x, coordA.x)
        }
        val (minY, maxY) = if (coordA.y < coordB.y) {
          (coordA.y, coordB.y)
        } else {
          (coordB.y, coordA.y)
        }

        val thisRectangleSize = (1 + maxX - minX).toLong * (1 + maxY - minY).toLong

//        println(s"starting ${(coordA, coordB)} with size $thisRectangleSize")

        //logic checks
        if (thisRectangleSize <= currentMaxForA) {
//          println(s"too small ${(coordA, coordB)}")
          (currentMaxForA, currentSetForA)
        } else {
          val doesRectangleIntersectEdges = intersectsAnyEdge(coordA, coordB, lineSegments)


          if (doesRectangleIntersectEdges) {
            //no op =>
//            println(s"intersection found ${(coordA, coordB)}")
            (currentMaxForA, currentSetForA)
          } else {
            if (thisRectangleSize > currentMaxForA) {
//              println(s"NEW MAX FOUND ${(coordA, coordB)}")
              (thisRectangleSize, Set(coordA, coordB))
            } else {
              //not quite, so it's a no-op
//              println(s"too small after all checks ${(coordA, coordB)}")
              (currentMaxForA, currentSetForA)
            }
          }
        }
      }
      }

      (maxForA, maxPairforA)
    }
    }


//    println(overallMax)
//    println(overallMaxCoords)

    overallMax
  }

  def distance(a: Seq[Int], b: Seq[Int]): Double = {
    val x = a.zip(b).map { case (c, d) => d - c }.map(Math.pow(_, 2)).sum
    Math.sqrt(x)
  }

  def distance(a: Coord, b: Coord): Double = {
    distance(Seq(a.x, a.y), Seq(b.x, b.y))
  }

  def intersectsAnyEdge(coordA: Coord, coordB: Coord, lineSegments: Map[Vector, Seq[Seq[Coord]]], topLevel: Boolean = true): Boolean = {

    val (minX, maxX) = if (coordA.x < coordB.x) {
      (coordA.x, coordB.x)
    } else {
      (coordB.x, coordA.x)
    }
    val (minY, maxY) = if (coordA.y < coordB.y) {
      (coordA.y, coordB.y)
    } else {
      (coordB.y, coordA.y)
    }

    val topLeft = Coord(minX, minY)
    val bottomLeft = Coord(minX, maxY)
    val topRight = Coord(maxX, minY)
    val bottomRight = Coord(maxX, maxY)

    val intersectsVertical = lineSegments(Vector.DOWN).exists { case Seq(a, b) => {
      //a and b are sorted so A comes before B in the vector direction
      /*
          x →
       y             a
       ↓             |
            *Left ------- *Right
                     |
                     b
       */

      val topEdgeIntersects = (topLeft.x < a.x && a.x < topRight.x) && (a.y < topLeft.y && topLeft.y < b.y)

      val bottomEdgeIntersects = (minX < a.x && a.x < maxX) && (a.y < bottomLeft.y && bottomLeft.y < b.y)

      if (topEdgeIntersects) {
//        println(s"topEdgeIntersects ${(coordA, coordB)} \t at ${(a, b)}")
      }
      if (bottomEdgeIntersects) {
//        println(s"bottomEdgeIntersects ${(coordA, coordB)} \t at ${(a, b)}")
      }

      topEdgeIntersects || bottomEdgeIntersects
    }
    }

    val intersectsHorizontal = lineSegments(Vector.RIGHT).exists { case Seq(a, b) => {
      //a and b are sorted so A comes before B in the vector direction
      /*  x →
       y
       ↓      top*
                |
         a ---------- b
                |
             bottom*
       */
      val leftEdgeIntersects = (topLeft.y < a.y && a.y < bottomLeft.y) && (a.x < topLeft.x && topLeft.x < b.x)

      val rightEdgeIntersects = (topRight.y < a.y && a.y < bottomRight.y) && (a.x < topRight.x && topRight.x < b.x)

      if (leftEdgeIntersects) {
//        println(s"leftEdgeIntersects ${(coordA, coordB)} \t at ${(a, b)}")
      }
      if (rightEdgeIntersects) {
//        println(s"rightEdgeIntersects ${(coordA, coordB)} \t at ${(a, b)}")
      }

      leftEdgeIntersects || rightEdgeIntersects

    }
    }

    val innerPointIntersectsEdges: Boolean = {
      if (!topLevel) {
        false //this is already on an inner point, don't go further down
      } else {
        val insidePoint = coordA.eightNeighbors().minBy(n => distance(n, coordB))
        if (insidePoint.x == minX || insidePoint.x == maxX || insidePoint.y == minY || insidePoint.y == maxY) {
          false //it's on an existing edge
        } else {
          val insideResult = intersectsAnyEdge(insidePoint, coordB, lineSegments, topLevel = false)
          insideResult
        }
      }
    }


    return intersectsHorizontal || intersectsVertical || innerPointIntersectsEdges
  }
}
