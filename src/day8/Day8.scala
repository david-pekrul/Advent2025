package day8

object Day8 {
  def main(args: Array[String]): Unit = {
//    val (input, make, take) = (helpers.Helpers.readTestFile(this), 10, 3)
    val (input, make, take) = (helpers.Helpers.readFile(this), 1000, 3)

    val points = input.map(line => {
      line.split(",").map(_.toInt).toSeq
    }).sortBy(a => a.head)

    val kdTree = buildKDTree(points)

    val part1 = solvePart1(points, make, take)
    println(s"Part 1: $part1")

    val part2 = solvePart2(points)
    println(s"Part 2: $part2")

  }

  def solvePart1(points: Seq[Seq[Int]], connectionsToMake: Int, circuitsToTake: Int): Long = {

    points.sortWith{ case (a,b) =>
      if(a.head != b.head) {
        a.head < b.head
      } else if (a(1) != b(1)) {
        a(1) < b(1)
      } else {
        a.last < b.last
      }
    }

    val distancesToPairs = points.zipWithIndex.flatMap { case (point, idx) =>
      points.drop(idx+1).map(other => distance(point, other) -> (point, other))
    }.sortBy(_._1)


    val initialMap = points.zipWithIndex.toMap

    val stateAfterConnections = Range(0, connectionsToMake)
      .foldLeft(initialMap)((currentState, nextConnectionIndex) => {
        val (dist, (pointA, pointB)) = distancesToPairs(nextConnectionIndex)

        val groupAid = currentState(pointA)
        val groupBid = currentState(pointB)

        val groupA = currentState.filter(_._2 == groupAid).keySet
        val groupB = currentState.filter(_._2 == groupBid).keySet

        //move group B to group A
        val updatedState = groupB.foldLeft(currentState)((acc, next) => {
          acc.updated(next, groupAid)
        })

        updatedState
      })

    val circuits = stateAfterConnections.toSeq
      .groupMap(_._2)(_._1)
      .values
      .map(c => c.size -> c)
      .toSeq
      .sortBy(_._1).reverse


    val selection = circuits.take(circuitsToTake).map(_._1.toLong)

    selection.product
  }

  def solvePart2(points: Seq[Seq[Int]]): Long = {

    points.sortWith { case (a, b) =>
      if (a.head != b.head) {
        a.head < b.head
      } else if (a(1) != b(1)) {
        a(1) < b(1)
      } else {
        a.last < b.last
      }
    }

    val distancesToPairs = points.zipWithIndex.flatMap { case (point, idx) =>
      points.drop(idx + 1).map(other => distance(point, other) -> (point, other))
    }.sortBy(_._1)


    val initialMap = points.zipWithIndex.toMap
    val circuitSizes = initialMap.values.map(groupId => groupId -> 1).toMap

    val stateAfterConnections = distancesToPairs.indices
      .foldLeft((initialMap,circuitSizes,Seq[Seq[Int]]())){case ((currentState,currentCircuitSizes,latestNodes), nextConnectionIndex) => {
        if(currentCircuitSizes.size == 1) {
          (currentState,currentCircuitSizes,latestNodes)
        } else {
          val (dist, (pointA, pointB)) = distancesToPairs(nextConnectionIndex)

          val groupAid = currentState(pointA)
          val groupBid = currentState(pointB)

          val groupA = currentState.filter(_._2 == groupAid).keySet
          val groupB = currentState.filter(_._2 == groupBid).keySet

          //move group B to group A
          val updatedState = groupB.foldLeft(currentState)((acc, next) => {
            acc.updated(next, groupAid)
          })

          val updatedCircuitSizes = currentCircuitSizes
            .removed(groupBid)
            .updated(groupAid, currentCircuitSizes(groupAid) + currentCircuitSizes(groupBid))

          (updatedState, updatedCircuitSizes, Seq(pointA, pointB))
        }
      }}

    stateAfterConnections._3.map(_.head.toLong).product
  }


  def buildKDTree(points: Seq[Seq[Int]], currentDepth: Int = 0): KDNode = {

    val dimensions = points.head.size
    if (points.size == 1) {
      return KDNode(points.head, EMPTY_NODE, EMPTY_NODE, currentDepth % dimensions)
    }

    val sortedPoints = sortPointsByDimension(points, currentDepth)
    val point = sortedPoints(sortedPoints.size / 2)
    val firstHalf = sortedPoints.takeWhile(_ != point)
    val lastHalf = sortedPoints.drop(firstHalf.size + 1)

    val firstHalfNode = firstHalf match {
      case Seq() => EMPTY_NODE
      case x => buildKDTree(x, currentDepth + 1)
    }
    val lastHalfNode = lastHalf match {
      case Seq() => EMPTY_NODE
      case x => buildKDTree(x, currentDepth + 1)
    }

    KDNode(point, firstHalfNode, lastHalfNode, currentDepth % dimensions)
  }

  private val EMPTY_NODE = KDNode(Seq(Int.MaxValue, Int.MaxValue, Int.MaxValue), null, null, -1)

  case class KDNode(point: Seq[Int],
                    firstHalf: KDNode,
                    secondHalf: KDNode,
                    sortIndex: Int) {
    lazy val isLeaf: Boolean = {
      firstHalf == EMPTY_NODE && secondHalf == EMPTY_NODE
    }

    def distanceToPoint(point: Seq[Int]): Double = {
      distance(this.point, point)
    }

    override def toString: String = {
      "[" + point.mkString(";") + s"f:${firstHalf},s:${secondHalf}]"
    }
  }


  def searchKDTree(target: Seq[Int], kdTree: KDNode): KDNode = {
    //https://medium.com/@isurangawarnasooriya/exploring-kd-trees-a-comprehensive-guide-to-implementation-and-applications-in-python-3385fd56a246
    def _search(node: KDNode, currentDepth: Int, currentBestNode: Option[KDNode]): KDNode = {
      if (node == EMPTY_NODE) {
        return currentBestNode.get
      }

      val searchDimension = currentDepth % target.size

      val nextBest = if (currentBestNode.isEmpty || (distance(target, node.point) < distance(target, currentBestNode.get.point) && node.point != target)) {
        node
      } else {
        currentBestNode.get
      }

      val (nextBranch, otherBranch) = if (target(searchDimension) < node.point(searchDimension)) {
        (node.firstHalf, node.secondHalf)
      } else {
        (node.secondHalf, node.firstHalf)
      }

      val firstSubSearch = _search(nextBranch, currentDepth + 1, Some(nextBest))

      val secondSubSearch = if (target == nextBest.point) {
        _search(otherBranch, currentDepth + 1, Some(firstSubSearch))
      } else if (distance(target, firstSubSearch.point) > Math.abs(target(searchDimension) - node.point(searchDimension))) {
        _search(otherBranch, currentDepth + 1, Some(firstSubSearch))
      } else {
        firstSubSearch
      }

      secondSubSearch
    }

    if (kdTree.point == target) {
      val first = _search(kdTree.firstHalf, 0, None)
      val second = _search(kdTree.secondHalf, 0, None)
      Seq(first,second).map(x => x -> distance(target,x.point))
        .minBy(_._2)._1
    } else {
      val closestNode = _search(kdTree, 0, None)
      closestNode
    }
  }

  def sortPointsByDimension(points: Seq[Seq[Int]], sortDimension: Int): Seq[Seq[Int]] = {
    points.sortBy(point => point(sortDimension % point.size))
  }

  def distance(a: Seq[Int], b: Seq[Int]): Double = {
    val x = a.zip(b).map { case (c, d) => d - c }.map(Math.pow(_, 2)).sum
    Math.sqrt(x)
  }

  def getPointDimension(point: Seq[Int], currentDepth: Int): Int = {
    point(currentDepth % point.size)
  }

  def getPointDimension(kdNode: KDNode, currentDepth: Int): Int = {
    getPointDimension(kdNode.point, currentDepth)
  }
}
