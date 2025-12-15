package day10

import scala.annotation.tailrec

object Day10 {
  def main(args: Array[String]): Unit = {
    val start = System.nanoTime()
//            val input = helpers.Helpers.readTestFile(this)
    val input = helpers.Helpers.readFile(this)

    val machines = parse1(input)

    val part1 = machines.map(_.part1Answer).sum
    println(s"Part 1: $part1")

    val machines2 = parse2(input)
    val part2 = machines2.map(_.solvePart2()).sum

    println(s"Part 2: $part2")

    val end = System.nanoTime() - start
    println(s"Time: ${end / 1000.0 / 1000.0}ms")
  }

  def parse1(input: Seq[String]): Seq[Machine] = {
    input.map(line => {
      val split = line.split(" ")
      val lightsString = split.head.replace("[", "").replace("]", "")
      val lights = lightsString.toCharArray.zipWithIndex.map { case (c, idx) =>
        if (c == '#') {
          Math.pow(2, idx).shortValue
        } else {
          0
        }
      }.sum
      val joltages = split.last.drop(1).dropRight(1).split(",").map(_.toInt).toSeq
      val buttons = split.drop(1).dropRight(1).map(buttonString => {
        buttonString.replaceAll("[()]", "").split(",")
          .map(_.toInt)
          .map(targetLightIdx => Math.pow(2, targetLightIdx).shortValue)
          .sum
      })
      Machine(lightsString.length.shortValue, 0, lights, buttons)
    })
  }

  def parse2(input: Seq[String]): Seq[Machine2] = {
    input.map(line => {
      val split = line.split(" ")
      val lightsString = split.head.replace("[", "").replace("]", "")
      val joltages = split.last.drop(1).dropRight(1).split(",").map(_.toInt).toSeq
      val buttons = split.drop(1).dropRight(1).map(buttonString => {
        buttonString.replaceAll("[()]", "").split(",")
          .map(_.toInt).toSeq
      }).toSeq
      Machine2(buttons, joltages)
    })
  }

  case class Machine(lightLength: Short, currentLights: Short, targetLights: Short, buttons: Seq[Short]) {
    override def toString: String = {
      s"Mach[${lightToString(lightLength, currentLights)}, ${lightToString(lightLength, targetLights)}, ${buttons}"
    }

    def pressButton(buttonIndex: Int): Machine = {
      Machine(lightLength, (currentLights ^ buttons(buttonIndex)).shortValue, targetLights, buttons)
    }

    /* There is NO point in pressing a button twice for part 1. */
    def part1Answer: Int = {

      Range(1, Math.pow(2, buttons.size).shortValue).foldLeft(Int.MaxValue) { case (currentLowestCount, nextButtonCombo) => {
        val buttonIndexes = getBinary(nextButtonCombo).zipWithIndex.filter(_._1 == 1)
        if (buttonIndexes.size > currentLowestCount) {
          //not even possible to best it
          currentLowestCount
        } else {
          //do the calculation
          val (lightState, buttonsUsed) = buttonIndexes.foldLeft((0.shortValue, 0)) { case ((current, btnCount), (btnFlag, btnIndex)) => {
            btnFlag match {
              case 0 => (current, btnCount)
              case 1 => ((current ^ buttons(btnIndex)).shortValue, btnCount + 1)
            }
          }
          }

          if (lightState == targetLights) {
            Math.min(buttonsUsed, currentLowestCount)
          } else {
            currentLowestCount
          }
        }
      }
      }

    }
  }


  case class Machine2(buttons: Seq[Seq[Int]], joltage: Seq[Int]) {



    def solvePart2(): Int = {

      @tailrec
      def _run(currentButtonStates: Seq[(Seq[Int], Int)]): Int = {

        //at this point, are there enough buttons left to be updated to cover all the missing joltages???


        val updatedStates = currentButtonStates.flatMap { case (currentButtons, lastUpdatedIdx) => {
          val updatedIndexes = Range(lastUpdatedIdx, buttons.size).map(idxToIncrement => {

            val updated = currentButtons.updated(idxToIncrement, currentButtons(idxToIncrement) + 1)
            val updatedInfo = validAndFinished(updated)
            updatedInfo -> (updated,idxToIncrement)
          })
          updatedIndexes.filter(_._1._1)
        }
        }.filter(_._1._1)



        val finishedOpt = updatedStates.find(stateKv => stateKv._1._2) //finished

        if (finishedOpt.isDefined) {
          println("DONE! " + finishedOpt)
          val f = finishedOpt.get._2._1.sum
          f
        } else {
          _run(updatedStates.map(_._2))
        }
      }


      val initialState = Seq((buttons.map(_ => 0), 0))
      val machineAnswer = _run(initialState)
      println(machineAnswer -> this)
      machineAnswer
    }

    def maxValueForBtn(btnIdx: Int): Int = {
      buttons(btnIdx).map(joltageIdx => joltage(joltageIdx)).min
    }

    def stillValid(buttonPressCounts: Seq[Int]): Boolean = {
      val thing = buttonPressCounts.zipWithIndex
        .flatMap { case (btnCount, btnIdx) => buttons(btnIdx).map(joltageIndex => joltageIndex -> btnCount) }
        .groupBy(_._1)
        .map(kv => kv._1 -> kv._2.map(_._2).sum)
      !thing.exists { case (idx, currentCount) => joltage(idx) < currentCount }
    }

    def finished(buttonPressCounts: Seq[Int]): Boolean = {
      val thing = buttonPressCounts.zipWithIndex
        .flatMap { case (btnCount, btnIdx) => buttons(btnIdx).map(joltageIndex => joltageIndex -> btnCount) }
        .groupBy(_._1)
        .map(kv => kv._1 -> kv._2.map(_._2).sum)
      thing.forall { case (idx, currentCount) => joltage(idx) == currentCount }
    }

    //valid , finished
    def validAndFinished(buttonPressCounts: Seq[Int]): (Boolean, Boolean) = {
      val thing = buttonPressCounts.zipWithIndex
        .flatMap { case (btnCount, btnIdx) => buttons(btnIdx).map(joltageIndex => joltageIndex -> btnCount) }
        .groupBy(_._1)
        .map(kv => kv._1 -> kv._2.map(_._2).sum)
      val valid = !thing.exists { case (idx, currentCount) => joltage(idx) < currentCount }
      val finished = valid && thing.forall { case (idx, currentCount) => joltage(idx) == currentCount }
      (valid, finished)
    }
  }


  def lightToString(lightLength: Short, light: Short): String = {

    @tailrec
    def _toStr(lightLength: Short, remaining: Short, str: String): String = {
      if (lightLength == 0) {
        return str
      }

      def next = remaining % 2 match {
        case 0 => str + '_'
        case 1 => str + '#'
      }

      _toStr((lightLength - 1).shortValue, (remaining >> 1).shortValue, next)
    }

    _toStr(lightLength, light, "")
  }

  @tailrec
  def getBinary(input: Int, indexes: Seq[Int] = Seq()): Seq[Int] = {
    if (input == 0) {
      return indexes
    }

    def next = indexes :+ (input % 2)

    getBinary(input / 2, next)
  }
}
