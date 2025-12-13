package day10

import scala.annotation.tailrec

object Day10 {
  def main(args: Array[String]): Unit = {
    val start = System.nanoTime()
    //    val input = helpers.Helpers.readTestFile(this)
    val input = helpers.Helpers.readFile(this)

    val machines = parse(input)

    val part1 = machines.map(_.part1Answer).sum
    println(s"Part 1: $part1")

    val end = System.nanoTime() - start
    println(s"Time: ${end / 1000.0 / 1000.0}ms")
  }

  def parse(input: Seq[String]): Seq[Machine] = {
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
      Machine(lightsString.length.shortValue, 0, lights, buttons, joltages)
    })
  }


  case class Machine(lightLength: Short, currentLights: Short, targetLights: Short, buttons: Seq[Short], joltage: Seq[Int]) {
    override def toString: String = {
      s"Mach[${lightToString(lightLength, currentLights)}, ${lightToString(lightLength, targetLights)}, ${buttons}"
    }

    def pressButton(buttonIndex: Int): Machine = {
      Machine(lightLength, (currentLights ^ buttons(buttonIndex)).shortValue, targetLights, buttons, joltage)
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
