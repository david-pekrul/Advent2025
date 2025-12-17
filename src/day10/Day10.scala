package day10

import scala.annotation.tailrec

object Day10 {
  def main(args: Array[String]): Unit = {
    val start = System.nanoTime()
    //    val input = helpers.Helpers.readTestFile(this)
    val input = helpers.Helpers.readFile(this)

    val machines = parse(input)

    val part1 = machines.map(_.part1Answer()).sum
    println(s"Part 1: $part1")

    //    val test = Machine(4, 0, 0, Seq(8, 10, 4, 12, 5, 3), Seq(2,0,0,2)).solvePart2
    //    println(test)

    val part2 = machines.map(_.solvePart2).sum
    println(s"Part 2: $part2")

    val end = System.nanoTime() - start
    println(s"Time: ${end / 1000.0 / 1000.0}ms")
  }

  def parse(input: Seq[String]): Seq[Machine] = {
    input.map(line => {
      val split = line.split(" ")
      val targetLights = split.head.replace("[", "").replace("]", "")
        .map {
          case '#' => 1
          case _ => 0
        }
      val joltages = split.last.drop(1).dropRight(1).split(",").map(_.toInt).toSeq
      val buttons = split.drop(1).dropRight(1).map(buttonString => {
        buttonString.replaceAll("[()]", "").split(",")
          .map(_.toInt).toSeq
      }).toSeq
      Machine(targetLights, buttons, joltages)
    })
  }


  case class Machine(targetLights: Seq[Int], buttons: Seq[Seq[Int]], joltage: Seq[Int]) {


    val binaryOptions = getBinaryOptions(buttons.length)

    def part1Answer(): Long = {


      val buttonCombinations = binaryOptions.map(bits => {
        val pressedButtons = buttons.zipWithIndex
          .filter { case (btn, idx) => bits(idx) }
          .map(_._1)
        pressedButtons
      })

      val emptyLights = targetLights.indices.map(_ => 0)

      val appliedCombos = buttonCombinations.map(buttonCombo => {
        buttonCombo -> buttonCombo.foldLeft(emptyLights)((state, nextButton) => {
          nextButton.foldLeft(state)((state2, idx) => {
            state2.updated(idx, state2(idx) ^ 1)
          })
        })
      })

      val combosThatWork = appliedCombos.filter(_._2 == targetLights)
      combosThatWork.map(_._1.length).min
    }

    def solvePart2: Long = {

      //Seq[Buttons, Resulting Joltages From Buttons]
      def combosThatEndInAllEvens(currentTarget: Seq[Int]): Seq[(Seq[Seq[Int]], Seq[Int])] = {

        val buttonCombinations = binaryOptions.map(bits => {
          val pressedButtons = buttons.zipWithIndex
            .filter { case (btn, idx) => bits(idx) }
            .map(_._1)
          pressedButtons
        })

        val emptyLights = targetLights.indices.map(_ => 0)

        //(buttons,resulting joltages)
        val appliedCombos = buttonCombinations.map(buttonCombo => {
          buttonCombo -> buttonCombo.foldLeft(emptyLights)((state, nextButton) => {
            nextButton.foldLeft(state)((state2, idx) => {
              state2.updated(idx, state2(idx) + 1)
            })
          })
        })

        //TODO: Check which end states get us to all even integers for the result.
        //  currentTarget - result ==> all even values
        val combosThatResultInAllEvens = appliedCombos
          .map { case (buttonCombo, joltagesFromButtons) => {
            buttonCombo -> currentTarget.zip(joltagesFromButtons).map { case (a, b) => a - b }
          }
          }
          .filter { case (buttonCombo, remainingJoltage) => {
            remainingJoltage.forall(j => j % 2 == 0 && j >= 0)
          }
          }

        combosThatResultInAllEvens
      }

      def _run(currentTarget: Seq[Int], depth: Int = 0): Option[Long] = {

        if (currentTarget.forall(_ == 0)) {
          return Some(0L)
        }

        val options = combosThatEndInAllEvens(currentTarget)
        if (options.isEmpty) {
          //The target is not empty, and no solution was found.
          return None
        }

        val result = options.map { case (buttons, resultingState) => {
          val halfedAnswerOpt = _run(resultingState.map(_ / 2), depth + 1)
          halfedAnswerOpt.map(h => buttons.size.toLong + 2 * h)
        }
        }.filter(_.isDefined).map(_.get)

        if result.isEmpty then {
          None
        } else {
          Some(result.min)
        }
      }

      val answer = _run(joltage)
      println(s"Solved: $answer => $this")
      answer.get
    }

  }


  def getBinaryOptions(length: Int): Seq[Seq[Boolean]] = {

    @tailrec
    def _run(current: Seq[Seq[Boolean]], depth: Int = 0): Seq[Seq[Boolean]] = {
      if (current.head.length == length) {
        return current
      }

      val next = current.flatMap(c => Seq(c :+ true, c :+ false))
      _run(next, depth + 1)
    }

    _run(Seq(Seq(true), Seq(false)))
  }
}
