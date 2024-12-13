package days

import utility.*

class Day13 extends IDay {
  override def execute(input: String): (Any, Any) = {
    val clawMachines = input.split("\n\n").map(ClawMachine.parseClawMachine).toList
    (part1(clawMachines), part2(clawMachines))
  }

  private def part1(clawMachines: List[ClawMachine]): Long = tokenCount(clawMachines)

  private def part2(clawMachines: List[ClawMachine]): Long = tokenCount(clawMachines, 10000000000000L)

  private def tokenCount(clawMachines: List[ClawMachine], offset: Long = 0): Long = {
    clawMachines.map(_.minTokens(offset)).filter(_.isDefined).map(_.get).sum
  }

  private case class ClawMachine(buttonA: Point2D, buttonB: Point2D, goal: Point2D) {
    def minTokens(offset: Long = 0): Option[Long] = {
      val b = calcPresses(buttonA, buttonB, offset)
      val a = calcPresses(buttonB, buttonA, offset)

      if (buttonA.x * a + buttonB.x * b == goal.x + offset && buttonA.y * a + buttonB.y * b == goal.y + offset)
        Some(a * 3 + b)
      else None
    }

    private def calcPresses(first: Point2D, second: Point2D, offset: Long = 0): Long = {
      ((goal.x + offset) * first.y - first.x * (goal.y + offset)) / (second.x * first.y - first.x * second.y)
    }
  }

  private case class ClawMachineParseException(s: String, missing: String)
    extends Exception(s"Could not parse ClawMachine, missing $missing.\n$s")

  private object ClawMachine {
    def parseClawMachine(s: String): ClawMachine = {
      val buttonAPattern = "Button A: X\\+(\\d+), Y\\+(\\d+)".r
      val buttonBPattern = "Button B: X\\+(\\d+), Y\\+(\\d+)".r
      val prizePattern = "Prize: X=(\\d+), Y=(\\d+)".r

      val buttonA = buttonAPattern.findFirstMatchIn(s) match {
        case Some(m) => Point2D(m.group(1).toInt, m.group(2).toInt)
        case None => throw ClawMachineParseException(s, "Button A")
      }
      val buttonB = buttonBPattern.findFirstMatchIn(s) match {
        case Some(m) => Point2D(m.group(1).toInt, m.group(2).toInt)
        case None => throw ClawMachineParseException(s, "Button B")
      }
      val prize = prizePattern.findFirstMatchIn(s) match {
        case Some(m) => Point2D(m.group(1).toInt, m.group(2).toInt)
        case None => throw ClawMachineParseException(s, "Prize")
      }
      ClawMachine(buttonA, buttonB, prize)
    }
  }
}
