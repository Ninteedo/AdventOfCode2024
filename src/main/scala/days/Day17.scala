package days

import utility.*

import scala.annotation.tailrec

class Day17 extends IDay {
  override def execute(input: String): (Any, Any) = {
    val lines = input.split("\n").filter(_.nonEmpty).map(_.trim)
    val a = lines(0).split(": ")(1).toLong
    val b = lines(1).split(": ")(1).toLong
    val c = lines(2).split(": ")(1).toLong
    val program = lines(3).split(": ")(1).split(",").map(_.toLong).toList
    val initialState = ExecutionState(a, b, c, program, 0, List.empty)

    (part1(initialState), part2(initialState))
  }

  private def part1(initialState: ExecutionState): String = {
    initialState.executeToCompletion.outputs.mkString(",")
  }

  private def part2(initialState: ExecutionState): Long = {
    @tailrec
    def reverseCalc(as: Set[Long], remaining: List[Long], alreadyExpected: List[Long]): Set[Long] = remaining match {
      case Nil => as
      case x :: xs =>
        val newExpected = x :: alreadyExpected
        val newAs = as
          .map(_ * 8)
          .flatMap(a => (0 until 8).map(a | _))
          .filter(newA => initialState.replaceARegister(newA).executeToCompletion.outputs == newExpected)
        reverseCalc(newAs, xs, newExpected)
    }

    reverseCalc(Set(0), initialState.instructions.reverse, List.empty).min
  }

  private case class ExecutionState(
    a: Long,
    b: Long,
    c: Long,
    instructions: List[Long],
    instructionPointer: Int,
    outputs: List[Long]
  ) {
    private def isComplete: Boolean = instructionPointer >= instructions.length

    private def executeNextInstruction: ExecutionState = {
      val opcode = instructions(instructionPointer)
      val instruction = Instruction.get(opcode, this)
      instruction.execute()
    }

    def executeToCompletion: ExecutionState = {
      if (isComplete) this
      else executeNextInstruction.executeToCompletion
    }

    def replaceARegister(newValue: Long): ExecutionState = ExecutionState(
      newValue,
      b,
      c,
      instructions,
      instructionPointer,
      outputs
    )
  }

  private abstract class Instruction(state: ExecutionState) {
    def execute(): ExecutionState

    def comboOperandValue(operand: Long): Long = {
      if (operand <= 3) operand
      else operand match {
        case 4 => state.a
        case 5 => state.b
        case 6 => state.c
      }
    }

    def operand: Long = state.instructions(state.instructionPointer + 1)
  }

  private object Instruction {
    def get(opcode: Long, state: ExecutionState): Instruction = opcode match {
      case 0 => Adv(state)
      case 1 => Bxl(state)
      case 2 => Bst(state)
      case 3 => Jnz(state)
      case 4 => Bxc(state)
      case 5 => Out(state)
      case 6 => Bdv(state)
      case 7 => Cdv(state)
    }
  }

  private case class Adv(state: ExecutionState) extends Instruction(state) {
    override def execute(): ExecutionState = {
      val newA = state.a / Math.pow(2, comboOperandValue(operand)).toLong
      ExecutionState(
        newA,
        state.b,
        state.c,
        state.instructions,
        state.instructionPointer + 2,
        state.outputs
      )
    }
  }

  private case class Bxl(state: ExecutionState) extends Instruction(state) {
    override def execute(): ExecutionState = {
      val newB = state.b ^ operand
      ExecutionState(
        state.a,
        newB,
        state.c,
        state.instructions,
        state.instructionPointer + 2,
        state.outputs
      )
    }
  }

  private case class Bst(state: ExecutionState) extends Instruction(state) {
    override def execute(): ExecutionState = {
      val newB = comboOperandValue(operand) % 8
      ExecutionState(
        state.a,
        newB,
        state.c,
        state.instructions,
        state.instructionPointer + 2,
        state.outputs
      )
    }
  }

  private case class Jnz(state: ExecutionState) extends Instruction(state) {
    override def execute(): ExecutionState = {
      val newInstrPointer = if (state.a == 0) state.instructionPointer + 2 else operand.toInt
      ExecutionState(
        state.a,
        state.b,
        state.c,
        state.instructions,
        newInstrPointer,
        state.outputs
      )
    }
  }

  private case class Bxc(state: ExecutionState) extends Instruction(state) {
    override def execute(): ExecutionState = {
      val newB = state.b ^ state.c
      ExecutionState(
        state.a,
        newB,
        state.c,
        state.instructions,
        state.instructionPointer + 2,
        state.outputs
      )
    }
  }

  private case class Out(state: ExecutionState) extends Instruction(state) {
    override def execute(): ExecutionState = {
      val newOutputs = state.outputs :+ comboOperandValue(operand) % 8
      ExecutionState(
        state.a,
        state.b,
        state.c,
        state.instructions,
        state.instructionPointer + 2,
        newOutputs
      )
    }
  }

  private case class Bdv(state: ExecutionState) extends Instruction(state) {
    override def execute(): ExecutionState = {
      val newB = state.a / Math.pow(2, comboOperandValue(operand)).toLong
      ExecutionState(
        state.a,
        newB,
        state.c,
        state.instructions,
        state.instructionPointer + 2,
        state.outputs
      )
    }
  }

  private case class Cdv(state: ExecutionState) extends Instruction(state) {
    override def execute(): ExecutionState = {
      val newC = state.a / Math.pow(2, comboOperandValue(operand)).toLong
      ExecutionState(
        state.a,
        state.b,
        newC,
        state.instructions,
        state.instructionPointer + 2,
        state.outputs
      )
    }
  }
}
