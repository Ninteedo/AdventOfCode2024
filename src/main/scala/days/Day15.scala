package days

import utility.*

class Day15 extends IDay {
  override def execute(input: String): (Any, Any) = {
    val splits = input.split("\n\n")
    val warehouseMap = WarehouseMapSingle.from(splits(0))
    val instructions = parseInstructions(splits(1).replace("\n", "").trim)
    (part1(warehouseMap, instructions), part2(warehouseMap, instructions))
  }

  private def part1(warehouseMap: WarehouseMapSingle, instructions: List[Direction]): Int = {
    warehouseMap.applyInstructions(instructions).boxScore
  }

  private def part2(warehouseMap: WarehouseMapSingle, instructions: List[Direction]): Int = {
    WarehouseMapDouble.from(warehouseMap).applyInstructions(instructions).boxScore
  }

  private abstract class WarehouseMap(grid: Grid2D[WarehouseMapTile], robotPos: Point2D) {
    def robotMove(instruction: Direction): WarehouseMap
    def boxScore: Int

    def applyInstructions(instructions: List[Direction]): WarehouseMap = {
      instructions.foldLeft(this)((map, instruction) => map.robotMove(instruction))
    }

    def toGridString: String = grid.toGridString({
      case WarehouseMapTile.Wall => "#"
      case WarehouseMapTile.Empty => "."
      case WarehouseMapTile.Box => "O"
      case WarehouseMapTile.Robot => "@"
      case WarehouseMapTile.DoubleBoxLeft => "["
      case WarehouseMapTile.DoubleBoxRight => "]"
    })
  }

  private case class WarehouseMapSingle(grid: Grid2D[WarehouseMapTile], robotPos: Point2D) extends WarehouseMap(grid, robotPos) {
    override def robotMove(instruction: Direction): WarehouseMapSingle = {
      var newRobotPos = robotPos
      var newGrid = grid

      def recMove(pos: Point2D): Boolean = {
        if (grid.at(pos) == WarehouseMapTile.Wall) false
        else if (grid.at(pos) == WarehouseMapTile.Box) {
          if (recMove(pos + instruction)) {
            newGrid = newGrid.updated(pos, WarehouseMapTile.Empty)
            newGrid = newGrid.updated(pos + instruction, WarehouseMapTile.Box)
            true
          } else false
        } else true
      }

      if (recMove(robotPos + instruction)) {
        newRobotPos = robotPos + instruction
        newGrid = newGrid.updated(robotPos, WarehouseMapTile.Empty)
        newGrid = newGrid.updated(newRobotPos, WarehouseMapTile.Robot)
      }

      WarehouseMapSingle(newGrid, newRobotPos)
    }

    def boxScore: Int = {
      grid.indicesWhere(_ == WarehouseMapTile.Box).map(pos => pos.x + 100 * pos.y).sum
    }
  }

  private object WarehouseMapSingle {
    def from(s: String): WarehouseMapSingle = {
      val grid = Grid2D.from2DCharArray(s, WarehouseMapTile.from)
      val robotPos = grid.indexWhere(_ == WarehouseMapTile.Robot).get
      WarehouseMapSingle(grid, robotPos)
    }
  }

  private case class WarehouseMapDouble(grid: Grid2D[WarehouseMapTile], robotPos: Point2D) extends WarehouseMap(grid, robotPos) {
    override def robotMove(instruction: Direction): WarehouseMapDouble = {
      var newRobotPos = robotPos
      var newGrid = grid

      def checkMove(pos: Point2D): Boolean = {
        def checkDouble(l: Point2D, r: Point2D): Boolean = {
          (l + instruction == r || checkMove(l + instruction)) && (r + instruction == l || checkMove(r + instruction))
        }

        val tile = grid.at(pos)
        if (tile == WarehouseMapTile.DoubleBoxLeft) {
          val other = pos + Direction.East
          checkDouble(pos, other)
        } else if (tile == WarehouseMapTile.DoubleBoxRight) {
          val other = pos + Direction.West
          checkDouble(other, pos)
        } else grid.at(pos) == WarehouseMapTile.Empty
      }

      def recMove(pos: Point2D): Unit = {
        def doubleMove(l: Point2D, r: Point2D): Unit = {
          if (l + instruction != r) recMove(l + instruction)
          if (r + instruction != l) recMove(r + instruction)
          newGrid = newGrid.updated(l, WarehouseMapTile.Empty)
          newGrid = newGrid.updated(r, WarehouseMapTile.Empty)
          newGrid = newGrid.updated(l + instruction, WarehouseMapTile.DoubleBoxLeft)
          newGrid = newGrid.updated(r + instruction, WarehouseMapTile.DoubleBoxRight)
        }

        val tile = newGrid.at(pos)
        if (tile == WarehouseMapTile.Empty || tile == WarehouseMapTile.Wall) return
        else if (tile == WarehouseMapTile.DoubleBoxLeft) {
          val otherPos = pos + Direction.East
          doubleMove(pos, otherPos)
        } else if (tile == WarehouseMapTile.DoubleBoxRight) {
          val otherPos = pos + Direction.West
          doubleMove(otherPos, pos)
        } else {
          recMove(pos + instruction)
          newGrid = newGrid.updated(pos, WarehouseMapTile.Empty)
          newGrid = newGrid.updated(pos + instruction, tile)
        }
      }

      if (checkMove(robotPos + instruction)) {
        recMove(robotPos)
        newRobotPos = robotPos + instruction
      }

      WarehouseMapDouble(newGrid, newRobotPos)
    }

    def boxScore: Int = {
      grid.indicesWhere(_ == WarehouseMapTile.DoubleBoxLeft).map(pos => pos.x + 100 * pos.y).sum
    }
  }

  private object WarehouseMapDouble {
    def from(w: WarehouseMapSingle): WarehouseMapDouble = {
      def double(t: WarehouseMapTile): List[WarehouseMapTile] = t match {
        case WarehouseMapTile.Box => List(WarehouseMapTile.DoubleBoxLeft, WarehouseMapTile.DoubleBoxRight)
        case WarehouseMapTile.Wall => List(WarehouseMapTile.Wall, WarehouseMapTile.Wall)
        case WarehouseMapTile.Empty => List(WarehouseMapTile.Empty, WarehouseMapTile.Empty)
        case WarehouseMapTile.Robot => List(WarehouseMapTile.Robot, WarehouseMapTile.Empty)
      }

      val grid = Grid2D(w.grid.entries.map(row => row.flatMap(double)))
      val robotPos = grid.indexWhere(_ == WarehouseMapTile.Robot).get
      WarehouseMapDouble(grid, robotPos)
    }
  }

  private enum WarehouseMapTile {
    case Wall, Empty, Box, Robot, DoubleBoxLeft, DoubleBoxRight
  }

  private object WarehouseMapTile {
    def from(c: Char): WarehouseMapTile = c match {
      case '#' => Wall
      case '.' => Empty
      case 'O' => Box
      case '@' => Robot
    }
  }

  private def parseInstructions(input: String): List[Direction] = input.map({
    case '^' => Direction.North
    case 'v' => Direction.South
    case '<' => Direction.West
    case '>' => Direction.East
    case other => throw new Exception(s"Invalid direction: $other")
  }).toList
}
