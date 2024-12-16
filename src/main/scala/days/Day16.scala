package days

import utility.*

import scala.collection.mutable

class Day16 extends IDay {

  override def execute(input: String): (Any, Any) = {
    val maze = Maze(Grid2D.from2DCharArray(input, MazeTile.from))
    (part1(maze), part2(maze))
  }

  private def part1(maze: Maze): Int = {
    MazeSearchNode(maze, maze.startPos, Direction.East, 0, Set(maze.startPos), None)
      .bestFirstSearch()
      .get
      .getResult
  }

  private def part2(maze: Maze): Int = {
    MazeSearchNode(maze, maze.startPos, Direction.East, 0, Set(maze.startPos), None)
      .allBestSearch()
      .flatMap(node => node.visited)
      .toSet
      .size
  }

  private case class MazeSearchNode(
    maze: Maze,
    pos: Point2D,
    dir: Direction,
    score: Int,
    visited: Set[Point2D],
    parent: Option[MazeSearchNode]
  ) extends SearchNode[MazeSearchNode] {

    override def calculateOrderingValue: Int = {
      pos.mannDist(maze.goalPos) - score
    }

    override def descendents: Iterable[MazeSearchNode] = {
      var res = List.empty[MazeSearchNode]
      if (maze.traversable(pos + dir.rotateClockwise)) {
        res = res :+ MazeSearchNode(maze, pos, dir.rotateClockwise, score + 1000, visited, Some(this))
      }
      if (maze.traversable(pos + dir.rotateCounterClockwise)) {
        res = res :+ MazeSearchNode(maze, pos, dir.rotateCounterClockwise, score + 1000, visited, Some(this))
      }
      if (maze.traversable(pos + dir)) {
        res = res :+ MazeSearchNode(maze, pos + dir, dir, score + 1, visited + (pos + dir), Some(this))
      }
      res
    }

    override lazy val atGoal: Boolean = pos == maze.goalPos
    override lazy val getParent: Option[MazeSearchNode] = parent
    override lazy val getResult: Int = score

    override val filterDuplicates: Boolean = true

    override def isDuplicateOf(other: SearchNode[MazeSearchNode]): Boolean = {
      val otherNode = other.asInstanceOf[MazeSearchNode]
      pos == otherNode.pos && dir == otherNode.dir && score >= otherNode.score
    }

    def isDuplicatePosition(other: SearchNode[MazeSearchNode]): Boolean = {
      val otherNode = other.asInstanceOf[MazeSearchNode]
      pos == otherNode.pos && dir == otherNode.dir
    }

    override def hashCode(): Int = (maze.hashCode() * 31 + pos.hashCode()) * 31 + dir.hashCode()

    override def toString: String = s"MazeSearchNode($pos, $dir, $score)"

    def allBestSearch(): List[MazeSearchNode] = {
      val frontier: mutable.PriorityQueue[MazeSearchNode] = mutable.PriorityQueue(this)(Ordering.by(_.orderingValue))
      val visited: mutable.Map[(Point2D, Direction), Int] = mutable.Map()

      var result: List[MazeSearchNode] = List()
      var goalMinValue: Option[Int] = None

      while (frontier.nonEmpty) {
        val node: MazeSearchNode = frontier.dequeue()

        if (goalMinValue.isDefined && node.getResult > goalMinValue.get) {
          return result
        }

        if (node.atGoal) {
          if (goalMinValue.isEmpty) {
            goalMinValue = Some(node.getResult)
          } else if (node.getResult > goalMinValue.get) {
            throw new RuntimeException("goal value increased")
          }
          result = result :+ node
        } else {
          if (!(filterDuplicates && visited.get((node.pos, node.dir)).exists(_ < node.score))) {
            var nodesToAdd = node.descendents
            if (filterDuplicates) {
              visited.put((node.pos, node.dir), node.score)
              nodesToAdd = nodesToAdd.filterNot(newNode => visited.get((newNode.pos, newNode.dir)).exists(_ < newNode.score))
            }
            nodesToAdd.foreach(frontier.enqueue(_))
          }
        }
      }

      result
    }

  }

  private case class Maze(grid: Grid2D[MazeTile]) {
    lazy val startPos: Point2D = grid.indexWhere(_ == MazeTile.Start).get

    lazy val goalPos: Point2D = grid.indexWhere(_ == MazeTile.End).get

    def traversable(pos: Point2D): Boolean = grid.at(pos) != MazeTile.Wall

    def toString(pos: Point2D, dir: Direction, visited: Set[Point2D]): String = {
      grid.toGridStringByIndex(p => {
        if (p == pos) {
          dir match {
            case Direction.North => "^"
            case Direction.East => ">"
            case Direction.South => "v"
            case Direction.West => "<"
          }
        } else {
          grid.at(p) match {
            case MazeTile.Wall => "#"
            case MazeTile.Empty => if (visited.contains(p)) "O" else "."
            case MazeTile.Start => "S"
            case MazeTile.End => "E"
          }
        }
      })
    }
  }

  private enum MazeTile {
    case Wall, Empty, Start, End
  }

  private object MazeTile {
    def from: Char => MazeTile = {
      case '#' => Wall
      case '.' => Empty
      case 'S' => Start
      case 'E' => End
    }
  }
}
