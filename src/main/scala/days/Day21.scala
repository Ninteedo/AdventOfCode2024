package days

import utility.*

class Day21 extends IDay {
  override def execute(input: String): (Any, Any) = {
    val pinCodes = Helper.readLines(input, PinCode.fromString)
    (part1(pinCodes), part2(pinCodes))
  }

  private def part1(pinCodes: Iterable[PinCode]): Long = pinCodes.map(_.complexity(2)).sum

  private def part2(pinCodes: Iterable[PinCode]): Long = pinCodes.map(_.complexity(25)).sum

  private case class PinCode(code: String) {
    private val asKeyPad: List[KeyPad] = code.map(KeyPad.fromChar).toList

    private val numericPart: Int = code.takeWhile(_.isDigit).toInt

    private def shortestSequence(order: Int): Long = (KeyPad.Activate :: asKeyPad)
      .zip(asKeyPad)
      .map((a, b) => a.minCountTo(b, order))
      .sum

    def complexity(order: Int): Long = shortestSequence(order) * numericPart
  }

  private object PinCode {
    def fromString(s: String): PinCode = PinCode(s)
  }

  private trait Pad[T <: Pad[T]] {
    def connections: Map[DirPad, T]

    private val minCountCache: collection.mutable.Map[(T, Int), Long] = collection.mutable.Map.empty

    def minCountTo(other: T, order: Int): Long = minCountCache.getOrElse((other, order), {
      if (order == 0) {
        val visited = collection.mutable.Set.empty[T]
        val frontier = collection.mutable.PriorityQueue((this.asInstanceOf[T], List.empty[DirPad]))(Ordering.by(-_._2.size))

        while (frontier.nonEmpty) {
          val (curr, path) = frontier.dequeue()
          if (curr == other) {
            val res = path.length + 1
            minCountCache((other, order)) = res
            return res
          }
          visited.add(curr)
          for ((dir, next) <- curr.connections) {
            if (!visited.contains(next)) {
              frontier.enqueue((next, path :+ dir))
            }
          }
        }

        throw new Exception("No path found")
      } else {
        def ordering(t: (T, Long, DirPad)): Long = {
          val (pos, pathLength, dir) = t
          val returnDist = dir.minCountTo(DirPad.Activate, order - 1)
          -(pathLength + returnDist)
        }

        val frontier = collection.mutable.PriorityQueue((this.asInstanceOf[T], 0L, DirPad.Activate))(Ordering.by(ordering))

        while (frontier.nonEmpty) {
          val (posCurr, path, dirCurr) = frontier.dequeue()
          if (posCurr == other) {
            val res = path + dirCurr.minCountTo(DirPad.Activate, order - 1)
            minCountCache((other, order)) = res
            return res
          }
          for ((dirNext, posNext) <- posCurr.connections) {
            val intermediate = dirCurr.minCountTo(dirNext, order - 1)
            frontier.enqueue((posNext, path + intermediate, dirNext))
          }
        }

        throw new Exception("No path found")
      }
    })
  }

  private enum KeyPad extends Pad[KeyPad] {
    case One, Two, Three, Four, Five, Six, Seven, Eight, Nine, Zero, Activate

    override def connections: Map[DirPad, KeyPad] = this match {
      case One      => Map(DirPad.Right -> Two, DirPad.Up -> Four)
      case Two      => Map(DirPad.Left -> One, DirPad.Right -> Three, DirPad.Up -> Five, DirPad.Down -> Zero)
      case Three    => Map(DirPad.Left -> Two, DirPad.Up -> Six, DirPad.Down -> Activate)
      case Four     => Map(DirPad.Down -> One, DirPad.Right -> Five, DirPad.Up -> Seven)
      case Five     => Map(DirPad.Left -> Four, DirPad.Right -> Six, DirPad.Up -> Eight, DirPad.Down -> Two)
      case Six      => Map(DirPad.Left -> Five, DirPad.Up -> Nine, DirPad.Down -> Three)
      case Seven    => Map(DirPad.Down -> Four, DirPad.Right -> Eight)
      case Eight    => Map(DirPad.Left -> Seven, DirPad.Down -> Five, DirPad.Right -> Nine)
      case Nine     => Map(DirPad.Left -> Eight, DirPad.Down -> Six)
      case Zero     => Map(DirPad.Up -> Two, DirPad.Right -> Activate)
      case Activate => Map(DirPad.Left -> Zero, DirPad.Up -> Three)
    }
  }

  private object KeyPad {
    def fromChar(c: Char): KeyPad = c match {
      case '1' => One
      case '2' => Two
      case '3' => Three
      case '4' => Four
      case '5' => Five
      case '6' => Six
      case '7' => Seven
      case '8' => Eight
      case '9' => Nine
      case '0' => Zero
      case 'A' => Activate
    }
  }

  private enum DirPad extends Pad[DirPad] {
    case Up, Down, Left, Right, Activate

    override def connections: Map[DirPad, DirPad] = this match {
      case Up       => Map(DirPad.Down -> Down, DirPad.Right -> Activate)
      case Down     => Map(DirPad.Up -> Up, DirPad.Left -> Left, DirPad.Right -> Right)
      case Left     => Map(DirPad.Right -> Down)
      case Right    => Map(DirPad.Left -> Down, DirPad.Up -> Activate)
      case Activate => Map(DirPad.Left -> Up, DirPad.Down -> Right)
    }

    def toChar: Char = this match {
      case Up       => '^'
      case Down     => 'v'
      case Left     => '<'
      case Right    => '>'
      case Activate => 'A'
    }
  }
}
