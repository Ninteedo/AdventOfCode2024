package days

import utility.*

class Day14 extends IDay {
  override def execute(input: String): (Any, Any) = {
    val robots = Helper.readLines(input, Robot.from).toList
    (part1(robots), part2(robots))
  }

  private val bounds = Point2D(101, 103)

  private def part1(robots: List[Robot]) = {
    def quadrant(pos: Point2D): Option[(Boolean, Boolean)] = {
      if pos.x == bounds.x / 2 || pos.y == bounds.y / 2
      then None
      else Some((pos.x < bounds.x / 2, pos.y < bounds.y / 2))
    }

    robots
      .map(_.positionAfter(100))
      .groupBy(quadrant)
      .filter(_._1.isDefined)
      .values
      .map(_.size)
      .product
  }

  private def part2(robots: List[Robot]) = {
    def robotVariance(positions: List[Point2D]): Float = {
      val average = Point2D(positions.map(_.x).sum / positions.size, positions.map(_.y).sum / positions.size)
      positions.map(p => p.mannDist(average)).sum / positions.size.toFloat
    }

//    def robotPositionsToText(positions: List[Point2D]) = {
//      val occupied = positions.groupBy(identity)
//      val sb = new StringBuilder(bounds.x * bounds.y)
//
//      for (y <- 0 until bounds.y) {
//        for (x <- 0 until bounds.x) {
//          val pos = Point2D(x, y)
//          sb.append(occupied.getOrElse(pos, List()) match {
//            case Nil => '.'
//            case x: List[Point2D] => '#'
//          })
//        }
//        sb.append('\n')
//      }
//
//      sb.toString
//    }
//
//    for (i <- 0 to 10000) {
//      val seconds = i
//      val positions = robots.map(_.positionAfter(seconds))
//      if (robotVariance(positions) < 30) {
//        println(s"Time: $seconds")
//        println(robotPositionsToText(positions))
//        println(robotVariance(positions))
//        Thread.sleep(300)
//      }
//    }

    Range(0, 10000).find(i => robotVariance(robots.map(_.positionAfter(i))) < 30).get
  }

  private case class Robot(start: Point2D, vel: Point2D) {
    def positionAfter(seconds: Long): Point2D = {
      val x = start.x + vel.x * seconds
      val y = start.y + vel.y * seconds

      Point2D(negMod(x, bounds.x), negMod(y, bounds.y))
    }

    private def negMod(a: Long, divisor: Int): Int = {
      val x = a % divisor
      (if (x >= 0) x else x + divisor).toInt
    }
  }

  private object Robot {
    def from(line: String): Robot = {
      "p=(\\d+),(\\d+) v=(-?\\d+),(-?\\d+)".r.findFirstMatchIn(line) match {
        case Some(m) => Robot(Point2D(m.group(1).toInt, m.group(2).toInt), Point2D(m.group(3).toInt, m.group(4).toInt))
        case None => throw new Exception("Could not parse robot line " + line)
      }
    }
  }
}
