package utility

enum Direction {
  case North, East, South, West

  def adjustPosition(pos: Point2D): Point2D = pos + this.toPoint

  def opposite: Direction = this match {
    case North => South
    case East => West
    case South => North
    case West => East
  }

  def toPoint: Point2D = this match {
    case North => Point2D(0, -1)
    case East => Point2D(1, 0)
    case South => Point2D(0, 1)
    case West => Point2D(-1, 0)
  }
  
  def rotateClockwise: Direction = this match {
    case North => East
    case East => South
    case South => West
    case West => North
  }
}
