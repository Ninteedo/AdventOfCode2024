package days

import utility.*

class Day23 extends IDay {
  override def execute(input: String): (Any, Any) = {
    val connections = Helper.readLines(input, Connection.fromString).toList
    val connsByComputer = connectionsByComputer(connections)
    (part1(connections, connsByComputer), part2(connections, connsByComputer))
  }

  private def part1(conns: List[Connection], connMap: Map[Computer, List[Computer]]): Int = {
    val tripleInterconnections: Set[MultiConnection] = {
      val tripleInterconnections = for {
        conn <- conns
        third <- connMap(conn.fst).intersect(connMap(conn.snd))
      } yield MultiConnection(Set(conn.fst, conn.snd, third))
      tripleInterconnections.toSet
    }

    tripleInterconnections.count(_.maybeHistorian)
  }

  private def part2(conns: List[Connection], connMap: Map[Computer, List[Computer]]): String = {
    val largestInterconnection: MultiConnection = {
      conns.view.map(conn => {
        var interconnected: Set[Computer] = conn.computers.toSet
        var notInterconnected: Set[Computer] = Set.empty

        val queue = collection.mutable.Queue.empty[Computer]
        queue.enqueueAll(connMap(conn.fst).intersect(connMap(conn.snd)))

        while (queue.nonEmpty) {
          val computer = queue.dequeue()
          if (!notInterconnected.contains(computer) && !interconnected.contains(computer)) {
            if (interconnected.forall(connMap(computer).contains)) {
              interconnected += computer
              queue.enqueueAll(connMap(computer))
            } else {
              notInterconnected += computer
            }
          }
        }

        MultiConnection(interconnected)
      }).maxBy(_.size)
    }

    largestInterconnection.toString
  }

  private def connectionsByComputer(conns: List[Connection]): Map[Computer, List[Computer]] = {
    conns
      .flatMap(_.computers)
      .toSet
      .map(comp =>
        comp -> conns.filter(_.computers.contains(comp)).flatMap(_.computers.filter(_ != comp)))
      .toMap
  }

  private case class Computer(id: String) {
    def maybeHistorian: Boolean = id.head == 't'

    override def toString: String = id
  }

  private case class Connection(fst: Computer, snd: Computer) {
    override def toString: String = s"$fst-$snd"

    def computers: List[Computer] = List(fst, snd)
  }

  private object Connection {
    def fromString(s: String): Connection = {
      val splits = s.trim.split("-")
      Connection(Computer(splits(0)), Computer(splits(1)))
    }
  }

  private case class MultiConnection(computers: Set[Computer]) {
    override def toString: String = computers.toList.sorted(Ordering.by(_.id)).mkString(",")

    def maybeHistorian: Boolean = computers.exists(_.maybeHistorian)

    def size: Int = computers.size
  }
}
