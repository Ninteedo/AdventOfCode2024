package days

import utility.*

import java.nio.file.{Files, Paths}
import scala.util.Random

class Day24 extends IDay {
  private type Env = Map[String, Wire]

  override def execute(input: String): (Any, Any) = {
    val env: Env = input
      .split("\n")
      .filter(_.nonEmpty)
      .map(Wire.fromString)
      .toMap
    (part1(env), part2(env))
  }

  private def part1(env: Env) = evaluateCircuit(env, "z")

  private def evaluateCircuit(env: Env, outputPrefix: String) = {
    val binary = env
      .keys
      .filter(_.startsWith(outputPrefix))
      .toList
      .sorted
      .map(env(_).doEval(env))
      .map(b => if b then 1 else 0)
      .reverse
      .mkString
    BigInt(binary, 2)
  }

  private def part2(originalEnv: Env) = {
    Files.write(Paths.get("chart.txt"), generateMermaidFlowchart(originalEnv).getBytes())

    val x = evaluateCircuit(originalEnv, "x")
    val y = evaluateCircuit(originalEnv, "y")

    val z = evaluateCircuit(originalEnv, "z")

    val incorrect = (x + y) ^ z

    def listIncorrectBinaryIndices(diff: BigInt): List[Int] = {
      diff.toString(2).reverse.zipWithIndex.filter(_._1 == '1').map(_._2).toList
    }

    println(listIncorrectBinaryIndices(incorrect))

    def swapOutputs(env: Env, wire1: String, wire2: String): Env = {
      var newEnv = env
      val swap = newEnv(wire1)
      newEnv = newEnv.updated(wire1, newEnv(wire2))
      newEnv = newEnv.updated(wire2, swap)
      newEnv
    }

    val pairs: List[(String, String)] = List(
      ("z07", "shj"), ("z27", "kcd"), ("wkb", "tpk"), ("z23", "pfn")
    ) // ("z27", "vpt"), ("z07", "shj"), ("tsb", "dhf"), ("wkb", "tpk"))

    val newEnv = pairs.foldLeft(originalEnv)((env, pair) => swapOutputs(env, pair._1, pair._2))

    println(testAddition(newEnv, x, y))

    def randomBinary(n: Int): BigInt = (0 until n).map(i => Random.nextInt(2)).foldLeft(0L)((acc, b) => (acc << 1) | b)

    for (i <- 0 until 32) {
      val x = randomBinary(44)
      val y = randomBinary(44)
      val z = x + y

      println(s"Testing $x + $y = $z")
      println(listIncorrectBinaryIndices(testAddition(newEnv, x, y)))
    }

    pairs.flatMap(p => List(p._1, p._2)).sorted.mkString(",")
  }

  private def testAddition(env: Env, x: BigInt, y: BigInt): BigInt = {
    val xBinary = x.toString(2).reverse
    val yBinary = y.toString(2).reverse

    val xWires = xBinary.zipWithIndex.map((b, i) => f"x$i%02d" -> ConstantWire(b == '1')).toMap
    val yWires = yBinary.zipWithIndex.map((b, i) => f"y$i%02d" -> ConstantWire(b == '1')).toMap

    val newEnv = env ++ xWires ++ yWires

    evaluateCircuit(newEnv, "z") ^ (x + y)
  }

  private def generateMermaidFlowchart(env: Env): String = {
    // mmdc -i chart.txt -o graph.png -c mermaidConfig.json --scale 5

    val sb = new StringBuilder

    sb.append("flowchart TD\n")

    val wireIds = env.keys.toList.sorted.zipWithIndex.toMap

    def addWire(wireName: String, wire: Wire): Unit = wire match {
      case ConstantWire(const) => sb.append(s"  $wireName(($wireName))\n")
      case AndWire(left, right) =>
        sb.append(s"  ${left} -->  ${wireIds(wireName)}[AND]\n")
        sb.append(s"  ${right} -->  ${wireIds(wireName)}\n")
        sb.append(s"  ${wireIds(wireName)} -->  $wireName{$wireName}\n")
      case OrWire(left, right) =>
        sb.append(s"  ${left} -->  ${wireIds(wireName)}[OR]\n")
        sb.append(s"  ${right} -->  ${wireIds(wireName)}\n")
        sb.append(s"  ${wireIds(wireName)} -->  $wireName{$wireName}\n")
      case XorWire(left, right) =>
        sb.append(s"  ${left} -->  ${wireIds(wireName)}[XOR]\n")
        sb.append(s"  ${right} -->  ${wireIds(wireName)}\n")
        sb.append(s"  ${wireIds(wireName)} -->  $wireName{$wireName}\n")
    }

    env.foreach((k, v) => addWire(k, v))

    sb.toString()
  }

  private trait Wire {
    private val evalCache: collection.mutable.Map[Env, Boolean] = collection.mutable.Map.empty

    def doEval(env: Env): Boolean = evalCache.getOrElseUpdate(env, evalInner(env))

    def involved(env: Env): Set[String]

    protected def evalInner(env: Env): Boolean
  }

  private case class ConstantWire(const: Boolean) extends Wire {
    override def involved(env: Env): Set[String] = Set.empty

    override protected def evalInner(env: Env): Boolean = const
  }

  private case class AndWire(left: String, right: String) extends Wire {
    override def involved(env: Env): Set[String] = env(left).involved(env) ++ env(right).involved(env) + left + right

    override protected def evalInner(env: Env): Boolean = env(left).doEval(env) && env(right).doEval(env)
  }

  private case class OrWire(left: String, right: String) extends Wire {
    override def involved(env: Env): Set[String] = env(left).involved(env) ++ env(right).involved(env) + left + right

    override protected def evalInner(env: Env): Boolean = env(left).doEval(env) || env(right).doEval(env)
  }

  private case class XorWire(left: String, right: String) extends Wire {
    override def involved(env: Env): Set[String] = env(left).involved(env) ++ env(right).involved(env) + left + right

    override protected def evalInner(env: Env): Boolean = env(left).doEval(env) ^ env(right).doEval(env)
  }

  private object Wire {
    private val pattern1 = "(\\w+): (\\d)".r
    private val pattern2 = "(\\w+) (\\w+) (\\w+) -> (\\w+)".r

    def fromString(line: String): (String, Wire) = {
      pattern1.findFirstMatchIn(line) match {
        case Some(m) => (m.group(1), ConstantWire(m.group(2).toInt == 1))
        case None =>
          pattern2.findFirstMatchIn(line) match {
            case Some(m) => m.group(2) match {
              case "AND" => (m.group(4), AndWire(m.group(1), m.group(3)))
              case "OR" => (m.group(4), OrWire(m.group(1), m.group(3)))
              case "XOR" => (m.group(4), XorWire(m.group(1), m.group(3)))
            }
            case None => throw new Exception("Invalid input for wire: " + line)
          }
      }
    }
  }
}

// SWAPS: z27-vpt, z07-shj, tsb-dhf
// dhf,shj,tpk,tsb,vpt,wkb,z07,z27  -- wrong
