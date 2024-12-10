package day10

@main def main(part: Int, others: String*): Unit =
  val file = others match
    case Nil      => "input"
    case Seq(str) => str
  val input = parseInput(io.Source.fromFile(file).mkString)
  val result = part match
    case 1 => part1(input)
    case 2 => part2(input)
  println(result)

type Input = TopographicMap
type Output = Int
type Matrix[A] = Vector[Vector[A]]

class TopographicMap(map: Vector[Vector[Int]]):
  private val bounds = (0 until map.size, 0 until map.head.size)
  def withinBounds(p: (Int, Int)) =
    bounds._1.contains(p._1) && bounds._2.contains(p._2)
  def apply(p: (Int, Int)) = map(p._1)(p._2)
  def startingPoints: Seq[(Int, Int)] =
    for
      (row, i) <- map.zipWithIndex
      (height, j) <- row.zipWithIndex
      if height == 0
    yield (i, j)
  def neighbours(position: (Int, Int)): Seq[(Int, Int)] =
    val deltas = Seq((1,0), (0,1), (-1, 0), (0, -1))
    deltas.map(position + _).filter(withinBounds)
  def nextSteps(position: (Int, Int)): Seq[(Int, Int)] =
    neighbours(position).filter(apply(_) == apply(position) + 1)
  def walk(position: (Int, Int)): Seq[(Int, Int)] = apply(position) match
    case 9 => Seq(position)
    case _ => nextSteps(position).flatMap(walk)

extension (a: (Int, Int))
  def +(b: (Int, Int)): (Int, Int) =
    (a._1 + b._1, a._2 + b._2)

def parseInput(input: String): Input =
  TopographicMap(input.split('\n').map(_.map(_.asDigit).toVector).toVector)

def part1(input: Input): Output =
  input.startingPoints.map(input.walk(_).toSet).flatten.size

def part2(input: Input): Output =
  input.startingPoints.map(input.walk).flatten.size
