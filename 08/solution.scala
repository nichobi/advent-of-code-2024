import Iterator.iterate

@main def main(part: Int, others: String*): Unit =
  val file = others match
    case Nil      => "input"
    case Seq(str) => str
  val input = parseInput(io.Source.fromFile(file).mkString)
  val result = part match
    case 1 => part1(input)
    case 2 => part2(input)
  println(result)

type Input = (Seq[Seq[(Int, Int)]], Bounds)
type Output = Int
type Bounds = (Range, Range)
type AntinodeGenerator = ((Int, Int), (Int, Int)) => Seq[(Int, Int)]

def parseInput(input: String): Input =
  val lines = input.split('\n')
  val locations = for
    (line, i) <- lines.zipWithIndex
    (char, j) <- line.zipWithIndex if char != '.'
  yield (char, (i, j))
  (
    locations.toSeq.groupMap(_._1)(_._2).values.toSeq,
    (0 until lines.size, 0 until lines.head.size)
  )

extension (a: (Int, Int))
  def +(b: (Int, Int)): (Int, Int) =
    (a._1 + b._1, a._2 + b._2)
  def -(b: (Int, Int)): (Int, Int) =
    (a._1 - b._1, a._2 - b._2)

def antinodes(a: (Int, Int), b: (Int, Int)) =
  val diff = a - b
  Seq(a + diff, b - diff)

def withinBounds(position: (Int, Int))(using bounds: Bounds) =
  bounds._1.contains(position._1) && bounds._2.contains(position._2)

def findAntinodes(
    antennas: Seq[(Int, Int)]
)(using Bounds)(using antinodeGenerator: AntinodeGenerator): Seq[(Int, Int)] =
  antennas
    .combinations(2)
    .flatMap:
      case Seq(a, b) => antinodeGenerator(a, b)
    .filter(withinBounds)
    .toSeq

def solve(antennaSets: Seq[Seq[(Int, Int)]])(using AntinodeGenerator, Bounds) =
  antennaSets
    .flatMap(findAntinodes)
    .toSet
    .size

def part1(input: Input): Output =
  val (antennaSets, bounds) = input
  solve(antennaSets)(using antinodes, bounds)

def resonantAntinodes(a: (Int, Int), b: (Int, Int))(using Bounds) =
  val diff = a - b
  Seq(iterate(a)(_ + diff), iterate(b)(_ - diff))
    .flatMap(_.takeWhile(withinBounds))

def part2(input: Input): Output =
  val (antennaSets, bounds) = input
  given Bounds = bounds
  solve(antennaSets)(using resonantAntinodes)
