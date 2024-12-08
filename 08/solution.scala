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
  def add(b: (Int, Int)) =
    (a._1 + b._1, a._2 + b._2)
  def sub(b: (Int, Int)): (Int, Int) =
    add(-b._1, -b._2)

def antinodesOfPair(a: (Int, Int), b: (Int, Int)) =
  val diff = a.sub(b)
  Seq(a.add(diff), b.sub(diff))

def withinBounds(position: (Int, Int))(using bounds: Bounds) =
  bounds._1.contains(position._1) && bounds._2.contains(position._2)

def findAntinodes(
    antennas: Seq[(Int, Int)]
)(using Bounds)(using antinodeGenerator: AntinodeGenerator): Set[(Int, Int)] =
  antennas
    .combinations(2)
    .map:
      case Seq(a, b) => antinodeGenerator(a, b)
    .flatten
    .toSet
    .filter(withinBounds)

def solve(input: Input)(using AntinodeGenerator) =
  val (antennas, bounds) = input
  given Bounds = bounds
  antennas
    .map(findAntinodes)
    .flatten
    .toSet
    .size

def part1(input: Input): Output =
  solve(input)(using antinodesOfPair)

def antinodesOfPair2(a: (Int, Int), b: (Int, Int))(using Bounds) =
  val diff = a.sub(b)
  (
    Iterator.iterate(a)(_.add(diff)).takeWhile(withinBounds) ++
      Iterator.iterate(b)(_.sub(diff)).takeWhile(withinBounds)
  ).toSeq

def part2(input: Input): Output =
  given Bounds = input._2
  solve(input)(using antinodesOfPair2)
