package day02

@main def main(part: Int, others: String*): Unit =
  val file = others match
    case Nil      => "input"
    case Seq(str) => str
  val input = parseInput(io.Source.fromFile(file).mkString)
  val result = part match
    case 1 => part1(input)
    case 2 => part2(input)
  println(result)

type Input = List[List[Int]]
type Output = Int

def parseInput(input: String): Input =
  input.linesIterator
    .map(_.split("\\s+").toList.map(_.toInt))
    .toList

def safeIncrease(x: Int) = x >= 1 && x <= 3
def safeDecrease(x: Int) = safeIncrease(-x)

def isSafe(report: List[Int]): Boolean =
  val diffs = report
    .sliding(2)
    .map:
      case Seq(x, y) => y - x
    .toList
  diffs.forall(safeIncrease) || diffs.forall(safeDecrease)

def part1(input: Input): Output =
  input.count(isSafe)

def isSafeWhenDampened(report: List[Int]): Boolean =
  def dropIndex[T](list: List[T], index: Int): List[T] =
    list.patch(index, Nil, 1)
  (0 until report.size).iterator.map(dropIndex(report, _)).exists(isSafe)

def part2(input: Input): Output =
  input.count(isSafeWhenDampened)
