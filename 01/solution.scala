@main def main(part: Int, others: String*): Unit =
  val file = others match
    case Nil      => "input"
    case Seq(str) => str
  val input = parseInput(io.Source.fromFile(file).mkString)
  val result = part match
    case 1 => part1(input)
    case 2 => part2(input)
  println(result)

type Input = (List[Int], List[Int])
type Output = Int

def parseInput(input: String): Input =
  def lineToPair(line: String): (Int, Int) =
    val pair = line.split("\\s+")
    (pair(0).toInt, pair(1).toInt)
  input.linesIterator
    .map(lineToPair)
    .toList
    .unzip

def part1(input: Input): Output =
  val (lefts, rights) = input
  lefts.sorted
    .zip(rights.sorted)
    .map:
      case (x, y) => (x - y).abs
    .sum

def part2(input: Input): Output =
  val (lefts, rights) = input
  lefts
    .map(n => rights.count(_ == n) * n)
    .sum
