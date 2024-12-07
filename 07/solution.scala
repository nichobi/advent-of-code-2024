@main def main(part: Long, others: String*): Unit =
  val file = others match
    case Nil      => "input"
    case Seq(str) => str
  val input = parseInput(io.Source.fromFile(file).mkString)
  val result = part match
    case 1 => part1(input)
    case 2 => part2(input)
  println(result)

type Input = Seq[(Long, Seq[Long])]
type Output = Long

def parseInput(input: String): Input =
  input
    .split('\n')
    .map(line =>
      val Array(testValue, rest) = line.split(": ")
      (testValue.toLong, rest.split(' ').map(_.toLong).toSeq)
    )
    .toSeq

def possibleResults(numbers: List[Long]): Set[Long] = numbers match
  case x :: Nil => Set(x)
  case x :: xs =>
    possibleResults(xs).map(x + _) ++ possibleResults(xs).map(x * _)

def part1(input: Input): Output =
  input
    .filter((testValue, numbers) =>
      possibleResults(numbers.toList.reverse).contains(testValue)
    )
    .map(_._1)
    .sum

def concat(a: Long, b: Long): Long =
  s"$b$a".toLong
def possibleResults2(numbers: List[Long]): Set[Long] =
  val operators =
    Seq((x: Long, y: Long) => x + y, (x: Long, y: Long) => x * y, concat)
  numbers match
    case x :: Nil => Set(x)
    case x :: xs =>
      operators.flatMap(op => possibleResults2(xs).map(y => op(x, y))).toSet

def part2(input: Input): Output =
  input
    .filter((testValue, numbers) =>
      possibleResults2(numbers.toList.reverse).contains(testValue)
    )
    .map(_._1)
    .sum
