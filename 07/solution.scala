@main def main(part: Long, others: String*): Unit =
  val file = others match
    case Nil      => "input"
    case Seq(str) => str
  val input = parseInput(io.Source.fromFile(file).mkString)
  val result = part match
    case 1 => part1(input)
    case 2 => part2(input)
  println(result)

type Input = Seq[(Long, List[Long])]
type Output = Long
type Operator = (Long, Long) => Long

def parseInput(input: String): Input =
  input
    .split('\n')
    .map(line =>
      val Array(testValue, rest) = line.split(": ")
      (testValue.toLong, rest.split(' ').map(_.toLong).toList)
    )
    .toSeq

def calculate(numbers: List[Long])(using operators: Seq[Operator]): Seq[Long] =
  numbers match
    case x :: y :: xs =>
      operators.flatMap(op => calculate(op(x, y) :: xs))
    case x :: Nil => Seq(x)
    case Nil      => ???

def solve(input: Input)(using operators: Seq[Operator]) =
  input
    .filter((testValue, numbers) => calculate(numbers).contains(testValue))
    .map(_._1)
    .sum

def part1(input: Input): Output =
  given Seq[Operator] = Seq(_ + _, _ * _)
  solve(input)

def part2(input: Input): Output =
  given Seq[Operator] = Seq(_ + _, _ * _, (a, b) => s"$a$b".toLong)
  solve(input)
