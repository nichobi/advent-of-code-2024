@main def main(part: Int, others: String*): Unit =
  val file = others match
    case Nil      => "input"
    case Seq(str) => str
  val input = parseInput(io.Source.fromFile(file).mkString)
  val result = part match
    case 1 => part1(input)
    case 2 => part2(input)
  println(result)

type Input = String
type Output = Int

def parseInput(input: String): Input = input

def part1(input: Input): Output =
  ???

def part2(input: Input): Output =
  ???
