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
  """mul\((\d{1,3}),(\d{1,3})\)""".r
    .findAllMatchIn(input)
    .map(m => m.group(1).toInt * m.group(2).toInt)
    .sum

def part2(input: Input): Output =
  part1(
    input
      .replaceAll("""(?s)don't\(\).*?(do\(\)|$)""", "")
  )
