@main def main(part: Int) =
  val input = io.Source.fromFile("input").mkString
  val result = part match
    case 1 => part1(input)
    case 2 => part2(input)
  println(result)

def parseInputLists(input: String): (List[Int], List[Int]) =
  def lineToPair(line: String): (Int, Int) =
    val pair = line.split("\\s+")
    (pair(0).toInt, pair(1).toInt)
  input
    .linesIterator
    .map(lineToPair)
    .toList
    .unzip

def part1(input: String): String =
  val (lefts, rights) = parseInputLists(input)
  lefts.sorted
    .zip(rights.sorted)
    .map{case (x, y) => (x - y).abs}
    .sum
    .toString

def part2(input: String): String =
  val (lefts, rights) = parseInputLists(input)
  lefts
    .map(n => rights.count(_ == n) * n)
    .sum
    .toString

