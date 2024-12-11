package day11
import collection.mutable.Map as MutMap

@main def main(part: Int, others: String*): Unit =
  val file = others match
    case Nil      => "input"
    case Seq(str) => str
  val input = parseInput(io.Source.fromFile(file).mkString)
  val result = part match
    case 1 => part1(input)
    case 2 => part2(input)
  println(result)

type Input = List[Long]
type Output = Long
type MemoMap = MutMap[(Int, Long), Long]

def parseInput(input: String): Input =
  input.filter(_ != '\n').split(' ').map(_.toLong).toList

def evenDigits(stone: Long) = stone.toString.length % 2 == 0
def blink(stone: Long): Seq[Long] = stone match
  case 0 => Seq(1)
  case n if evenDigits(n) =>
    val nStr = n.toString
    nStr.splitAt(nStr.length / 2).toList.map(_.toLong)
  case n => Seq(n * 2024)

def part1(input: Input): Output =
  Iterator.iterate(input)(_.flatMap(blink)).drop(25).next.size

def recurse(depth: Int, stone: Long)(using memo: MemoMap): Long =
  (depth, stone) match
    case (75, _) => 1
    case x =>
      memo.getOrElseUpdate(x, blink(stone).map(depth + 1 -> _).map(recurse).sum)

def part2(input: Input): Output =
  given MemoMap = MutMap()
  input.map(recurse(0, _)).sum
