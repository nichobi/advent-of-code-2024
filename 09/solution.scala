package day09

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
type Output = Long

def parseInput(input: String): Input = input.filter(_.isDigit)

def compact(input: Seq[Option[Int]]): Seq[Option[Int]] =
  val filesystem = input.toArray
  val filledBlocks = filesystem.count(_.isDefined)
  while (filesystem.take(filledBlocks).filter(_.isEmpty).nonEmpty)
    val firstEmpty = filesystem.indexOf(None)
    val lastFilled = filesystem.lastIndexWhere(_.isDefined)
    filesystem(firstEmpty) = filesystem(lastFilled)
    filesystem(lastFilled) = None
  filesystem.toSeq

def part1(input: Input): Output =
  val filesystem = (for (a, i) <- input.map(_.asDigit).zipWithIndex
  yield
    if i % 2 == 0 then Seq.fill(a)(Some(i / 2)) else Seq.fill(a)(None)).flatten
  compact(filesystem).flatten.zipWithIndex.map(_.toLong * _).sum

def fill(free: Free, file: File) = (free, file) match
  case (Free(x), File(y, _)) if x > y  => Seq(file, Free(x - y))
  case (Free(x), File(y, _)) if x == y => Seq(file)

@annotation.tailrec
def squashFrees(
    filesystem: List[BlockSection],
    acc: List[BlockSection] = List()
): List[BlockSection] = filesystem match
  case Free(x) :: Free(y) :: xs => squashFrees(Free(x + y) :: xs, acc)
  case x :: xs                  => squashFrees(xs, x :: acc)
  case Nil                      => acc.reverse

def compactIteration(
    filesystem: Seq[BlockSection],
    compactQueue: List[File]
): Seq[BlockSection] =
  compactQueue match
    case Nil => filesystem
    case next :: remaining =>
      (filesystem.zipWithIndex.find:
        case (Free(x), i) if x >= next.size && i < filesystem.indexOf(next) =>
          true
        case _ => false
      ) match
        case Some(free: Free, index) =>
          val newFilesystem = filesystem
            .updated(filesystem.indexOf(next), Free(next.size))
            .patch(index, fill(free, next), 1)
          compactIteration(squashFrees(newFilesystem.toList), remaining)
        case None => compactIteration(filesystem, remaining)
        case _    => ???

def compact2(input: Seq[BlockSection]): Seq[BlockSection] =
  compactIteration(input, input.collect { case x: File => x }.reverse.toList)
enum BlockSection(size: Int):
  case Free(size: Int) extends BlockSection(size)
  case File(size: Int, index: Int) extends BlockSection(size)
import BlockSection.{Free, File}

def part2(input: Input): Output =
  val filesystem =
    (for (a, i) <- input.map(_.asDigit).zipWithIndex
    yield if i % 2 == 0 then File(a, i / 2) else Free(a))
  (for x <- compact2(filesystem)
  yield x match
    case f: File => Seq.fill(f.size)(f.index)
    case f: Free => Seq.fill(f.size)(0)
  ).flatten.zipWithIndex.map(_.toLong * _).sum
