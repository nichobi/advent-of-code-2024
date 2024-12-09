package day09
import annotation.tailrec

@main def main(part: Int, others: String*): Unit =
  val file = others match
    case Nil      => "input"
    case Seq(str) => str
  val input = parseInput(io.Source.fromFile(file).mkString)
  val result = part match
    case 1 => part1(input)
    case 2 => part2(input)
  println(result)

type Input = Filesystem
type Output = Long

enum BlockSection:
  val size: Int
  case Free(size: Int)
  case File(size: Int, index: Int)
import BlockSection.{Free, File}

case class Filesystem(blocks: List[BlockSection]):
  def squashFrees: Filesystem =
    @tailrec
    def squash(
        filesystem: List[BlockSection],
        acc: List[BlockSection] = List()
    ): List[BlockSection] = filesystem match
      case Free(x) :: Free(y) :: xs => squash(Free(x + y) :: xs, acc)
      case x :: xs                  => squash(xs, x :: acc)
      case Nil                      => acc.reverse
    Filesystem(squash(blocks))

  def swap(fileIndex: Int, freeIndex: Int): Filesystem =
    def fill(free: Free, file: File) = (free, file) match
      case (Free(x), File(y, _)) if x > y  => Seq(file, Free(x - y))
      case (Free(x), File(y, _)) if x == y => Seq(file)
    (blocks(fileIndex), blocks(freeIndex)) match
      case (file: File, free: Free) =>
        Filesystem(
          blocks
            .updated(fileIndex, Free(file.size))
            .patch(freeIndex, fill(free, file), 1)
        ).squashFrees

  def findSwap(maxIndex: Int): Option[(Int, Int)] =
    val possibleSwaps =
      for (file, index) <- blocks
          .take(maxIndex)
          .zipWithIndex
          .reverse
          .iterator
          .collect:
            case (f: File, i) => (f, i)
      yield blocks
        .take(index)
        .zipWithIndex
        .collect:
          case (f: Free, j) if f.size >= file.size => (index, j)
        .headOption
    possibleSwaps.flatten.nextOption

  @tailrec
  final def compress(maxIndex: Int = blocks.size): Filesystem =
    findSwap(maxIndex) match
      case Some((file, free)) =>
        val newMaxIndex = // TODO: Improve this hotfix
          if blocks(file).size < blocks(free).size then file + 1
          else file
        swap(file, free).compress(maxIndex = newMaxIndex)
      case None => this

  def checksum: Long =
    (for b <- blocks
    yield b match
      case f: File => Seq.fill(f.size)(f.index)
      case f: Free => Seq.fill(f.size)(0)
    ).flatten.zipWithIndex.map(_.toLong * _).sum

  def chunkFiles =
    def chunkFile(blockSection: BlockSection) = blockSection match
      case f: File => List.fill(f.size)(File(1, f.index))
      case f: Free => List(f)
    Filesystem(blocks.flatMap(chunkFile))

def parseInput(input: String): Input =
  Filesystem(
    input
      .filter(_.isDigit)
      .map(_.asDigit)
      .zipWithIndex
      .map((a, i) => if i % 2 == 0 then File(a, i / 2) else Free(a))
      .toList
  )

def part1(input: Input): Output =
  input.chunkFiles.compress().checksum

def part2(input: Input): Output =
  input.compress().checksum
