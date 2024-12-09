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

enum BlockSection(val size: Int):
  case Free(private val s: Int) extends BlockSection(s)
  case File(private val s: Int, index: Int) extends BlockSection(s)
  def isFree = this match
    case _: Free => true
    case _       => false
  def isFile = this match
    case _: File => true
    case _       => false
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
          .filter(_._1.isFile)
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
        val newMaxIndex =
          if blocks(file).size < blocks(free).size then file + 1
          else file // TODO: Improve this hotfix
        swap(file, free).compress(maxIndex = newMaxIndex)
      case None => this

  def checksum: Long =
    println(blocks)
    (for b <- blocks
    yield b match
      case f: File => Seq.fill(f.size)(f.index)
      case f: Free => Seq.fill(f.size)(0)
    ).flatten.zipWithIndex.map(_.toLong * _).sum

  def breakBlockSections =
    def breakBlockSection(blockSection: BlockSection) = blockSection match
      case f: File => List.fill(f.size)(File(1, f.index))
      case f: Free => List.fill(f.size)(Free(1))

    Filesystem(blocks.flatMap(breakBlockSection))

def parseInput(input: String): Input =
  Filesystem(
    (for (a, i) <- input.filter(_.isDigit).map(_.asDigit).zipWithIndex
    yield if i % 2 == 0 then File(a, i / 2) else Free(a)).toList
  )

def part1(input: Input): Output =
  input.breakBlockSections.compress().checksum

def part2(input: Input): Output =
  input.compress().checksum
