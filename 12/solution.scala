package day12
import collection.mutable

@main def main(part: Int, others: String*): Unit =
  val file = others match
    case Nil      => "input"
    case Seq(str) => str
  val input = parseInput(io.Source.fromFile(file).mkString)
  val result = part match
    case 1 => part1(input)
    case 2 => part2(input)
  println(result)

type Input = Vector[Vector[Char]]
type Output = Int
type Memoization = mutable.Map[(Int, Int), Int]

def parseInput(input: String): Input =
  input.split('\n').map(_.toVector).toVector

extension (a: (Int, Int))
  def +(b: (Int, Int)): (Int, Int) = (a._1 + b._1, a._2 + b._2)
  def -(b: (Int, Int)): (Int, Int) = (a._1 - b._1, a._2 - b._2)
  def up = a + (-1, 0)
  def down = a + (1, 0)
  def left = a + (0, -1)
  def right = a + (0, 1)

extension [A](matrix: Vector[Vector[A]])
  def isDefinedAt(p: (Int, Int)) =
    matrix.isDefinedAt(p._1) && matrix(p._1).isDefinedAt(p._2)
  def matrixIndices: Seq[(Int, Int)] =
    for
      i <- matrix.indices
      j <- matrix.head.indices
    yield (i, j)
  def apply(pos: (Int, Int)) = matrix(pos._1)(pos._2)

  def directNeighbours(pos: (Int, Int)): Seq[(Int, Int)] =
    val deltas: Seq[((Int, Int)) => (Int, Int)] = Seq(_.up, _.down, _.left, _.right)
    val char = matrix(pos)
    deltas.map(_(pos)).filter(isDefinedAt).filter(matrix(_) == char)

  @annotation.tailrec
  def walkRegion(
      queue: List[(Int, Int)],
      acc: Set[(Int, Int)] = Set()
  ): Set[(Int, Int)] = queue match
    case Nil                              => acc
    case seen :: xs if acc.contains(seen) => walkRegion(xs, acc)
    case pos :: xs =>
      walkRegion(xs ++ directNeighbours(pos), acc + pos)

  def regionCost(pos: (Int, Int), fences: ((Int, Int)) => Int)(using
      regionSizeCache: mutable.Map[(Int, Int), Int]
  ): Int =
    val regionSize =
      if regionSizeCache.contains(pos)
      then regionSizeCache(pos)
      else
        val region = walkRegion(List(pos))
        regionSizeCache ++= region.map(_ -> region.size)
        region.size
    fences(pos) * regionSize

def part1(input: Input): Output =
  given mutable.Map[(Int, Int), Int] = mutable.Map()
  def fences(pos: (Int, Int)): Int =
    4 - input.directNeighbours(pos).size
  input.matrixIndices.map(p => input.regionCost(p, fences)).sum

def part2(input: Input): Output =
  given mutable.Map[(Int, Int), Int] = mutable.Map()
  def fences(pos: (Int, Int)): Int =
    def isOurFence(pos: (Int, Int), delta: (Int, Int)): Boolean = delta match
      case (-1, 0) =>
        (!input.isDefinedAt(pos.left) || !input.isDefinedAt(pos.left.up)) ||
        input(pos.left) != input(pos) ||
        input(pos.left.up)   == input(pos)
      case (1, 0) =>
        (!input.isDefinedAt(pos.left) || !input.isDefinedAt(pos.left.down)) ||
        input(pos.left) != input(pos)  ||
        input(pos.left.down) == input(pos)
      case (0, 1) => 
        (!input.isDefinedAt(pos.up) || !input.isDefinedAt(pos.up.right)) ||
        input(pos.up)   != input(pos) ||
        input(pos.up.right)  == input(pos)
      case (0, -1) =>
        (!input.isDefinedAt(pos.up) || !input.isDefinedAt(pos.up.left)) ||
        input(pos.up)   != input(pos) ||
        input(pos.up.left)   == input(pos)
    def freeSides(pos: (Int, Int)): Seq[(Int, Int)] =
      input.directNeighbours(pos).map(_ - pos).filter(isOurFence(pos, _))
    4 - freeSides(pos).size
  input.matrixIndices.map(p => input.regionCost(p, fences)).sum
