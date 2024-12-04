@main def main(part: Int, others: String*): Unit =
  val file = others match
    case Nil      => "input"
    case Seq(str) => str
  val input = parseInput(io.Source.fromFile(file).mkString)
  val result = part match
    case 1 => part1(input)
    case 2 => part2(input)
  println(result)

type Matrix[A] = Seq[Seq[A]]
type Input = Matrix[Char]
type Output = Int

def parseInput(rawInput: String): Input = rawInput.split("\n").map(_.toSeq)

val xmas = "XMAS".toSeq

def countXmas(input: Seq[Char]) =
  input
    .sliding(4)
    .count(Seq(xmas, xmas.reverse).contains)

def diagonals(matrix: Matrix[Char]): Matrix[Char] =
  def nthDiagonal(matrix: Matrix[Char], n: Int): Seq[Char] =
    val size = matrix.size
    val range = if n <= size then 0 until n else (n - size) until size
    (for i <- range
    yield matrix(i)(n - i - 1))

  (1 until 2 * matrix.size).map(nthDiagonal(matrix, _))

def part1(input: Input): Output =
  Seq(
    input, // Rows
    input.transpose, // Columns
    diagonals(input), // Upwards diagonals
    diagonals(input.reverse) // Downwards diagonals
  ).flatten.map(countXmas).sum

def submatrices(matrix: Matrix[Char]): Seq[Matrix[Char]] =
  for
    i <- 0 until matrix.size - 2
    j <- 0 until matrix.size - 2
  yield matrix.slice(i, i + 3).map(_.slice(j, j + 3))

def isMasX(m: Matrix[Char]): Boolean =
  m(1)(1) == 'A'
    && Set(m(0)(0), m(2)(2)) == Set('M', 'S')
    && Set(m(2)(0), m(0)(2)) == Set('M', 'S')

def part2(input: Input): Output =
  submatrices(input).count(isMasX)
