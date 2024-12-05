import scala.annotation.tailrec

@main def main(part: Int, others: String*): Unit =
  val file = others match
    case Nil      => "input"
    case Seq(str) => str
  val input = parseInput(io.Source.fromFile(file).mkString)
  val result = part match
    case 1 => part1(input)
    case 2 => part2(input)
  println(result)

type Input = (Seq[(Int, Int)], Seq[List[Int]])
type Output = Int

def parseInput(input: String): Input =
  val Seq(ruleInput, updateInput) = input.split("\n\n").toSeq
  val rules =
    for (rule <- ruleInput.split("\n"))
      yield
        val Seq(a, b) = rule.split('|').map(_.toInt).toSeq
        (a, b)
  val updates =
    for (update <- updateInput.split("\n"))
      yield update.split(',').map(_.toInt).toList
  (rules, updates)

def predecessors(rules: Seq[(Int, Int)], page: Int): Seq[Int] =
  rules.filter(_._2 == page).map(_._1)

@tailrec
def updateIsSorted(rules: Seq[(Int, Int)])(update: List[Int]): Boolean =
  update match
    case (x :: xs) =>
      predecessors(rules, x).forall(!xs.contains(_))
      && updateIsSorted(rules)(xs)
    case Nil => true

def midpoint[A](list: Seq[A]): A =
  list(list.size / 2)

def part1(input: Input): Output =
  val (rules, updates) = input
  updates.filter(updateIsSorted(rules)).map(midpoint).sum

def part2(input: Input): Output =
  val (rules, updates) = input
  def compare(a: Int, b: Int) = predecessors(rules, b).contains(a)
  updates
    .filter(!updateIsSorted(rules)(_))
    .map(_.sortWith(compare))
    .map(midpoint)
    .sum
