package day05

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

type Rule = (Int, Int)
type Update = List[Int]
type Input = (Seq[Rule], Seq[Update])
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

def predecessors(page: Int)(using rules: Seq[Rule]): Seq[Int] =
  rules.filter(_._2 == page).map(_._1)

@tailrec
def updateIsSorted(update: Update)(using rules: Seq[Rule]): Boolean =
  update match
    case (x :: xs) =>
      predecessors(x).forall(!xs.contains(_))
      && updateIsSorted(xs)
    case Nil => true

def midpoint[A](list: Seq[A]): A =
  list(list.size / 2)

def part1(input: Input): Output =
  val (rules, updates) = input
  given Seq[Rule] = rules

  updates
    .filter(updateIsSorted)
    .map(midpoint)
    .sum

def compare(a: Int, b: Int)(using rules: Seq[Rule]) =
  predecessors(b).contains(a)

def part2(input: Input): Output =
  val (rules, updates) = input
  given Seq[Rule] = rules

  updates
    .filter(!updateIsSorted(_))
    .map(_.sortWith(compare))
    .map(midpoint)
    .sum
