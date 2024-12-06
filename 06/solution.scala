import Direction.{Up, Down, Left, Right}
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

enum Direction:
  case Up, Down, Left, Right
  def delta = this match
    case Up    => (-1, 0)
    case Down  => (1, 0)
    case Right => (0, 1)
    case Left  => (0, -1)
  def turn = this match
    case Up    => Right
    case Down  => Left
    case Right => Down
    case Left  => Up

case class GuardState(coordinates: (Int, Int), direction: Direction)
case class State(
    guard: GuardState,
    map: Vector[Vector[Boolean]],
    visited: Set[(Int, Int)]
):
  def finished = !withinBounds(guard.coordinates, map)

type Input = State
type Output = Int

def parseInput(input: String): Input =
  val guardChar = input.filter("^v<>".contains).head
  val coordinates =
    (
      for
        (l, i) <- input.split('\n').zipWithIndex
        (c, j) <- l.zipWithIndex if (c == guardChar)
      yield (i, j)
    ).head
  val direction = guardChar match
    case '^' => Up
    case 'v' => Down
    case '<' => Left
    case '>' => Right
  val map = input.split('\n').map(_.map(_ == '#').toVector).toVector
  State(GuardState(coordinates, direction), map, Set(coordinates))

extension (a: (Int, Int))
  def add(b: (Int, Int)) =
    (a._1 + b._1, a._2 + b._2)
  def shift(direction: Direction): (Int, Int) =
    add(direction.delta)

def withinBounds[A](next: (Int, Int), map: Vector[Vector[A]]): Boolean =
  val size = (map.size, map.head.size)
  (0 until size._1).contains(next._1) &&
  (0 until size._2).contains(next._2)

def step(state: State): State = state match
  case State(GuardState(coordinates, direction), map, visited) =>
    val next = coordinates.shift(direction)
    if withinBounds(next, map) && map(next._1)(next._2)
    then
      state.copy(
        guard = GuardState(coordinates, direction.turn)
      )
    else
      state.copy(
        guard = GuardState(next, direction),
        visited = visited + coordinates
      )

@tailrec
def play(state: State): State =
  if state.finished
  then state
  else play(step(state))

def part1(input: Input): Output = play(input).visited.size

@tailrec
def isLooped(state: State, seenStates: Set[GuardState] = Set()): Boolean =
  if state.finished
  then false
  else if seenStates.contains(state.guard)
  then true
  else isLooped(step(state), seenStates + state.guard)

def part2(input: Input): Output =
  val visited = play(input).visited
  val map = input.map
  visited
    .map((i, j) => map.updated(i, map(i).updated(j, true)))
    .map(newMap => input.copy(map = newMap))
    .count(isLooped(_))
