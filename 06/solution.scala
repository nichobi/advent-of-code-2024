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
import Direction.{Up, Down, Left, Right}

case class GuardState(position: (Int, Int), direction: Direction)

case class State(
    guard: GuardState,
    map: Vector[Vector[Boolean]],
    visited: Set[(Int, Int)]
):
  def finished = !withinBounds(guard.position)
  val bounds = (0 until map.size, 0 until map.head.size)
  def withinBounds(position: (Int, Int)) =
    bounds._1.contains(position._1) && bounds._2.contains(position._2)
type Input = State
type Output = Int

def parseInput(input: String): Input =
  val guardChar = input.filter("^v<>".contains).head
  val Array(position) =
    for
      (l, i) <- input.split('\n').zipWithIndex
      (c, j) <- l.zipWithIndex if (c == guardChar)
    yield (i, j)
  val direction = guardChar match
    case '^' => Up
    case 'v' => Down
    case '<' => Left
    case '>' => Right
  val map = input.split('\n').map(_.map(_ == '#').toVector).toVector
  State(GuardState(position, direction), map, Set(position))

extension (a: (Int, Int))
  def add(b: (Int, Int)) =
    (a._1 + b._1, a._2 + b._2)
  def shift(direction: Direction): (Int, Int) =
    add(direction.delta)

def step(state: State): State = state match
  case State(GuardState(position, direction), map, visited) =>
    val next = position.shift(direction)
    if state.withinBounds(next) && map(next._1)(next._2)
    then
      state.copy(
        guard = GuardState(position, direction.turn)
      )
    else
      state.copy(
        guard = GuardState(next, direction),
        visited = visited + position
      )

@tailrec
def walk(state: State): State =
  if state.finished
  then state
  else walk(step(state))

def part1(input: Input): Output = walk(input).visited.size

@tailrec
def isLooped(state: State, seenStates: Set[GuardState] = Set()): Boolean =
  if state.finished
  then false
  else if seenStates.contains(state.guard)
  then true
  else isLooped(step(state), seenStates + state.guard)

def part2(input: Input): Output =
  val visited = walk(input).visited
  val map = input.map
  visited
    .map((i, j) => map.updated(i, map(i).updated(j, true)))
    .map(newMap => input.copy(map = newMap))
    .count(isLooped(_))
