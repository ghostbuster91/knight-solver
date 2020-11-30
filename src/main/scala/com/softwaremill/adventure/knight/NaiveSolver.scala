package com.softwaremill.adventure.knight

import com.softwaremill.adventure.knight.NaiveSolver.{Modifiers, Position}

import scala.annotation.tailrec

class NaiveSolver(n: Int, m: Int) {
  private var counter = 0L
  private var lastScore = 0L

  def solve(startingPosition: Position): Unit = {
    println(s"Starting position: $startingPosition")
    val path = go(List.empty, VisitedCell(startingPosition))
    println(s"$counter ${path.size} $path")
    println(s"Starting position: $startingPosition")
    printBoard(path, n, m)
  }

  @tailrec
  private def go(
      visited: List[VisitedCell],
      currentlyVisited: VisitedCell
  ): List[Position] = {
    counter += 1
    printProgress(visited.map(_.position))
    if (visited.size + 1 == m * n) {
      (visited :+ currentlyVisited).map(_.position)
    } else {
      val nextMoves =
        getPossibleMoves(
          visited.map(_.position),
          currentlyVisited
        )
      nextMoves match {
        case head :: _ =>
          go(visited :+ currentlyVisited, VisitedCell(head, List.empty))
        case Nil =>
          val previousPosition =
            visited.last.copy(restricted =
              visited.last.restricted :+ currentlyVisited.position
            )
          go(visited.dropRight(1), previousPosition)
      }

    }
  }

  private def getPossibleMoves(
      visited: List[Position],
      currentlyVisited: VisitedCell
  ): List[Position] = {
    val possibleMoves = getPossibleMovesSimple(
      visited,
      currentlyVisited.position,
      currentlyVisited.restricted
    )
    val movesWithOutputs = possibleMoves.map(move =>
      move -> getPossibleMovesSimple(
        visited :+ currentlyVisited.position,
        move,
        currentlyVisited.restricted
      ).size
    )
    sortMovesByOutputsAsc(movesWithOutputs)
  }

  private def sortMovesByOutputsAsc(
      movesWithOutputs: List[((Int, Int), Int)]
  ) = {
    movesWithOutputs
      .sortBy(_._2)
      .map(_._1)
  }

  private def getPossibleMovesSimple(
      visited: List[Position],
      position: Position,
      restricted: List[Position]
  ): List[Position] = {
    Modifiers
      .map(m => (position._1 + m._1, position._2 + m._2))
      .filter {
        case (i, j) if i < 0 || j < 0   => false
        case (i, j) if i >= n || j >= m => false
        case _                          => true
      }
      .filterNot(visited.contains)
      .filterNot(restricted.contains)
  }

  private def printProgress(
      visited: List[Position]
  ): Unit = {
    if (lastScore < visited.size) {
      lastScore = visited.size
      println(s"counter: $counter")
      println(s"visited: ${visited.size}")
      printBoard(visited, n, m)
    }
  }

  private def printBoard(visited: List[Position], n: Int, m: Int): Unit = {
    println("============================")
    (0 until n).foreach { ni =>
      (0 until m).foreach { mi =>
        if (visited.contains(ni -> mi)) {
          print("x|")
        } else {
          print("o|")
        }
      }
      println("")
    }
  }
}

object NaiveSolver {

  type Position = (Int, Int)

  private val Modifiers = List(
    (-2, -1),
    (-1, -2),
    (-2, 1),
    (-1, 2),
    (1, -2),
    (2, -1),
    (1, 2),
    (2, 1)
  )

  def main(args: Array[String]): Unit = {
    new NaiveSolver(8, 8).solve(2, 0)
  }

}

case class VisitedCell(
    position: Position,
    restricted: List[Position] = List.empty
)
