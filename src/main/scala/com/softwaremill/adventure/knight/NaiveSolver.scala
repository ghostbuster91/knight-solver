package com.softwaremill.adventure.knight

import java.util.concurrent.Executors

import monix.eval.Task
import monix.execution.Scheduler
import monix.execution.atomic.{AtomicInt, AtomicLong}

import scala.collection.mutable
import scala.util.Random

object NaiveSolver {

  type Position = (Int, Int)

  val random = new Random()
  val counter = AtomicLong(0L)
  val lastScore = AtomicInt(0)
  def main(args: Array[String]): Unit = {
    solve(8, 8)
  }

  def solve(n: Int, m: Int): Unit = {

    val startX = random.nextInt(n)
    val startY = random.nextInt(m)
    //0,2
    val currentMove = startX -> startY
    println(s"Starting position: $currentMove")
    implicit val schedulerService = Scheduler.computation()
    val path = go(n, m, List.empty, currentMove -> List.empty).runSyncUnsafe()
    println(s"$counter ${path.size} ${path}")
    println(s"Starting position: $currentMove")
    printV(path, n, m)
  }

  private def go(
      n: Int,
      m: Int,
      visited: List[(Position, List[Position])],
      currentPosition: (Position, List[Position])
  ): Task[List[Position]] = {
    val c = counter.incrementAndGet()
    if (lastScore.get() < visited.size) {
      lastScore.set(visited.size)
      println(c)
      println(visited.size)
      printV(visited.map(_._1), n, m)
    }
    if (counter.get() % 100_000 == 0) {
      println(counter.get())
    }
    if (visited.size + 1 == m * n) {
      Task.pure((visited :+ currentPosition).map(_._1))
    } else {
      val nextMoves =
        getPossibleMoves(
          visited.map(_._1),
          currentPosition._1,
          n,
          m,
          currentPosition._2
        )
      nextMoves match {
        case nm if nm.nonEmpty =>
          go(n, m, visited :+ currentPosition, nm.head -> List.empty)
        case Nil =>
          val nextPosition =
            visited.last.copy(_2 = visited.last._2 :+ currentPosition._1)
          go(
            n,
            m,
            visited.dropRight(1),
            nextPosition
          )
      }

    }
  }

  def getPossibleMoves(
      visited: List[Position],
      position: Position,
      n: Int,
      m: Int,
      restricted: List[Position]
  ): List[Position] = {
    getPossibleMovesSimple(visited, position, n, m, restricted)
      .map(move =>
        move -> getPossibleMovesSimple(
          visited :+ position,
          move,
          n,
          m,
          restricted
        ).size
      )
      .sortBy(_._2)
      .map(_._1)
  }

  def getPossibleMovesSimple(
      visited: List[Position],
      position: Position,
      n: Int,
      m: Int,
      restricted: List[Position]
  ): List[Position] = {
    val modifiers = List(
      (-2, -1),
      (-1, -2),
      (-2, 1),
      (-1, 2),
      (1, -2),
      (2, -1),
      (1, 2),
      (2, 1)
    )

    modifiers
      .map(m => (position._1 + m._1, position._2 + m._2))
      .filter {
        case (i, j) if i < 0 || j < 0   => false
        case (i, j) if i >= n || j >= m => false
        case _                          => true
      }
      .filterNot(visited.contains)
      .filterNot(restricted.contains)
  }

  def printV(visited: List[Position], n: Int, m: Int) = {
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
//    0  1  2  3  4
// 0 |x|  |
// 1
// 2     x
// 3
// 4
