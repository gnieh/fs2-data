package fs2.data.pattern

import cats.syntax.all._

import scala.annotation.tailrec

sealed trait Heuristic[Tag] {

  def execute[Pat, Out](matrix: Matrix[Tag, Pat, Out]): List[Int] =
    this match {
      case Heuristic.Score(score) =>
        val VMatrix(columns, _) = matrix.verticalView
        val scores = columns.map(_.patterns).zipWithIndex.map { case (skels, idx) =>
          score(matrix.map(_.patterns), idx, skels)
        }
        val maxIndices = scores.zipWithIndex.groupBy(_._1).toList.sortBy(-_._1).map(_._2)
        maxIndices match {
          case idcs :: _ => idcs.map(_._2)
          case Nil       => Nil
        }
      case Heuristic.Combine(h1, h2) =>
        h1.execute(matrix) match {
          case indicesH1 @ (_ :: _ :: _) =>
            val indexMap = indicesH1.zipWithIndex.foldRight(Map.empty[Int, Int]) { case ((oidx, nidx), acc) =>
              acc.updated(nidx, oidx)
            }
            val vm @ VMatrix(columns, _) = matrix.verticalView
            val filtCols = indicesH1.flatMap(columns.lift(_))
            val filtMatrix = vm.copy(columns = filtCols).horizontalView
            val indicesH2 = h2.execute(filtMatrix)
            indicesH2.flatMap(indexMap.get(_))
          case indicesH1 =>
            indicesH1
        }
    }

}
object Heuristic {
  private case class Score[Tag](f: (List[List[Skeleton[Tag]]], Int, List[Skeleton[Tag]]) => Int) extends Heuristic[Tag]
  private case class Combine[Tag, Out](h1: Heuristic[Tag], h2: Heuristic[Tag]) extends Heuristic[Tag]

  /** Leaves the columns in the same order. */
  def none[Tag]: Heuristic[Tag] =
    Score((_, idx, _) => -idx)

  /** Lifts a score function into a heuristic. */
  def lift[Tag](f: (List[List[Skeleton[Tag]]], Int, List[Skeleton[Tag]]) => Int): Heuristic[Tag] =
    Score(f)

  /** Favours columns whose top apttern is a generalized constructor pattern. */
  def firstRow[Tag]: Heuristic[Tag] = {
    @tailrec
    def score(skels: List[Skeleton[Tag]]): Int =
      skels match {
        case Skeleton.Wildcard(_) :: _       => 0
        case Skeleton.Constructor(_, _) :: _ => 1
        case Skeleton.As(p, _) :: ps         => score(p :: ps)
        case Nil                             => 1
      }
    Score((_, _, col) => score(col))
  }

  /** Favours columns with the least number of wildcards. */
  def smallDefault[Tag]: Heuristic[Tag] = {
    @tailrec
    def score(skel: Skeleton[Tag]): Int =
      skel match {
        case Skeleton.Wildcard(_)       => -1
        case Skeleton.Constructor(_, _) => 0
        case Skeleton.As(skel, _)       => score(skel)
      }
    Score((_, _, col) => col.foldMap(score(_)))
  }

  /** Combines a list of heuristics, from left to right, defaulting to no heuristics. */
  def sequence[Tag](hs: List[Heuristic[Tag]]): Heuristic[Tag] =
    hs.reduceRightOption(Combine(_, _)).getOrElse(none)

}
