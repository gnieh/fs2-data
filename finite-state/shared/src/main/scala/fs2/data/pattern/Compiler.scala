/*
 * Copyright 2022 Lucas Satabin
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package fs2.data.pattern

import cats.{Defer, MonadError}
import cats.syntax.all._

class PatternException(msg: String) extends Exception(msg)

class Compiler[F[_], Tag, Pat, Out](heuristic: Heuristic[Tag])(implicit F: MonadError[F, Throwable], defer: Defer[F]) {

  /** Compiles a pattern into a decision tree. */
  def compile(
      cases: List[(Pat, Out)])(implicit Tag: IsTag[Tag], Pat: IsPattern[Pat, Tag]): F[DecisionTree[Tag, Out]] = {
    val matrix = cases.flatMap { case (pat, out) =>
      Pat.decompose(pat).map(skel => Row[Tag, Pat, Out](pat, Nil, List(skel), out))
    }
    compileMatrix(heuristic, List(Selector.Root), matrix)
  }

  private def compileMatrix(heuristic: Heuristic[Tag], occurrences: List[Selector[Tag]], matrix: Matrix[Tag, Pat, Out])(
      implicit Tag: IsTag[Tag]): F[DecisionTree[Tag, Out]] =
    matrix match {
      case Nil => F.pure(DecisionTree.Fail())
      case (row @ Row(_, bindings, pats, out)) :: _ =>
        if (row.isWidcard) {
          // all the patterns in the row are wildcards on the top row
          // this means this always succeeds, and the other rows are
          // redundant
          def bindingsIn(occ: Selector[Tag], skel: Skeleton[Tag]): F[List[Binding[Selector[Tag]]]] =
            skel match {
              case Skeleton.Constructor(_, _) =>
                F.raiseError(new PatternException("A wildcard row cannot contain a constructor skeleton"))
              case Skeleton.Wildcard(id) => id.map(_ -> occ).toList.pure
              case Skeleton.As(p, id)    => bindingsIn(occ, p).map((id -> occ) :: _)
            }
          occurrences
            .zip(pats)
            .flatTraverse { case (occ, skel) => bindingsIn(occ, skel) }
            .map(bds => DecisionTree.Leaf((bds ++ bindings).toMap, out))
        } else {
          // some patterns in the row are not wildcards
          // use the heuristic to reorder the columns according
          // to their score
          heuristic
            .execute(matrix)
            .headOption
            .liftTo[F](new PatternException("Occurrences cannot be empty"))
            .flatMap { maxScoreIndex =>
              (swapFront(maxScoreIndex, occurrences), swapColumn(maxScoreIndex, matrix)).tupled
                .flatMap { case (shuffledOccs, shuffledMatrix) =>
                  shuffledOccs.headOption.liftTo[F](new PatternException("Occurrences cannot be empty")).flatMap {
                    headOcc =>
                      matchFirstColumn(headOcc, shuffledMatrix).flatMap { case (specializedMatrices, defaultMatrix) =>
                        def makeBranch(subOccs: List[Selector[Tag]], subp: SubProblem) =
                          compileMatrix(heuristic, subOccs ++ shuffledOccs.tail, subp.subMatrix)
                        def branches =
                          specializedMatrices.toList
                            .traverse { case (k, v) =>
                              makeBranch(v._1, v._2).map(k -> _)
                            }
                            .map(_.toMap)
                        def defaultBranch =
                          defaultMatrix.traverse(makeBranch(Nil, _))
                        (branches, defaultBranch).mapN { (branches, defaultBranch) =>
                          DecisionTree.Switch(headOcc, branches, defaultBranch)
                        }
                      }
                  }
                }
            }
        }
    }

  private def swapFront[T](n: Int, ts: List[T]): F[List[T]] =
    if (n < 0) {
      F.raiseError(new PatternException("The index selected by the heuristic cannot be negative"))
    } else {
      def go(n: Int, ts: List[T]): F[(T, List[T])] =
        (n, ts) match {
          case (_, Nil)     => F.raiseError(new PatternException("Trying to swap a column past the end of list"))
          case (0, t :: ts) => (t, ts).pure[F]
          case (_, t :: ts) => go(n - 1, ts).map { case (t1, ts1) => (t1, t :: ts1) }
        }
      go(n, ts).map { case (t1, l1) => t1 :: l1 }
    }

  private def swapColumn(idx: Int, matrix: Matrix[Tag, Pat, Out]): F[Matrix[Tag, Pat, Out]] = {
    val vmatrix @ VMatrix(columns, _) = matrix.verticalView
    swapFront(idx, columns).map { columns =>
      vmatrix.copy(columns = columns).horizontalView
    }
  }

  private case class SubProblem(subMatrix: Matrix[Tag, Pat, Out])

  private def matchFirstColumn(expr: Selector[Tag], matrix: Matrix[Tag, Pat, Out])(implicit
      Tag: IsTag[Tag]): F[(Map[Tag, (List[Selector[Tag]], SubProblem)], Option[SubProblem])] =
    matrix match {
      case Row(_, _, skel :: _, _) :: _ =>
        def go(
            cons: Skeleton.Constructor[Tag],
            matrices: Map[Tag, (List[Selector[Tag]], SubProblem)]): F[Map[Tag, (List[Selector[Tag]], SubProblem)]] = {
          specialize(expr, cons, matrix).map { smat =>
            val soccs = select(cons, expr)
            matrices.updated(cons.tag, (soccs, SubProblem(smat)))
          }
        }
        val specializedMatrices: F[Map[Tag, (List[Selector[Tag]], SubProblem)]] =
          matrix.verticalView.columns.headOption
            .liftTo[F](new PatternException("the pattern matrix cannot be empty"))
            .map(_.patterns)
            .flatMap(headConstructors(_).foldRightDefer(F.pure(Map.empty[Tag, (List[Selector[Tag]], SubProblem)])) {
              (cons, acc) => acc.flatMap(go(cons, _))
            })
        specializedMatrices.flatMap { specializedMatrices =>
          val matchedTags = specializedMatrices.keySet
          val defaultMat =
            if (Tag.hasUnmatched(skel, matchedTags)) {
              defaultMatrix(expr, matrix).map { dmatrix =>
                SubProblem(dmatrix).some
              }
            } else {
              F.pure(none)
            }
          defaultMat.map((specializedMatrices, _))
        }
      case _ =>
        F.pure((Map.empty, none))
    }

  private def select(const: Skeleton.Constructor[Tag], expr: Selector[Tag]): List[Selector[Tag]] =
    const.args.zipWithIndex.map { case (_, idx) => Selector.Sel(expr, const.tag, idx) }

  private def specialize(expr: Selector[Tag], const: Skeleton.Constructor[Tag], matrix: Matrix[Tag, Pat, Out])(implicit
      Tag: IsTag[Tag]): F[Matrix[Tag, Pat, Out]] = matrix match {
    case Row(_, _, Nil, _) :: _ =>
      F.pure(matrix)
    case _ =>
      def go(row: Row[Tag, Pat, Out]): F[Option[Row[Tag, Pat, Out]]] =
        row match {
          case Row(pat, bindings, p :: ps, out) =>
            p match {
              case Skeleton.Constructor(consTag, subps) =>
                if (consTag === const.tag)
                  Row(pat, bindings, subps ++ ps, out).some.pure[F]
                else
                  none.pure[F]
              case Skeleton.Wildcard(id) =>
                Row(pat,
                    id.fold(bindings)(_ -> expr :: bindings),
                    const.args.map(_ => Skeleton.Wildcard[Tag](none)) ++ ps,
                    out).some.pure[F]
              case Skeleton.As(p, id) =>
                go(Row(pat, (id -> expr) :: bindings, (p :: ps), out))
            }
          case Row(_, _, Nil, _) => F.raiseError(new PatternException("Unexpected empty row"))
        }

      matrix.toList.traverse(go(_)).map(_.flatten)
  }

  private def defaultMatrix(expr: Selector[Tag], matrix: Matrix[Tag, Pat, Out]): F[Matrix[Tag, Pat, Out]] =
    matrix match {
      case Row(_, _, Nil, _) :: _ => matrix.pure[F]
      case _ =>
        def go(row: Row[Tag, Pat, Out]): F[Option[Row[Tag, Pat, Out]]] =
          row match {
            case Row(pat, bindings, Skeleton.Wildcard(id) :: ps, out) =>
              Row(pat, id.fold(bindings)(_ -> expr :: bindings), ps, out).some.pure[F]
            case Row(_, _, Skeleton.Constructor(_, _) :: _, _) =>
              none.pure[F]
            case Row(pat, bindings, Skeleton.As(p, id) :: ps, out) =>
              go(Row(pat, bindings, p :: ps, out)).map(_.map(_.bind(id -> expr)))
            case Row(_, _, Nil, _) =>
              F.raiseError(new PatternException("Unexpected empty row"))
          }
        matrix.toList.traverse(go(_)).map(_.flatten)
    }

}
