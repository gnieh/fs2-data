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

/** A pattern matching compiler to decision tree, based on _Compiling Successor ML Pattern Guards_
  * by J. Reppy and M. Zahir
  */
class Compiler[F[_], Expr, Tag, Pat, Out](implicit
    F: MonadError[F, Throwable],
    D: Defer[F],
    Tag: IsTag[Tag],
    Pat: IsPattern[Pat, Expr, Tag]) {

  private case class Row(patterns: List[Skeleton[Expr, Tag]], output: Out) {
    def isTrivial: Boolean =
      patterns.forall(_.isTrivial)
  }

  private type RawMatrix = List[(List[RawSkeleton[Expr, Tag]], Out)]

  private type Matrix = List[Row]

  private type Occs = List[Selector[Expr, Tag]]

  private def guardSplit(cases: List[List[RawSkeleton[Expr, Tag]]]): F[List[List[Skeleton[Expr, Tag]]]] =
    cases match {
      case Nil :: _ => F.pure(cases.as(Nil))
      case _ =>
        F.catchNonFatal(cases.transpose)
          .handleErrorWith(e => F.raiseError(new PatternException(s"malformed pattern matching: $cases", e)))
          .map(_.map(_.exists(_.guard.isDefined)))
          .map { hasGuards =>
            cases.map { pats =>
              pats.zip(hasGuards).flatMap {
                case (c @ RawSkeleton.Constructor(tag, args, guard), true) =>
                  // at least one row of this column has a guard, apply guard splitting rule
                  List[Skeleton[Expr, Tag]](Skeleton.Constructor(tag, args, true), Skeleton.Guard(guard))
                case (c @ RawSkeleton.Constructor(tag, args, guard), false) =>
                  List[Skeleton[Expr, Tag]](Skeleton.Constructor(tag, args, false))
                case (RawSkeleton.Wildcard(guard), true) =>
                  // at least one row of this column has a guard, apply guard splitting rule
                  List[Skeleton[Expr, Tag]](Skeleton.Wildcard(true), Skeleton.Guard(guard))
                case (RawSkeleton.Wildcard(guard), false) =>
                  List[Skeleton[Expr, Tag]](Skeleton.Wildcard(false))
              }
            }
          }
    }

  private def expandMatrix(matrix: RawMatrix) = guardSplit(matrix.map(_._1)).flatMap { skels =>
    skels.zip(matrix).traverse[F, Row] {
      case (Nil, _)          => F.raiseError(new PatternException("malformed pattern matching"))
      case (skels, (_, out)) => F.pure(Row(skels, out))
    }
  }

  def compile(cases: List[(Pat, Out)]): F[DecisionTree[Expr, Tag, Out]] = {
    val rows = cases.flatMap { case (pat, out) =>
      Pat.decompose(pat).map(skel => (List(skel), out))
    }
    expandMatrix(rows).flatMap(compileMatrix(List(Selector.Root()), _))
  }

  private def extractColumn[T](col: Int, pats: List[T]): Option[(T, List[T])] =
    pats.splitAt(col) match {
      case (head, c :: tail) => Some((c, head ++ tail))
      case _                 => None
    }

  private def insertColumn[T](col: Int, t: T, pats: List[T]): List[T] = {
    val (head, tail) = pats.splitAt(col)
    head ++ (t :: tail)
  }

  private def compileMatrix(occs: List[Selector[Expr, Tag]], matrix: Matrix): F[DecisionTree[Expr, Tag, Out]] = {
    matrix match {
      case Nil =>
        F.pure(DecisionTree.Fail())
      case (row @ Row(pats, out)) :: rows =>
        pats.indexWhere(!_.isTrivial) match {
          case -1 =>
            // all the patterns in the row are wildcard or trivially true guards
            // this means it always succeeds, and the rest is redundant
            // create the leaf with the output
            F.pure(DecisionTree.Leaf(out))
          case col =>
            extractColumn(if (occs.size == pats.size) col else col - 1, occs)
              .liftTo[F](new PatternException("occurrences cannot be empty"))
              .flatMap { case (occ, restOccs) =>
                matchColumn(col, occ, matrix).flatMap { case (g, smats, dmat) =>
                  def branches =
                    smats.toList
                      .traverse { case (tag, (hasGuard, subOccs, mat)) =>
                        compileMatrix(subOccs ++ (if (hasGuard) insertColumn(col, occ, restOccs) else restOccs), mat)
                          .map(tag -> _)
                      }
                      .map(_.toMap)
                  def defaultBranch =
                    dmat.traverse { case (hasGuard, dmat) =>
                      compileMatrix(if (hasGuard) insertColumn(col, occ, restOccs) else restOccs, dmat)
                    }
                  val sel = g.fold(occ)(Selector.Guard(occ, _))
                  (branches, defaultBranch).mapN(DecisionTree.Switch(sel, _, _))
                }
              }
        }
    }
  }

  private def column(col: Int, matrix: Matrix) =
    matrix.traverse(_.patterns.get(col).liftTo[F](new PatternException("malformed pattern matching")))

  private def constructors(skels: List[Skeleton[Expr, Tag]]): List[Skeleton.Constructor[Expr, Tag]] =
    skels.collect { case cons @ Skeleton.Constructor(_, _, _) => cons }

  private def specialize(col: Int, tag: Tag, args: List[RawSkeleton[Expr, Tag]], matrix: Matrix): F[Matrix] =
    matrix match {
      case Row(Nil, _) :: _ =>
        F.pure(matrix)
      case _ =>
        object ExtractColumn {
          def unapply(pats: List[Skeleton[Expr, Tag]]): Option[(Skeleton[Expr, Tag], List[Skeleton[Expr, Tag]])] =
            extractColumn(col, pats)
        }

        def go(row: Row): F[List[(List[RawSkeleton[Expr, Tag]], List[Skeleton[Expr, Tag]], Out)]] =
          row match {
            case Row(ExtractColumn(p, ps), out) =>
              p match {
                case Skeleton.Constructor(constTag, subps, _) =>
                  if (constTag === tag)
                    F.pure(List((subps, ps, out)))
                  else
                    F.pure(Nil)
                case Skeleton.Guard(Some(_)) =>
                  F.pure(List((Nil, ps, out)))
                case Skeleton.Wildcard(_) | Skeleton.Guard(None) =>
                  F.pure(List((args.as(RawSkeleton.wildcard[Expr, Tag]), ps, out)))
                case _ =>
                  F.raiseError(new PatternException(s"malformed pattern matching: $p"))
              }
            case Row(_, _) =>
              F.raiseError(new PatternException("unexpected empty row"))
          }
        matrix.flatTraverse(go(_)).flatMap { cases =>
          guardSplit(cases.map(_._1)).map { split =>
            split.zip(cases).map { case (subps, (_, ps, out)) => Row(subps ++ ps, out) }
          }
        }
    }

  private def matchColumn(
      col: Int,
      expr: Selector[Expr, Tag],
      matrix: Matrix): F[(Option[Expr], Map[Tag, (Boolean, Occs, Matrix)], Option[(Boolean, Matrix)])] = {

    object Col {
      def unapply(pats: List[Skeleton[Expr, Tag]]): Option[Skeleton[Expr, Tag]] =
        pats.get(col)
    }

    matrix match {
      case Row(Col(skel @ Skeleton.Constructor(_, _, hasGuard)), _) :: _ =>
        def go(cons: Skeleton.Constructor[Expr, Tag],
               matrices: Map[Tag, (Boolean, Occs, Matrix)]): F[Map[Tag, (Boolean, Occs, Matrix)]] = {
          specialize(col, cons.tag, cons.args, matrix).map { smat =>
            val soccs = select(cons.tag, cons.args, expr)
            matrices.updated(cons.tag, (hasGuard, soccs, smat))
          }
        }
        column(col, matrix)
          .flatMap(constructors(_).foldRightDefer(F.pure(Map.empty[Tag, (Boolean, Occs, Matrix)])) { (cons, acc) =>
            acc.flatMap(go(cons, _))
          })
          .map { smats =>
            val matchedTags = smats.keySet
            if (Tag.hasUnmatchedConstructor(skel, matchedTags)) {
              (none, smats, (hasGuard, defaultMatrix(col, matrix)).some)
            } else {
              (none, smats, none)
            }
          }
      case Row(Col(skel @ Skeleton.Guard(g)), _) :: _ =>
        column(col, matrix).flatMap {
          case Nil =>
            F.pure((none, Map.empty, none))
          case column =>
            val nonTrivialIdx = column.drop(1).indexWhere(!_.isTrivial)
            if (nonTrivialIdx >= 0) {
              // there are non trivially true guards after the first one
              specialize(col, Pat.trueTag, Nil, matrix.take(1 + nonTrivialIdx)).map { smat =>
                (g, Map(Pat.trueTag -> (false, Nil, smat)), (false, matrix.drop(1 + nonTrivialIdx)).some)
              }
            } else {
              // only trivially false guard afterwards
              specialize(col, Pat.trueTag, Nil, matrix).map { smat =>
                (g, Map(Pat.trueTag -> (false, Nil, smat)), (false, defaultMatrix(col, matrix)).some)
              }
            }
        }
      case _ =>
        F.pure((none, Map.empty, none))
    }
  }

  private def select(tag: Tag,
                     args: List[RawSkeleton[Expr, Tag]],
                     expr: Selector[Expr, Tag]): List[Selector[Expr, Tag]] =
    args.zipWithIndex.map { case (_, idx) => Selector.Cons(expr, tag, idx) }

  private def defaultMatrix(col: Int, matrix: Matrix): Matrix = {
    object ExtractColumn {
      def unapply(pats: List[Skeleton[Expr, Tag]]): Option[(Skeleton[Expr, Tag], List[Skeleton[Expr, Tag]])] =
        extractColumn(col, pats)
    }

    matrix match {
      case Row(Nil, _) :: _ =>
        matrix
      case _ =>
        matrix.collect { case Row(ExtractColumn(Skeleton.Wildcard(_) | Skeleton.Guard(None), ps), out) =>
          Row(ps, out)
        }
    }
  }

}
