package fs2.data.pattern

import cats.effect.Sync
import cats.syntax.all._

class PatternException(msg: String) extends Exception(msg)

class Compiler[F[_], Tag, Pat, Out](heuristic: Heuristic[Tag])(implicit F: Sync[F]) {

  /** Compiles a pattern into a decision tree. */
  def compile(cases: List[(Pat, Out)])(implicit Tag: IsTag[Tag], Pat: Pattern[Pat, Tag]): F[DecisionTree[Tag, Out]] = {
    val matrix = cases.flatMap { case (pat, out) =>
      Pat.decompose(pat).map(skel => Row[Tag, Pat, Out](pat, Nil, List(skel), out))
    }
    compileMatrix(heuristic, List(Select.NoSel), matrix)
  }

  private def compileMatrix(heuristic: Heuristic[Tag], occurrences: List[Select[Tag]], matrix: Matrix[Tag, Pat, Out])(
      implicit Tag: IsTag[Tag]): F[DecisionTree[Tag, Out]] =
    matrix match {
      case Nil => F.pure(DecisionTree.Fail())
      case (row @ Row(_, bindings, pats, out)) :: ors =>
        if (row.isWidcard) {
          // all the patterns in the row are wildcards on the top row
          // this means this always succeeds, and the other rows are
          // redundant
          def bindingsIn(occ: Select[Tag], skel: Skeleton[Tag]): F[List[Binding[Select[Tag]]]] =
            skel match {
              case Skeleton.Constructor(_, _) =>
                F.raiseError(new PatternException("A wildcard row cannot contain a constructor skeleton"))
              case Skeleton.Wildcard(id) => List(id -> occ).pure
              case Skeleton.As(p, id)    => bindingsIn(occ, p).map((id.some -> occ) :: _)
            }
          occurrences
            .zip(pats)
            .flatTraverse { case (occ, skel) => bindingsIn(occ, skel) }
            .map(bds => DecisionTree.Leaf(bds ++ bindings, out))
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
                        def swapFailureCont(l: List[List[Skeleton[Tag]]]) =
                          l.map(swapBack(maxScoreIndex, _))
                        def makeBranch(subOccs: List[Select[Tag]], subp: SubProblem) =
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

  private def swapBack[T](n: Int, ts: List[T]): List[T] =
    ts match {
      case Nil =>
        throw new PatternException("Cannot swap back on empty list (THIS IS A BUG)")
      case p :: ps =>
        val (ys, zs) = ps.splitAt(n - 1)
        ys ++ (p :: zs)
    }

  private def swapColumn(idx: Int, matrix: Matrix[Tag, Pat, Out]): F[Matrix[Tag, Pat, Out]] = {
    val vmatrix @ VMatrix(columns, _) = matrix.verticalView
    swapFront(idx, columns).map { columns =>
      vmatrix.copy(columns = columns).horizontalView
    }
  }

  private case class SubProblem(subMatrix: Matrix[Tag, Pat, Out])

  private def matchFirstColumn(expr: Select[Tag], matrix: Matrix[Tag, Pat, Out])(implicit
      Tag: IsTag[Tag]): F[(Map[Tag, (List[Select[Tag]], SubProblem)], Option[SubProblem])] =
    matrix match {
      case Row(_, _, skel :: _, _) :: _ =>
        def go(cons: Skeleton.Constructor[Tag],
               matrices: Map[Tag, (List[Select[Tag]], SubProblem)]): F[Map[Tag, (List[Select[Tag]], SubProblem)]] = {
          specialize(expr, cons, matrix).map { smat =>
            val soccs = select(cons, expr)
            matrices.updated(cons.tag, (soccs, SubProblem(smat)))
          }
        }
        val specializedMatrices: F[Map[Tag, (List[Select[Tag]], SubProblem)]] =
          matrix.verticalView.columns.headOption
            .liftTo[F](new PatternException("the pattern matrix cannot be empty"))
            .map(_.patterns)
            .flatMap(headConstructors(_).foldRightDefer(F.pure(Map.empty[Tag, (List[Select[Tag]], SubProblem)])) {
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

  private def select(const: Skeleton.Constructor[Tag], expr: Select[Tag]): List[Select[Tag]] =
    const.args.zipWithIndex.map { case (_, idx) => Select.Sel(expr, const.tag, idx) }

  private def specialize(expr: Select[Tag], const: Skeleton.Constructor[Tag], matrix: Matrix[Tag, Pat, Out])(implicit
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
                Row(pat, (id -> expr) :: bindings, const.args.map(_ => Skeleton.Wildcard(none)) ++ ps, out).some.pure[F]
              case Skeleton.As(p, id) =>
                go(Row(pat, (id.some -> expr) :: bindings, (p :: ps), out))
            }
          case Row(_, _, Nil, _) => F.raiseError(new PatternException("Unexpected empty row"))
        }

      matrix.toList.traverse(go(_)).map(_.flatten)
  }

  private def defaultMatrix(expr: Select[Tag], matrix: Matrix[Tag, Pat, Out]): F[Matrix[Tag, Pat, Out]] =
    matrix match {
      case Row(_, _, Nil, _) :: _ => matrix.pure[F]
      case _ =>
        def go(row: Row[Tag, Pat, Out]): F[Option[Row[Tag, Pat, Out]]] =
          row match {
            case Row(pat, bindings, Skeleton.Wildcard(id) :: ps, out) =>
              Row(pat, (id -> expr) :: bindings, ps, out).some.pure[F]
            case Row(pat, bindings, Skeleton.Constructor(_, _) :: ps, out) =>
              none.pure[F]
            case Row(pat, bindings, Skeleton.As(p, id) :: ps, out) =>
              go(Row(pat, bindings, p :: ps, out)).map(_.map(_.bind(id.some -> expr)))
            case Row(_, _, Nil, _) =>
              F.raiseError(new PatternException("Unexpected empty row"))
          }
        matrix.toList.traverse(go(_)).map(_.flatten)
    }

}
