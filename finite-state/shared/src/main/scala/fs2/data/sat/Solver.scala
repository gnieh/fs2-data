package fs2.data.sat

import cats.syntax.all._

class Solver[Formula, Literal](implicit cnf: CNF[Formula, Literal]) {

  private type F[T] = Either[Res, T]

  private case class Env(gamma: Map[Literal, Set[Literal]], delta: List[(List[Literal], Set[Literal])])

  private sealed trait Res
  private object Res {
    case object Sat extends Res
    case class Unsat(deps: Set[Literal]) extends Res
  }

  private def assume(env: Env, f: (Literal, Set[Literal])): F[Env] =
    if (env.gamma.contains(f._1)) env.asRight
    else bcp(env.copy(gamma = env.gamma + f))

  private def bcp(env: Env): F[Env] =
    env.delta.foldLeftM(env.copy(delta = Nil)) { case (env, (lit, deps)) =>
      val l =
        lit.foldLeftM((List.empty[Literal], deps)) { case ((lits, deps), f) =>
          if (env.gamma.contains(f)) none
          else env.gamma.get(cnf.not(f)).fold((f :: lits, deps))(deps1 => (lits, deps ++ deps1)).some
        }
      l.fold(env.asRight[Res]) {
        case (Nil, deps)      => Res.Unsat(deps).asLeft // conflict
        case (f :: Nil, deps) => assume(env, (f, deps))
        case l                => env.copy(delta = l :: env.delta).asRight
      }
    }

  private def unsat(env: Env): F[Set[Literal]] =
    ((env.delta: @unchecked) match {
      case Nil => Res.Sat.asLeft
      case ((a :: _), _) :: _ =>
        assume(env, (a, Set(a))).flatMap(unsat(_)).recover { case Res.Unsat(deps) => deps }.flatMap { deps =>
          if (deps.contains(a))
            assume(env, (cnf.not(a), deps - a)).flatMap(unsat(_))
          else
            deps.asRight
        }
    }).recover { case Res.Unsat(deps) => deps }

  def isSatisfiable(f: Formula): Boolean =
    bcp(Env(Map.empty, cnf.make(f).map(_ -> Set.empty)))
      .flatMap(unsat(_).as(false))
      .leftMap {
        case Res.Sat      => true
        case Res.Unsat(_) => false
      }
      .merge

}
