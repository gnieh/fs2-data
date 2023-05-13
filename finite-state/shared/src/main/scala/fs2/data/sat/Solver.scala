package fs2.data.sat

import cats.syntax.all._

class Solver[Formula, Literal](implicit cnf: CNF[Formula, Literal]) {

  private type F[T] = Either[Res, T]

  private case class Env(gamma: Set[Literal], delta: List[List[Literal]])

  private def assume(env: Env, f: Literal): F[Env] =
    if (env.gamma.contains(f)) env.asRight
    else bcp(env.copy(gamma = env.gamma + f))

  private def bcp(env: Env): F[Env] =
    env.delta.foldLeftM(env.copy(delta = Nil)) { (env, lit) =>
      val l =
        lit.filterA(f =>
          if (env.gamma.contains(f)) none
          else (!env.gamma.contains(cnf.not(f))).some)
      l.fold(env.asRight[Res]) {
        case Nil      => Res.Unsat.asLeft // conflict
        case f :: Nil => assume(env, f)
        case l        => env.copy(delta = l :: env.delta).asRight
      }
    }

  private def unsat(env: Env): F[Unit] =
    ((env.delta: @unchecked) match {
      case Nil => Res.Sat.asLeft
      case (a :: _) :: _ =>
        assume(env, a).flatMap(unsat(_)).recover { case Res.Unsat => () } >>
          assume(env, cnf.not(a)).flatMap(unsat(_))
    }).recover { case Res.Unsat => () }

  def isSatisfiable(f: Formula): Boolean =
    bcp(Env(Set.empty, cnf.make(f)))
      .flatMap(unsat(_).as(false))
      .leftMap {
        case Res.Sat   => true
        case Res.Unsat => false
      }
      .merge

}

private sealed trait Res
private object Res {
  case object Sat extends Res
  case object Unsat extends Res
}
