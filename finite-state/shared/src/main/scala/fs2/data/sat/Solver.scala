package fs2.data.sat

import cats.syntax.all._
import scala.annotation.tailrec

class Solver[Formula, Literal](implicit cnf: CNF[Formula, Literal]) {

  private type F[T] = Either[Res, T]
  private type AnnClauses = Set[(List[Literal], Set[Literal])]

  private case class Env(gamma: Map[Literal, Set[Literal]], delta: List[(List[Literal], Set[Literal])])

  private[this] val emptyEnv = Env(Map.empty, Nil)

  private sealed trait Res
  private object Res {
    case object Sat extends Res
    case class Unsat(deps: Set[Literal]) extends Res
  }

  private def assume(env: Env, f: Literal, deps: Set[Literal]): F[Env] =
    if (env.gamma.contains(f)) env.asRight
    else bcp(Env(gamma = env.gamma.updated(f, deps), delta = Nil), env.delta)

  @tailrec
  private def filter(env: Env,
                     lit: List[Literal],
                     acc: List[Literal],
                     deps: Set[Literal]): Option[(List[Literal], Set[Literal])] =
    lit match {
      case f :: lit =>
        if (env.gamma.contains(f)) none
        else
          env.gamma
            .get(cnf.not(f)) match {
            case None        => filter(env, lit, f :: acc, deps)
            case Some(deps1) => filter(env, lit, acc, deps.union(deps1))

          }
      case _ => (acc, deps).some
    }

  @tailrec
  private def bcp(env: Env, delta: List[(List[Literal], Set[Literal])]): F[Env] =
    delta match {
      case Nil => env.asRight
      case (lit, deps) :: delta =>
        filter(env, lit, Nil, deps) match {
          case None              => env.asRight
          case Some((Nil, deps)) => Res.Unsat(deps).asLeft // conflict
          case Some((f :: Nil, deps)) =>
            assume(env, f, deps) match {
              case error @ Left(_) => error
              case Right(env)      => bcp(env, delta)
            }
          case Some(l) => bcp(env.copy(delta = l :: env.delta), delta)
        }
    }

  private def shift(a: Literal, clauses: AnnClauses): AnnClauses =
    clauses.map { x =>
      val deps = x._2
      if (deps.contains(a)) (cnf.not(a) :: x._1, deps.excl(a))
      else x
    }

  private def unsat(env: Env): F[(Set[Literal], AnnClauses)] =
    ((env.delta: @unchecked) match {
      case Nil => Res.Sat.asLeft
      case ((a :: _), _) :: _ =>
        assume(env, a, Set.empty.incl(a))
          .flatMap(unsat(_))
          .recover { case Res.Unsat(deps) => (deps, Set.empty[(List[Literal], Set[Literal])]) }
          .flatMap { case (deps, clauses) =>
            val clauses1 = shift(a, clauses)
            if (deps.contains(a)) {
              val n = cnf.not(a)
              val dn = deps.excl(a)
              assume(env.copy(delta = clauses1.toList reverse_::: env.delta), n, dn).flatMap(unsat(_).map {
                case (deps, clauses2) =>
                  (deps, clauses1.union(clauses2).incl((n :: Nil), dn))
              })
            } else {
              (deps, clauses1).asRight
            }
          }
    }).recover { case Res.Unsat(deps) => (deps, Set.empty) }

  def isSatisfiable(f: Formula): Boolean =
    bcp(emptyEnv, cnf.make(f).map(_ -> Set.empty))
      .flatMap(unsat(_)) match {
      case Right(_)           => false
      case Left(Res.Sat)      => true
      case Left(Res.Unsat(_)) => false
    }

}
