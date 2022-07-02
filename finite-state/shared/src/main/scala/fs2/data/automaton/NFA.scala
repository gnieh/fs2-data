package fs2.data.automaton

import cats.Show
import cats.syntax.all._

import scala.annotation.tailrec

import scala.collection.compat._

/** A non deterministic finite state automaton. */
class NFA[T](val init: Int, val finals: Set[Int], val transitions: Map[Int, List[(Symbol[T], Int)]]) {

  def epsilonClosure(qs: List[Int]): Set[Int] = {
    @tailrec
    def loop(qs: List[Int], visited: Set[Int]): Set[Int] =
      qs match {
        case q :: qs =>
          if (visited.contains(q))
            loop(qs, visited)
          else
            loop(transitions.getOrElse(q, Nil).collect { case (Symbol.Epsilon, q1) => q1 } reverse_::: qs, visited + q)
        case Nil => visited
      }
    loop(qs, Set.empty)
  }

  /** Returns the equivalent deterministic finite state automaton with catch all transitions. */
  def determinize: DFA[T] = {
    def loop(toProcess: List[Set[Int]],
             newStates: Map[Set[Int], Int],
             newTransitions: Map[Int, Map[T, Set[Int]]],
             newFallbacks: Map[Int, Set[Int]],
             newFinals: Set[Set[Int]]): DFA[T] =
      toProcess match {
        case Nil =>
          val finals = newFinals.map(newStates(_))
          val fallbacks = newFallbacks.view.mapValues(newStates(_)).toMap
          val transitions = newTransitions
            .map { case (src, ts) =>
              (src,
               ts.view.mapValues(newStates(_)).filterNot { case (_, tgt) => fallbacks.get(src).contains(tgt) }.toMap)
            }
            .toMap
            .filter(_._2.nonEmpty)
          new DFA(0, finals, transitions, fallbacks)
        case q :: qs =>
          if (newStates.contains(q)) {
            loop(qs, newStates, newTransitions, newFallbacks, newFinals)
          } else {
            val newQ = newStates.size
            val newStates1 = newStates.updated(q, newQ)
            val ts = q.toList
              .flatMap(transitions.get(_))
              .flatMap(_.collect {
                case t @ (Symbol.Any, _)    => t
                case t @ (Symbol.Sym(_), _) => t
              })
              .groupMap(_._1)(_._2)
              .view
              .mapValues(epsilonClosure(_))
              .toMap
            val fallback = ts.getOrElse(Symbol.Any, Set.empty)
            val newTs = ts.collect { case (Symbol.Sym(t), q) => (t, q ++ fallback) }.toMap
            val newTransitions1 = newTransitions.updated(newQ, newTs)
            val fallbacks1 = if (fallback.isEmpty) newFallbacks else newFallbacks.updated(newQ, fallback)
            val newFinals1 = if (finals.exists(q.contains(_))) newFinals + q else newFinals
            loop(qs ++ newTransitions1.values.flatMap(_.values) ++ ts.get(Symbol.Any),
                 newStates1,
                 newTransitions1,
                 fallbacks1,
                 newFinals1)
          }
      }

    loop(List(epsilonClosure(List(init))), Map.empty, Map.empty, Map.empty, Set.empty)
  }

}

object NFA {

  implicit def show[T: Show]: Show[NFA[T]] = new Show[NFA[T]] {

    implicit val transShow: Show[(Symbol[T], Int)] = Show.show { case (sym, q) =>
      show"q$q: $sym"
    }

    def showTransitions(t: NFA[T], q: Int): String =
      t.transitions
        .get(q)
        .map { ts =>
          show"q$q --> ${ts.mkString_(show"\n  q$q --> ")}"
        }
        .getOrElse("")

    override def show(t: NFA[T]): String =
      show"""stateDiagram-v2
            |  direction LR
            |  [*] --> q${t.init}
            |  ${t.finals.toList.map(q => show"q$q --> [*]").mkString_("\n  ")}
            |  ${t.transitions.keySet.toList.map(showTransitions(t, _)).mkString_("\n  ")}""".stripMargin

  }

}

sealed trait Symbol[+T]
object Symbol {
  case object Any extends Symbol[Nothing]
  case object Epsilon extends Symbol[Nothing]
  case class Sym[T](sym: T) extends Symbol[T]

  implicit def show[T: Show]: Show[Symbol[T]] = Show.show {
    case Any     => "*"
    case Epsilon => "ε"
    case Sym(s)  => Show[T].show(s)
  }
}
