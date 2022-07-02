package fs2.data.automaton

import cats.Show
import cats.syntax.all._

class DFA[T](val init: Int, val finals: Set[Int], val transitions: Map[Int, Map[T, Int]], val fallback: Map[Int, Int]) {

  def step(q: Int, t: T): Option[Int] =
    transitions.get(q).flatMap(_.get(t)).orElse(fallback.get(q))

}

object DFA {

  implicit def show[T: Show]: Show[DFA[T]] = new Show[DFA[T]] {

    implicit val transShow: Show[(T, Int)] = Show.show { case (sym, q) =>
      show"q$q: $sym"
    }

    def showTransitions(t: DFA[T], q: Int): String =
      t.transitions
        .get(q)
        .map { ts =>
          show"q$q --> ${ts.toList.mkString_(show"\n  q$q --> ")}"
        }
        .getOrElse("")

    override def show(t: DFA[T]): String =
      show"""stateDiagram-v2
            |  direction LR
            |  [*] --> q${t.init}
            |  ${t.finals.toList.map(q => show"q$q --> [*]").mkString_("\n  ")}
            |  ${t.transitions.keySet.toList.map(showTransitions(t, _)).mkString_("\n  ")}
            |  ${t.fallback.toList
             .map { case (src, tgt) => show"q$src --> q$tgt: [other]" }
             .mkString_("\n  ")}""".stripMargin

  }

}
