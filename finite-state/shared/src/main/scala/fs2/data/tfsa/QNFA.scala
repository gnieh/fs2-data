package fs2.data
package tfsa

import utils.IntMap

import cats.Show
import fs2.data.utils.CharMap

sealed trait Priority
object Priority {
  case object Zero extends Priority
  case object One extends Priority
}

sealed trait Tag
object Tag {
  case class T(t: Int) extends Tag
  case class NoT(t: Int) extends Tag
}

sealed trait EpsTransition
object EpsTransition {
  case class Eps(tgt: Int) extends EpsTransition
  case class Tagged(t: Tag, tgt: Int) extends EpsTransition
  case class Choice(zero: Int, one: Int) extends EpsTransition
}

class TNFA(val init: Int,
           val finals: Set[Int],
           val transitions: IntMap[CharMap[Int]],
           val epstransitions: IntMap[EpsTransition]) {

  def isCore(q: Int): Boolean =
    finals.contains(q) || transitions.get(q).exists(!_.isEmpty)

}

object TNFA {

  implicit object TNFAShow extends Show[TNFA] {

    override def show(t: TNFA): String =
      s"""stateDiagram-v2
         |  direction LR
         |  [*] --> q${t.init}
         |  ${t.finals.map(q => s"q$q --> [*]").mkString("\n  ")}
         |  ${t.transitions.toList
          .map { case (q, ts) =>
            ts.toList
              .map { case (c, q1) =>
                s"q$q --> q$q1: $c"
              }
              .mkString("\n  ")
          }
          .mkString("\n  ")}
         |  ${t.epstransitions.toList
          .map {
            case (q, EpsTransition.Eps(q1)) =>
              s"q$q --> q$q1: ε"
            case (q, EpsTransition.Tagged(t, q1)) =>
              s"q$q --> q$q1: $t"
            case (q, EpsTransition.Choice(q1, q2)) =>
              s"""q$q --> q$q1: 0
                 |  q$q --> q$q2: 1""".stripMargin
          }
          .mkString("\n  ")}
         |""".stripMargin

  }

}
