package fs2.data.regex

import fs2.data.tfsa.TNFA

import cats.syntax.all._
import fs2.data.utils.IntMap
import fs2.data.tfsa.{Tag => TTag, _}
import scala.annotation.tailrec
import fs2.data.utils.CharMap

sealed trait TRE
object TRE {
  private case object Empty extends TRE
  private case object Epsilon extends TRE
  private case class Character(c: Char) extends TRE
  private case class Tag(n: Int) extends TRE
  private case class NoTag(n: Int) extends TRE
  private case class Or(e1: TRE, e2: TRE) extends TRE
  private case class Concat(e1: TRE, e2: TRE) extends TRE
  private case class Rep(e: TRE, min: Int, max: Option[Int]) extends TRE

  val empty: TRE = Empty

  val epsilon: TRE = Epsilon

  def char(c: Char): TRE = Character(c)

  def tag(t: Int): TRE = Tag(t)

  def or(e1: TRE, e2: TRE): TRE =
    (e1, e2) match {
      case (Empty, _) => e2
      case (_, Empty) => e1
      case (_, _)     => Or(e1, e2)
    }

  def concat(e1: TRE, e2: TRE): TRE =
    (e1, e2) match {
      case (Empty, _)   => Empty
      case (_, Empty)   => Empty
      case (Epsilon, _) => e2
      case (_, Epsilon) => e1
      case (_, _)       => Concat(e1, e2)
    }

  def rep(e: TRE, min: Int, max: Int): TRE =
    Rep(e, min, max.some)

  def atleast(e: TRE, min: Int): TRE =
    Rep(e, min, none)

  def rep(e: TRE, n: Int): TRE =
    Rep(e, n, n.some)

  def star(e: TRE): TRE =
    Rep(e, 0, none)

  def plus(e: TRE): TRE =
    Rep(e, 1, none)

  def optional(e: TRE): TRE =
    Rep(e, 0, 1.some)

  def normalize(re: TRE): TRE =
    re match {
      case Empty          => Empty
      case Epsilon        => Epsilon
      case Character(_)   => re
      case Tag(_)         => re
      case NoTag(_)       => re
      case Or(e1, e2)     => Or(Concat(normalize(e1), tags(e2)), Concat(normalize(e2), tags(e1)))
      case Concat(e1, e2) => Concat(normalize(e1), normalize(e2))
      case Rep(e, 0, m)   => Or(Rep(normalize(e), 1, m), tags(e))
      case Rep(e, n, m)   => Rep(normalize(e), n, m)
    }

  private def tags(re: TRE): TRE =
    re match {
      case Tag(t)         => NoTag(t)
      case Or(e1, e2)     => concat(tags(e1), tags(e2))
      case Concat(e1, e2) => concat(tags(e1), tags(e2))
      case Rep(e, _, _)   => tags(e)
      case _              => Epsilon
    }

  def toTNFA(e: TRE): TNFA = {
    def loop(q: Int,
             e: TRE,
             transitions: IntMap[CharMap[Int]],
             epstransitions: IntMap[EpsTransition]): (Int, IntMap[CharMap[Int]], IntMap[EpsTransition]) =
      e match {
        case Empty =>
          (q + 1, transitions, epstransitions)
        case Epsilon =>
          (q, transitions, epstransitions)
        case Character(c) =>
          (q + 1, transitions.combine(IntMap.one(q, CharMap.one(c, q + 1))), epstransitions)
        case Tag(t) =>
          (q + 1, transitions, epstransitions.updated(q, EpsTransition.Tagged(TTag.T(t), q + 1)))
        case NoTag(t) =>
          (q + 1, transitions, epstransitions.updated(q, EpsTransition.Tagged(TTag.NoT(t), q + 1)))
        case Or(e1, e2) =>
          val (q1, transitions1, epstransitions1) = loop(q + 1, e1, transitions, epstransitions)
          val (q2, transitions2, epstransitions2) = loop(q1 + 1, e2, transitions1, epstransitions1)
          (q2 + 1,
           transitions2,
           epstransitions2
             .updated(q, EpsTransition.Choice(q + 1, q1 + 1))
             .updated(q1, EpsTransition.Eps(q2 + 1))
             .updated(q2, EpsTransition.Eps(q2 + 1)))
        case Concat(e1, e2) =>
          val (q1, transitions1, epstransitions1) = loop(q, e1, transitions, epstransitions)
          loop(q1, e2, transitions1, epstransitions1): @tailrec
        case Rep(e, n, None) =>
          val (last, q1, transitions1, epstransitions1) = (1 to n).foldLeft((q, q, transitions, epstransitions)) {
            case ((_, q, transitions, epstransitions), _) =>
              val (q1, transitions1, epstransitions1) = loop(q, e, transitions, epstransitions)
              (q, q1, transitions1, epstransitions1)
          }
          (q1 + 1, transitions1, epstransitions1.updated(q1, EpsTransition.Choice(last, q1 + 1)))
        case Rep(e, n, Some(m)) if n == m =>
          // exact count
          (1 to n).foldLeft((q, transitions, epstransitions)) { case ((q, transitions, epstransitions), _) =>
            loop(q, e, transitions, epstransitions)
          }
        case Rep(e, n, Some(m)) =>
          val (q1, transitions1, epstransitions1) = (1 to n).foldLeft((q, transitions, epstransitions)) {
            case ((q, transitions, epstransitions), _) => loop(q, e, transitions, epstransitions)
          }
          val ((q2, transitions2, epstransitions2), states) =
            (n until m).toList.mapAccumulate((q1 + 1, transitions1, epstransitions1)) {
              case ((q, transitions, epstransitions), _) =>
                val acc @ (q1, transitions1, epstransitions1) = loop(q, e, transitions, epstransitions)
                ((q1 + 1, transitions1, epstransitions1), q1)
            }
          (q2,
           transitions2,
           states.foldLeft(epstransitions2.updated(q1, EpsTransition.Choice(q1 + 1, q2))) { (acc, q) =>
             if (q + 1 == q2)
               // last one, simple epsilon transition
               acc.updated(q, EpsTransition.Eps(q2))
             else
               acc.updated(q, EpsTransition.Choice(q + 1, q2))
           })
      }

    val (q, transitions, epstransitions) = loop(0, normalize(e), IntMap.empty, IntMap.empty)
    new TNFA(0, Set(q), transitions, epstransitions)
  }

}
