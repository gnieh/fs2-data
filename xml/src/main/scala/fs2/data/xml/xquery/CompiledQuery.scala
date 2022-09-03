package fs2
package data
package xml
package xquery

import esp.{ESP, Tag}
import pattern.{IsPattern, Skeleton}
import xpath.Node
import fs2.data.xml.xpath.Predicate
import fs2.data.pattern.RawSkeleton

sealed trait XmlTag
private object XmlTag {
  case class Element(node: Node) extends XmlTag
  case class Attribute(name: QName) extends XmlTag
  case class AttributeValue(value: String) extends XmlTag
  case object NoAttributeValue extends XmlTag
  case object SomeAttributeValue extends XmlTag
}

case class XmlInput(node: Node, predicate: Option[Predicate])
object XmlInput {

  private def pushNegationDown(predicate: Predicate): Predicate =
    predicate match {
      case Predicate.Not(Predicate.Not(p)) =>
        pushNegationDown(p)
      case Predicate.Not(Predicate.Or(p1, p2)) =>
        Predicate.And(pushNegationDown(Predicate.Not(p1)), pushNegationDown(Predicate.Not(p2)))
      case Predicate.Not(Predicate.And(p1, p2)) =>
        Predicate.Or(pushNegationDown(Predicate.Not(p1)), pushNegationDown(Predicate.Not(p2)))
      case Predicate.Not(Predicate.True) =>
        Predicate.False
      case Predicate.Not(Predicate.False) =>
        Predicate.True
      case Predicate.Not(Predicate.Eq(a, v)) =>
        Predicate.Neq(a, v)
      case Predicate.Not(Predicate.Neq(a, v)) =>
        Predicate.Eq(a, v)
      case Predicate.And(p1, p2) =>
        Predicate.And(pushNegationDown(p1), pushNegationDown(p2))
      case Predicate.Or(p1, p2) =>
        Predicate.Or(pushNegationDown(p1), pushNegationDown(p2))
      case _ =>
        predicate
    }

  private def distributeAndOverOr(predicate: Predicate): List[List[Predicate]] =
    predicate match {
      case Predicate.Or(p1, p2)  => distributeAndOverOr(p1) ++ distributeAndOverOr(p2)
      case Predicate.And(p1, p2) => distributeAndOverOr(p1).flatMap(p1 => distributeAndOverOr(p2).map(p2 => p1 ++ p2))
      case _                     => List(List(predicate))
    }

  def dnf(predicate: Predicate): List[List[Predicate]] =
    // push negations down then use distributive law
    distributeAndOverOr(pushNegationDown(predicate))

  implicit object labelIsPattern extends IsPattern[XmlInput, Predicate, Tag[XmlTag]] {

    override def trueTag: Tag[XmlTag] = Tag.True

    override def decompose(in: XmlInput): List[RawSkeleton[Predicate, Tag[XmlTag]]] =
      List(
        RawSkeleton.Constructor(
          Tag.Name(XmlTag.Element(in.node)),
          Nil,
          in.predicate
        ))

  }

}

private sealed trait XmlOutput
private object XmlOutput {
  case class Element(name: QName) extends XmlOutput
  case class Text(txt: String) extends XmlOutput
}

class CompiledQuery[F[_]](mft: ESP[F, Predicate, XmlInput, XmlOutput]) {

  def pipe: Pipe[F, XmlEvent, XmlEvent] = ???

}
