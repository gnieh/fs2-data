package fs2.data.pattern

import fs2.data.esp.Tag
import fs2.data.esp.Conversion

import cats.Eq

sealed trait MiniXML
object MiniXML {
  case class Open(name: String) extends MiniXML
  case class Close(name: String) extends MiniXML
  case class Text(txt: String) extends MiniXML

  implicit val eq: Eq[MiniXML] = Eq.fromUniversalEquals

  implicit object MiniXMLSelectable extends Selectable[MiniXML, Tag[String]] {

    override def tree(e: MiniXML): ConstructorTree[Tag[String]] =
      e match {
        case Open(name)  => ConstructorTree(Tag.Open, List(ConstructorTree(Tag.Name(name), Nil)))
        case Close(name) => ConstructorTree(Tag.Close, List(ConstructorTree(Tag.Name(name), Nil)))
        case Text(t)     => ConstructorTree(Tag.Leaf, List(ConstructorTree(Tag.Value(t), Nil)))
      }

  }

  implicit object MiniXMLConversion extends Conversion[String, MiniXML] {

    override def makeOpen(t: String): MiniXML = MiniXML.Open(t)

    override def makeClose(t: String): MiniXML = MiniXML.Close(t)

    override def makeLeaf(t: String): MiniXML = MiniXML.Text(t)

  }

}
