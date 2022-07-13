package fs2
package data
package html

sealed trait HtmlToken
object HtmlToken {

  case class Character(c: Char) extends HtmlToken

  case class OpenTag(name: String, attributes: Map[String, String], selfClosing: Boolean) extends HtmlToken
  case class EndTag(name: String) extends HtmlToken

  case class Comment(content: String) extends HtmlToken

  case class Doctype(forceQuirks: Boolean, name: String, publicid: Option[String], systemid: Option[String]) extends HtmlToken

}
