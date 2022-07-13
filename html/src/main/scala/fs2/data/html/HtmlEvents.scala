package fs2
package data
package html

sealed trait HtmlEvent
object HtmlEvent {

  case object StartDocument extends HtmlEvent

  case class Character(c: Char) extends HtmlEvent

  case class DocumentType(name: String, publicid: String, systemid: String) extends HtmlEvent

  case class Comment(content: String) extends HtmlEvent

  case class Text(content: String) extends HtmlEvent

  case class StartElement(name: String, attributes: Map[String, String], selfClosing: Boolean) extends HtmlEvent

  case class EndElement(name: String) extends HtmlEvent

  case object EndDocument extends HtmlEvent

}
