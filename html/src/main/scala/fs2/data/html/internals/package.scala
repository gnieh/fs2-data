package fs2
package data
package html

import scala.collection.mutable.ListBuffer

package object internals {

  private val voidElements =
    Set("area", "base", "br", "col", "embed", "hr", "img", "input", "link", "meta", "source", "track", "wbr")
  private[internals] def acknowledgeSelfClosing(flag: Boolean, name: String): Boolean =
    flag && voidElements.contains(name)

  private val quirkPublicIds = Set("-//W3O//DTD W3 HTML Strict 3.0//EN//", "-/W3C/DTD HTML 4.0 Transitional/EN", "HTML")
  private val quirkPublicPrefixes = Set(
    "+//Silmaril//dtd html Pro v0r11 19970101//",
    "-//AS//DTD HTML 3.0 asWedit + extensions//",
    "-//AdvaSoft Ltd//DTD HTML 3.0 asWedit + extensions//",
    "-//IETF//DTD HTML 2.0 Level 1//",
    "-//IETF//DTD HTML 2.0 Level 2//",
    "-//IETF//DTD HTML 2.0 Strict Level 1//",
    "-//IETF//DTD HTML 2.0 Strict Level 2//",
    "-//IETF//DTD HTML 2.0 Strict//",
    "-//IETF//DTD HTML 2.0//",
    "-//IETF//DTD HTML 2.1E//",
    "-//IETF//DTD HTML 3.0//",
    "-//IETF//DTD HTML 3.2 Final//",
    "-//IETF//DTD HTML 3.2//",
    "-//IETF//DTD HTML 3//",
    "-//IETF//DTD HTML Level 0//",
    "-//IETF//DTD HTML Level 1//",
    "-//IETF//DTD HTML Level 2//",
    "-//IETF//DTD HTML Level 3//",
    "-//IETF//DTD HTML Strict Level 0//",
    "-//IETF//DTD HTML Strict Level 1//",
    "-//IETF//DTD HTML Strict Level 2//",
    "-//IETF//DTD HTML Strict Level 3//",
    "-//IETF//DTD HTML Strict//",
    "-//IETF//DTD HTML//",
    "-//Metrius//DTD Metrius Presentational//",
    "-//Microsoft//DTD Internet Explorer 2.0 HTML Strict//",
    "-//Microsoft//DTD Internet Explorer 2.0 HTML//",
    "-//Microsoft//DTD Internet Explorer 2.0 Tables//",
    "-//Microsoft//DTD Internet Explorer 3.0 HTML Strict//",
    "-//Microsoft//DTD Internet Explorer 3.0 HTML//",
    "-//Microsoft//DTD Internet Explorer 3.0 Tables//",
    "-//Netscape Comm. Corp.//DTD HTML//",
    "-//Netscape Comm. Corp.//DTD Strict HTML//",
    "-//O'Reilly and Associates//DTD HTML 2.0//",
    "-//O'Reilly and Associates//DTD HTML Extended 1.0//",
    "-//O'Reilly and Associates//DTD HTML Extended Relaxed 1.0//",
    "-//SQ//DTD HTML 2.0 HoTMetaL + extensions//",
    "-//SoftQuad Software//DTD HoTMetaL PRO 6.0::19990601::extensions to HTML 4.0//",
    "-//SoftQuad//DTD HoTMetaL PRO 4.0::19971010::extensions to HTML 4.0//",
    "-//Spyglass//DTD HTML 2.0 Extended//",
    "-//Sun Microsystems Corp.//DTD HotJava HTML//",
    "-//Sun Microsystems Corp.//DTD HotJava Strict HTML//",
    "-//W3C//DTD HTML 3 1995-03-24//",
    "-//W3C//DTD HTML 3.2 Draft//",
    "-//W3C//DTD HTML 3.2 Final//",
    "-//W3C//DTD HTML 3.2//",
    "-//W3C//DTD HTML 3.2S Draft//",
    "-//W3C//DTD HTML 4.0 Frameset//",
    "-//W3C//DTD HTML 4.0 Transitional//",
    "-//W3C//DTD HTML Experimental 19960712//",
    "-//W3C//DTD HTML Experimental 970421//",
    "-//W3C//DTD W3 HTML//",
    "-//W3O//DTD W3 HTML 3.0//",
    "-//WebTechs//DTD Mozilla HTML 2.0//",
    "-//WebTechs//DTD Mozilla HTML//"
  )
  private val quirkPublicPrefixesExclusive =
    Set("-//W3C//DTD HTML 4.01 Frameset//", "-//W3C//DTD HTML 4.01 Transitional//")

  private[internals] def isQuirkPublicId(id: Option[String], hasSystem: Boolean): Boolean =
    id.exists(id => quirkPublicIds.exists(_.equalsIgnoreCase(id))) || id.exists(id =>
      quirkPublicPrefixes.exists(p => id.toLowerCase.startsWith(p.toLowerCase))) || (!hasSystem && id.exists(id =>
      quirkPublicPrefixesExclusive.exists(p => id.toLowerCase.startsWith(p.toLowerCase))))

  private val limitedQuirkPublicIds = Set("-//W3C//DTD XHTML 1.0 Frameset//", "-//W3C//DTD XHTML 1.0 Transitional//")
  private[internals] val limitedQuirkPublicPrefixes =
    Set("-//W3C//DTD HTML 4.01 Frameset//", "-//W3C//DTD HTML 4.01 Transitional//")
  private[internals] def isLimitedQuirkPublicId(id: Option[String], hasSytem: Boolean): Boolean =
    id.exists(id => limitedQuirkPublicIds.exists(_.equalsIgnoreCase(id))) || (hasSytem && id.exists(id =>
      limitedQuirkPublicPrefixes.exists(p => id.toLowerCase.startsWith(p.toLowerCase))))

  private val quirkSystemIds = Set("http://www.ibm.com/data/dtd/v11/ibmxhtml1-transitional.dtd")
  private[internals] def isQuirkSystemId(id: Option[String]): Boolean =
    id.exists(id => quirkSystemIds.exists(_.equalsIgnoreCase(id)))

  private[internals] def pop(opens: List[String]) =
    opens match {
      case _ :: opens => opens
      case _          => opens
    }

  private[internals] def pop(opens: List[String], name: String) =
    opens match {
      case `name` :: opens => opens
      case _               => opens
    }

  private val implicitlyEndable = Set("dd", "dt", "li", "optgroup", "option", "p", "rb", "rp", "rt", "rtc")
  private[internals] def generateImpliedEndTags(opens: List[String],
                                                chunkAcc: ListBuffer[HtmlEvent],
                                                except: Set[String] = Set.empty): List[String] = {
    val matching = implicitlyEndable.removedAll(except)
    val (toClose, rest) = opens.span(matching.contains(_))
    chunkAcc ++= toClose.map(HtmlEvent.EndElement(_))
    rest
  }

  private val thoroughlyEndable =
    implicitlyEndable ++ Set("caption", "colgroup", "tbody", "td", "tfoot", "th", "thead", "tr")
  private[internals] def generateImpliedEndTagsThorouhly(opens: List[String],
                                                         chunkAcc: ListBuffer[HtmlEvent]): List[String] = {
    val (toClose, rest) = opens.span(thoroughlyEndable.contains(_))
    chunkAcc ++= toClose.map(HtmlEvent.EndElement(_))
    rest
  }
}
