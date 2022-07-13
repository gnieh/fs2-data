package fs2
package data
package html
package internals

import fs2.data.text.CharLikeChunks

sealed trait QuirksMode
object QuirksMode {
  case object NoQuirks extends QuirksMode
  case object Quirks extends QuirksMode
  case object LimitedQuirks extends QuirksMode
}

case class ParserContext[F[_], Ctx](parserCannotChangeMode: Boolean = false,
                                    framesetOk: Boolean = false,
                                    mode: QuirksMode = QuirksMode.NoQuirks) {
  def switchToMode(m: QuirksMode): ParserContext[F, Ctx] =
    if (parserCannotChangeMode) this
    else this.copy(mode = m)

}

private[html] class HtmlParser[F[_], T](implicit F: RaiseThrowable[F], T: CharLikeChunks[F, T]) {

  val tokenizer = new HtmlTokenizer[F, T]
  import tokenizer._

  private type Ctx = tokenizer.T.Context
  private type PCtx = ParserContext[F, Ctx]

  // https://html.spec.whatwg.org/#the-initial-mode
  def initial(pulled: Pulled[F, Ctx, HtmlEvent], pctx: PCtx): Pull[F, HtmlEvent, Unit] =
    pulled match {
      case Pulled(ctx, chunkAcc, opens, Some(token), next) =>
        token match {
          case HtmlToken.Character('\t' | '\r' | '\n' | '\f' | ' ') =>
            // just ignore whitespaces
            next(ctx, opens, chunkAcc).flatMap(initial(_, pctx))
          case HtmlToken.Comment(content) =>
            next(ctx, opens, chunkAcc += HtmlEvent.Comment(content)).flatMap(initial(_, pctx))
          case HtmlToken.Doctype(forceQuirks, name, publicid, systemid) =>
            val pctx1 =
              if (forceQuirks || name != "html" || isQuirkPublicId(publicid, systemid.isDefined) || isQuirkSystemId(
                  systemid))
                pctx.switchToMode(QuirksMode.Quirks)
              else if (isLimitedQuirkPublicId(publicid, systemid.isDefined))
                pctx.switchToMode(QuirksMode.LimitedQuirks)
              else
                pctx
            next(ctx, opens, chunkAcc += HtmlEvent.DocumentType(name, publicid.getOrElse(""), systemid.getOrElse("")))
              .flatMap(beforeHtml(_, pctx1))
          case _ =>
            beforeHtml(pulled, pctx.switchToMode(QuirksMode.Quirks))
        }
      case _ =>
        beforeHtml(pulled, pctx.switchToMode(QuirksMode.Quirks))
    }

  // https://html.spec.whatwg.org/#the-before-html-mode
  def beforeHtml(pulled: Pulled[F, Ctx, HtmlEvent], pctx: PCtx): Pull[F, HtmlEvent, Unit] =
    pulled match {
      case Pulled(ctx, chunkAcc, opens, Some(token), next) =>
        token match {
          case HtmlToken.Doctype(_, _, _, _) =>
            // just ignore it
            next(ctx, opens, chunkAcc).flatMap(beforeHtml(_, pctx))
          case HtmlToken.Comment(content) =>
            next(ctx, opens, chunkAcc += HtmlEvent.Comment(content)).flatMap(beforeHtml(_, pctx))
          case HtmlToken.Character('\t' | '\r' | '\n' | '\f' | ' ') =>
            // just ignore whitespaces
            next(ctx, opens, chunkAcc).flatMap(beforeHtml(_, pctx))
          case HtmlToken.OpenTag("html", attrs, selfClosing) =>
            next(ctx, "html" :: opens, chunkAcc += HtmlEvent.StartElement("html", attrs, selfClosing))
              .flatMap(beforeHead(_, pctx))
          case HtmlToken.EndTag("head" | "body" | "html" | "br") =>
            beforeHead(pulled
                         .pushOpen("html")
                         .copy(chunkAcc = pulled.chunkAcc += HtmlEvent.StartElement("html", Map.empty, false)),
                       pctx)
          case HtmlToken.EndTag(_) =>
            // just ignore closing tags
            next(ctx, opens, chunkAcc).flatMap(beforeHtml(_, pctx))
          case _ =>
            beforeHead(pulled
                         .pushOpen("html")
                         .copy(chunkAcc = pulled.chunkAcc += HtmlEvent.StartElement("html", Map.empty, false)),
                       pctx)
        }
      case _ =>
        beforeHead(
          pulled.pushOpen("html").copy(chunkAcc = pulled.chunkAcc += HtmlEvent.StartElement("html", Map.empty, false)),
          pctx)
    }

  // https://html.spec.whatwg.org/#the-before-head-mode
  def beforeHead(pulled: Pulled[F, Ctx, HtmlEvent], pctx: PCtx): Pull[F, HtmlEvent, Unit] =
    pulled match {
      case Pulled(ctx, chunkAcc, opens, Some(token), next) =>
        token match {
          case HtmlToken.Character('\t' | '\r' | '\n' | '\f' | ' ') =>
            // just ignore whitespaces
            next(ctx, opens, chunkAcc).flatMap(beforeHead(_, pctx))
          case HtmlToken.Comment(content) =>
            next(ctx, opens, chunkAcc += HtmlEvent.Comment(content)).flatMap(beforeHead(_, pctx))
          case HtmlToken.Doctype(_, _, _, _) =>
            // just ignore it
            next(ctx, opens, chunkAcc).flatMap(beforeHead(_, pctx))
          case HtmlToken.OpenTag("html", _, _) =>
            // [SPEC DEVIATION]
            // just ignore it
            next(ctx, opens, chunkAcc).flatMap(beforeHead(_, pctx))
          case HtmlToken.OpenTag("head", attrs, selfClosing) =>
            next(ctx, "head" :: opens, chunkAcc += HtmlEvent.StartElement("head", attrs, selfClosing))
              .flatMap(inHead(_, pctx))
          case HtmlToken.EndTag("head" | "body" | "html" | "br") =>
            inHead(pulled
                     .pushOpen("head")
                     .copy(chunkAcc = pulled.chunkAcc += HtmlEvent.StartElement("head", Map.empty, false)),
                   pctx)
          case HtmlToken.EndTag(_) =>
            // just ignore it
            next(ctx, opens, chunkAcc).flatMap(beforeHead(_, pctx))
          case _ =>
            inHead(pulled
                     .pushOpen("head")
                     .copy(chunkAcc = pulled.chunkAcc += HtmlEvent.StartElement("head", Map.empty, false)),
                   pctx)
        }
      case _ =>
        inHead(
          pulled.pushOpen("head").copy(chunkAcc = pulled.chunkAcc += HtmlEvent.StartElement("head", Map.empty, false)),
          pctx)
    }

  // https://html.spec.whatwg.org/#parsing-main-inhead
  def inHead(pulled: Pulled[F, Ctx, HtmlEvent], pctx: PCtx): Pull[F, HtmlEvent, Unit] =
    pulled match {
      case Pulled(ctx, chunkAcc, opens, Some(token), next) =>
        token match {
          case HtmlToken.Character(c @ ('\t' | '\r' | '\n' | '\f' | ' ')) =>
            // just ignore whitespaces
            next(ctx, opens, chunkAcc += HtmlEvent.Character(c)).flatMap(inHead(_, pctx))
          case HtmlToken.Comment(content) =>
            next(ctx, opens, chunkAcc += HtmlEvent.Comment(content)).flatMap(inHead(_, pctx))
          case HtmlToken.OpenTag("html", _, _) =>
            // [SPEC DEVIATION]
            // just ignore it
            next(ctx, opens, chunkAcc).flatMap(inHead(_, pctx))
          case HtmlToken.OpenTag(name @ ("base" | "basefront" | "bgsound" | "link"), attrs, selfClosing) =>
            if (acknowledgeSelfClosing(selfClosing, name))
              next(ctx,
                   opens,
                   chunkAcc += HtmlEvent.StartElement(name, attrs, selfClosing) += HtmlEvent.EndElement(name))
                .flatMap(inHead(_, pctx))
            else
              next(ctx, opens, chunkAcc += HtmlEvent.StartElement(name, attrs, selfClosing))
                .flatMap(inHead(_, pctx))
          case HtmlToken.OpenTag("meta", attrs, selfClosing) =>
            // [SPEC DEVIATION]
            // no change in charset possible
            if (acknowledgeSelfClosing(selfClosing, "meta"))
              next(ctx,
                   opens,
                   chunkAcc += HtmlEvent.StartElement("meta", attrs, selfClosing) += HtmlEvent.EndElement("meta"))
                .flatMap(inHead(_, pctx))
            else
              next(ctx, opens, chunkAcc += HtmlEvent.StartElement("meta", attrs, selfClosing))
                .flatMap(inHead(_, pctx))
          case HtmlToken.OpenTag("title", attrs, selfClosing) =>
            rcdata(ctx, "title" :: opens, chunkAcc += HtmlEvent.StartElement("title", attrs, selfClosing))
              .flatMap(text(_, pctx, inHead))
          case HtmlToken.OpenTag(name @ ("noframes" | "style"), attrs, selfClosing) =>
            rawText(ctx, name :: opens, chunkAcc += HtmlEvent.StartElement(name, attrs, selfClosing))
              .flatMap(text(_, pctx, inHead))
          case HtmlToken.OpenTag("noscript", attrs, selfClosing) =>
            next(ctx, "noscript" :: opens, chunkAcc += HtmlEvent.StartElement("noscript", attrs, selfClosing))
              .flatMap(inHeadNoscript(_, pctx))
          case HtmlToken.OpenTag("script", attrs, selfClosing) =>
            scriptData(ctx, "script" :: opens, chunkAcc += HtmlEvent.StartElement("script", attrs, selfClosing))
              .flatMap(text(_, pctx, inHead))
          case HtmlToken.EndTag("head") =>
            val opens1 = pop(opens, "head")
            next(ctx, opens1, chunkAcc += HtmlEvent.EndElement("head")).flatMap(afterHead(_, pctx))
          case HtmlToken.EndTag("body" | "html" | "br") =>
            val opens1 = pop(opens, "head")
            afterHead(pulled.copy(opens = opens1), pctx)
          case HtmlToken.OpenTag("template", attrs, selfClosing) =>
            next(ctx, "template" :: opens, chunkAcc += HtmlEvent.StartElement("template", attrs, selfClosing))
              .flatMap(inTemplate(_, pctx))
          case HtmlToken.EndTag("template") =>
            val (toClose, rest) = opens.span(_ != "template")
            rest match {
              case "template" :: rest =>
                next(ctx, rest, chunkAcc ++= toClose.map(HtmlEvent.EndElement(_))).flatMap(inHead(_, pctx))
              case _ =>
                // just ignore it
                next(ctx, opens, chunkAcc).flatMap(inHead(_, pctx))
            }
          case HtmlToken.OpenTag("head", _, _) =>
            // just ignore it
            next(ctx, opens, chunkAcc).flatMap(inHead(_, pctx))
          case HtmlToken.EndTag(_) =>
            // just ignore it
            next(ctx, opens, chunkAcc).flatMap(inHead(_, pctx))
          case _ =>
            val opens1 = pop(opens, "head")
            afterHead(pulled.copy(opens = opens1, chunkAcc = chunkAcc += HtmlEvent.EndElement("head")), pctx)
        }
      case _ =>
        val opens1 = pop(pulled.opens, "head")
        afterHead(pulled.copy(opens = opens1, chunkAcc = pulled.chunkAcc += HtmlEvent.EndElement("head")), pctx)
    }

  // https://html.spec.whatwg.org/#parsing-main-inheadnoscript
  def inHeadNoscript(pulled: Pulled[F, Ctx, HtmlEvent], pctx: PCtx): Pull[F, HtmlEvent, Unit] =
    pulled match {
      case Pulled(ctx, chunkAcc, opens, Some(token), next) =>
        token match {
          case HtmlToken.Doctype(_, _, _, _) =>
            // ignore it
            next(ctx, opens, chunkAcc).flatMap(inHeadNoscript(_, pctx))
          case HtmlToken.OpenTag("html", attrs, selfClosing) =>
            // [SPEC DEVIATION]
            // don't add new attributes
            // just ignore it in any case
            next(ctx, opens, chunkAcc).flatMap(inHeadNoscript(_, pctx))
          case HtmlToken.EndTag("noscript") =>
            val opens1 = pop(opens, "noscript")
            next(ctx, opens1, chunkAcc += HtmlEvent.EndElement("noscript")).flatMap(inHead(_, pctx))
          case HtmlToken.Character(c @ ('\t' | '\r' | '\n' | '\f' | ' ')) =>
            next(ctx, opens, chunkAcc += HtmlEvent.Character(c)).flatMap(inHeadNoscript(_, pctx))
          case HtmlToken.Comment(content) =>
            next(ctx, opens, chunkAcc += HtmlEvent.Comment(content)).flatMap(inHeadNoscript(_, pctx))
          case HtmlToken.OpenTag(name @ ("basefront" | "bgsound" | "link"), attrs, selfClosing) =>
            if (acknowledgeSelfClosing(selfClosing, name))
              next(ctx,
                   opens,
                   chunkAcc += HtmlEvent.StartElement(name, attrs, selfClosing) += HtmlEvent.EndElement(name))
                .flatMap(inHeadNoscript(_, pctx))
            else
              next(ctx, name :: opens, chunkAcc += HtmlEvent.StartElement(name, attrs, selfClosing))
                .flatMap(inHeadNoscript(_, pctx))
          case HtmlToken.OpenTag("meta", attrs, selfClosing) =>
            // [SPEC DEVIATION]
            // no change in charset possible
            if (acknowledgeSelfClosing(selfClosing, "meta"))
              next(ctx,
                   opens,
                   chunkAcc += HtmlEvent.StartElement("meta", attrs, selfClosing) += HtmlEvent.EndElement("meta"))
                .flatMap(inHeadNoscript(_, pctx))
            else
              next(ctx, opens, chunkAcc += HtmlEvent.StartElement("meta", attrs, selfClosing))
                .flatMap(inHeadNoscript(_, pctx))
          case HtmlToken.OpenTag(name @ ("noframes" | "style"), attrs, selfClosing) =>
            rawText(ctx, name :: opens, chunkAcc += HtmlEvent.StartElement(name, attrs, selfClosing))
              .flatMap(text(_, pctx, inHeadNoscript))
          case HtmlToken.EndTag("br") =>
            val opens1 = pop(opens, "noscript")
            inHead(pulled.copy(opens = opens1), pctx)
          case HtmlToken.OpenTag("head" | "noscript", _, _) =>
            // just ignore it
            next(ctx, opens, chunkAcc).flatMap(inHeadNoscript(_, pctx))
          case HtmlToken.EndTag(_) =>
            // just ignore it
            next(ctx, opens, chunkAcc).flatMap(inHeadNoscript(_, pctx))
          case _ =>
            val opens1 = pop(opens, "noscript")
            inHead(pulled.copy(opens = opens1), pctx)
        }
      case _ =>
        val opens1 = pop(pulled.opens, "noscript")
        inHead(pulled.copy(opens = opens1), pctx)
    }

  // https://html.spec.whatwg.org/#the-after-head-insertion-mode
  def afterHead(pulled: Pulled[F, Ctx, HtmlEvent], pctx: PCtx): Pull[F, HtmlEvent, Unit] =
    pulled match {
      case Pulled(ctx, chunkAcc, opens, Some(token), next) =>
        token match {
          case HtmlToken.Character(c @ ('\t' | '\r' | '\n' | '\f' | ' ')) =>
            next(ctx, opens, chunkAcc += HtmlEvent.Character(c)).flatMap(afterHead(_, pctx))
          case HtmlToken.Doctype(_, _, _, _) =>
            // ignore it
            next(ctx, opens, chunkAcc).flatMap(afterHead(_, pctx))
          case HtmlToken.Comment(content) =>
            next(ctx, opens, chunkAcc += HtmlEvent.Comment(content)).flatMap(afterHead(_, pctx))
          case HtmlToken.OpenTag("html", attrs, selfClosing) =>
            // [SPEC DEVIATION]
            // don't add new attributes
            // just ignore it in any case
            next(ctx, opens, chunkAcc).flatMap(inHeadNoscript(_, pctx))
          case HtmlToken.OpenTag("body", attrs, selfClosing) =>
            next(ctx, "body" :: opens, chunkAcc += HtmlEvent.StartElement("body", attrs, selfClosing))
              .flatMap(inBody(_, pctx.copy(framesetOk = false)))
          case HtmlToken.OpenTag("frameset", attrs, selfClosing) =>
            next(ctx, "frameset" :: opens, chunkAcc += HtmlEvent.StartElement("frameset", attrs, selfClosing))
              .flatMap(inFrameset(_, pctx))
          case HtmlToken.OpenTag(name @ ("base" | "basefront" | "bgsound" | "link"), attrs, selfClosing) =>
            if (acknowledgeSelfClosing(selfClosing, name))
              next(ctx,
                   opens,
                   chunkAcc += HtmlEvent.StartElement(name, attrs, selfClosing) += HtmlEvent.EndElement(name))
                .flatMap(afterHead(_, pctx))
            else
              next(ctx, opens, chunkAcc += HtmlEvent.StartElement(name, attrs, selfClosing))
                .flatMap(afterHead(_, pctx))
          case HtmlToken.OpenTag("meta", attrs, selfClosing) =>
            // [SPEC DEVIATION]
            // no change in charset possible
            if (acknowledgeSelfClosing(selfClosing, "meta"))
              next(ctx,
                   opens,
                   chunkAcc += HtmlEvent.StartElement("meta", attrs, selfClosing) += HtmlEvent.EndElement("meta"))
                .flatMap(afterHead(_, pctx))
            else
              next(ctx, opens, chunkAcc += HtmlEvent.StartElement("meta", attrs, selfClosing))
                .flatMap(afterHead(_, pctx))
          case HtmlToken.OpenTag(name @ ("noframes" | "style"), attrs, selfClosing) =>
            rawText(ctx, name :: opens, chunkAcc += HtmlEvent.StartElement(name, attrs, selfClosing))
              .flatMap(text(_, pctx, afterHead))
          case HtmlToken.OpenTag("script", attrs, selfClosing) =>
            scriptData(ctx, "script" :: opens, chunkAcc += HtmlEvent.StartElement("script", attrs, selfClosing))
              .flatMap(text(_, pctx, afterHead))
          case HtmlToken.OpenTag("template", attrs, selfClosing) =>
            next(ctx, "template" :: opens, chunkAcc += HtmlEvent.StartElement("template", attrs, selfClosing))
              .flatMap(inTemplate(_, pctx))
          case HtmlToken.OpenTag("title", attrs, selfClosing) =>
            rcdata(ctx, "title" :: opens, chunkAcc += HtmlEvent.StartElement("title", attrs, selfClosing))
              .flatMap(text(_, pctx, afterHead))
          case HtmlToken.EndTag("template") =>
            val (toClose, rest) = opens.span(_ != "template")
            rest match {
              case "template" :: rest =>
                next(ctx, rest, chunkAcc ++= toClose.map(HtmlEvent.EndElement(_))).flatMap(afterHead(_, pctx))
              case _ =>
                // just ignore it
                next(ctx, opens, chunkAcc).flatMap(afterHead(_, pctx))
            }
          case HtmlToken.EndTag("body" | "html" | "br") =>
            inBody(pulled
                     .pushOpen("body")
                     .copy(chunkAcc = chunkAcc += HtmlEvent.StartElement("body", Map.empty, false)),
                   pctx)
          case HtmlToken.OpenTag("head", _, _) =>
            // just ignore it
            next(ctx, opens, chunkAcc).flatMap(afterHead(_, pctx))
          case HtmlToken.EndTag(_) =>
            // just ignore it
            next(ctx, opens, chunkAcc).flatMap(afterHead(_, pctx))
          case _ =>
            inBody(pulled
                     .pushOpen("body")
                     .copy(chunkAcc = chunkAcc += HtmlEvent.StartElement("body", Map.empty, false)),
                   pctx)
        }
      case _ =>
        inBody(pulled
                 .pushOpen("body")
                 .copy(chunkAcc = pulled.chunkAcc += HtmlEvent.StartElement("body", Map.empty, false)),
               pctx)
    }

  // https://html.spec.whatwg.org/#parsing-main-inbody
  def inBody(pulled: Pulled[F, Ctx, HtmlEvent], pctx: PCtx): Pull[F, HtmlEvent, Unit] =
    pulled match {
      case Pulled(ctx, chunkAcc, opens, Some(token), next) =>
        token match {
          case HtmlToken.Character('\u0000') =>
            // ignore it
            next(ctx, opens, chunkAcc).flatMap(inBody(_, pctx))
          case HtmlToken.Character(c @ ('\t' | '\r' | '\n' | '\f' | ' ')) =>
            next(ctx, opens, chunkAcc += HtmlEvent.Character(c)).flatMap(inBody(_, pctx))
          case HtmlToken.Character(c) =>
            next(ctx, opens, chunkAcc += HtmlEvent.Character(c)).flatMap(inBody(_, pctx.copy(framesetOk = false)))
          case HtmlToken.Comment(content) =>
            next(ctx, opens, chunkAcc += HtmlEvent.Comment(content)).flatMap(inBody(_, pctx))
          case HtmlToken.Doctype(_, _, _, _) =>
            // ignore it
            next(ctx, opens, chunkAcc).flatMap(inBody(_, pctx))
          case HtmlToken.OpenTag("html", attrs, selfClosing) =>
            // [SPEC DEVIATION]
            // don't add new attributes
            // just ignore it in any case
            next(ctx, opens, chunkAcc).flatMap(inBody(_, pctx))
          case HtmlToken.OpenTag(name @ ("base" | "basefront" | "bgsound" | "link"), attrs, selfClosing) =>
            if (acknowledgeSelfClosing(selfClosing, name))
              next(ctx,
                   opens,
                   chunkAcc += HtmlEvent.StartElement(name, attrs, selfClosing) += HtmlEvent.EndElement(name))
                .flatMap(inBody(_, pctx))
            else
              next(ctx, opens, chunkAcc += HtmlEvent.StartElement(name, attrs, selfClosing))
                .flatMap(inBody(_, pctx))
          case HtmlToken.OpenTag("meta", attrs, selfClosing) =>
            // [SPEC DEVIATION]
            // no change in charset possible
            if (acknowledgeSelfClosing(selfClosing, "meta"))
              next(ctx,
                   opens,
                   chunkAcc += HtmlEvent.StartElement("meta", attrs, selfClosing) += HtmlEvent.EndElement("meta"))
                .flatMap(inBody(_, pctx))
            else
              next(ctx, opens, chunkAcc += HtmlEvent.StartElement("meta", attrs, selfClosing))
                .flatMap(inBody(_, pctx))
          case HtmlToken.OpenTag(name @ ("noframes" | "style"), attrs, selfClosing) =>
            rawText(ctx, name :: opens, chunkAcc += HtmlEvent.StartElement(name, attrs, selfClosing))
              .flatMap(text(_, pctx, inBody))
          case HtmlToken.OpenTag("script", attrs, selfClosing) =>
            scriptData(ctx, "script" :: opens, chunkAcc += HtmlEvent.StartElement("script", attrs, selfClosing))
              .flatMap(text(_, pctx, inBody))
          case HtmlToken.OpenTag("template", attrs, selfClosing) =>
            next(ctx, "template" :: opens, chunkAcc += HtmlEvent.StartElement("template", attrs, selfClosing))
              .flatMap(inTemplate(_, pctx))
          case HtmlToken.OpenTag("title", attrs, selfClosing) =>
            rcdata(ctx, "title" :: opens, chunkAcc += HtmlEvent.StartElement("title", attrs, selfClosing))
              .flatMap(text(_, pctx, inBody))
          case HtmlToken.EndTag("template") =>
            val (toClose, rest) = opens.span(_ != "template")
            rest match {
              case "template" :: rest =>
                next(ctx, rest, chunkAcc ++= toClose.map(HtmlEvent.EndElement(_))).flatMap(inBody(_, pctx))
              case _ =>
                // just ignore it
                next(ctx, opens, chunkAcc).flatMap(inBody(_, pctx))
            }
          case HtmlToken.OpenTag("body", _, _) =>
            // [SPEC DEVIATION]
            // ignore it
            next(ctx, opens, chunkAcc).flatMap(inBody(_, pctx.copy(framesetOk = false)))
          case HtmlToken.OpenTag("frameset", attrs, selfClosing) =>
            if (pctx.framesetOk) {
              next(ctx, opens, chunkAcc += HtmlEvent.StartElement("frameset", attrs, selfClosing))
                .flatMap(inFrameset(_, pctx))
            } else {
              // ignore it
              next(ctx, opens, chunkAcc).flatMap(inBody(_, pctx))
            }
        }
      case _ =>
        ???
    }

  // https://html.spec.whatwg.org/#parsing-main-intemplate
  def inTemplate(pulled: Pulled[F, Ctx, HtmlEvent], pctx: PCtx): Pull[F, HtmlEvent, Unit] =
    ???

  // https://html.spec.whatwg.org/#parsing-main-incdata
  def text(
      pulled: Pulled[F, Ctx, HtmlEvent],
      pctx: PCtx,
      originalInsertionMode: (Pulled[F, Ctx, HtmlEvent], PCtx) => Pull[F, HtmlEvent, Unit]): Pull[F, HtmlEvent, Unit] =
    pulled match {
      case Pulled(ctx, chunkAcc, opens, Some(token), next) =>
        token match {
          case HtmlToken.Character(c) =>
            next(ctx, opens, chunkAcc += HtmlEvent.Character(c)).flatMap(text(_, pctx, originalInsertionMode))
          case HtmlToken.EndTag(name) =>
            val opens1 = pop(opens, name)
            next(ctx, opens1, chunkAcc += HtmlEvent.EndElement(name)).flatMap(originalInsertionMode(_, pctx))
          case _ =>
            // this should not happen, skip the token
            next(ctx, opens, chunkAcc).flatMap(originalInsertionMode(_, pctx))
        }
      case _ =>
        val opens1 = pop(pulled.opens)
        originalInsertionMode(pulled.copy(opens = opens1), pctx)
    }

  // https://html.spec.whatwg.org/#parsing-main-inframeset
  def inFrameset(pulled: Pulled[F, Ctx, HtmlEvent], pctx: PCtx): Pull[F, HtmlEvent, Unit] =
    ???

}
