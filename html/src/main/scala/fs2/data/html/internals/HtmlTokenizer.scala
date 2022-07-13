package fs2
package data
package html
package internals

import text.CharLikeChunks

import cats.syntax.all._

import scala.annotation.switch
import scala.collection.mutable.ListBuffer
import fs2.data.html.utils.RadixNode
import cats.data.NonEmptyList

private case class CurrentTag(open: Boolean, name: StringBuilder)

private[html] class HtmlTokenizer[F[_], T](implicit F: RaiseThrowable[F], T: CharLikeChunks[F, T]) {

  def ensureNext[Res](ctx: T.Context, chunkAcc: ListBuffer[HtmlToken])(
      kont: Option[(T.Context, ListBuffer[HtmlToken])] => Pull[F, HtmlToken, Res]): Pull[F, HtmlToken, Res] =
    if (T.needsPull(ctx)) {
      Pull.output(Chunk.seq(chunkAcc.result())) >> T.pullNext(ctx).flatMap {
        case Some(ctx) =>
          chunkAcc.clear()
          ensureNext(ctx, chunkAcc)(kont)
        case None =>
          kont(none)
      }
    } else {
      Pull.suspend(kont((ctx, chunkAcc).some))
    }

  def ensureNextOrDone(ctx: T.Context, chunkAcc: ListBuffer[HtmlToken])(
      kont: (T.Context, ListBuffer[HtmlToken]) => Pull[F, HtmlToken, Unit]): Pull[F, HtmlToken, Unit] =
    ensureNext(ctx, chunkAcc) {
      case Some((ctx, chunkAcc)) => kont(ctx, chunkAcc)
      case None                  => Pull.done
    }

  private def readFromRadixTree(ctx: T.Context, chunkAcc: ListBuffer[HtmlToken], tree: RadixNode) =
    Pull
      .loopEither[F,
                  HtmlToken,
                  (T.Context, ListBuffer[HtmlToken], StringBuilder),
                  (T.Context, ListBuffer[HtmlToken], String)] { case (ctx, chunkAcc, acc) =>
        ensureNext(ctx, chunkAcc) {
          case Some((ctx, chunkAcc)) =>
            val current = acc.result()
            val c = T.current(ctx)
            if (tree.isPrefix(current + c))
              Pull.pure((T.advance(ctx), chunkAcc, acc.addOne(c)).asLeft)
            else
              Pull.pure((ctx, chunkAcc, acc.result()).asRight)
          case None =>
            Pull.pure((T.emptyContext, new ListBuffer[HtmlToken], acc.result()).asRight)
        }
      }(ctx, chunkAcc, new StringBuilder)

  def characterReference(ctx: T.Context,
                         continue: (T.Context, ListBuffer[HtmlToken], String) => Pull[F, HtmlToken, Unit],
                         chunkAcc: ListBuffer[HtmlToken]): Pull[F, HtmlToken, Unit] =
    ensureNextOrDone(ctx, chunkAcc) { (ctx, chunkAcc) =>
      (T.current(ctx): @switch) match {
        case '#' =>
          numericCharacterReference(T.advance(ctx), new StringBuilder("&#"), continue, chunkAcc)
        case c =>
          if (isAsciiAlpha(c))
            namedCharacterReference(ctx, new StringBuilder("&"), continue, chunkAcc)
          else {
            continue(ctx, chunkAcc, "&")
          }
      }
    }

  def namedCharacterReference(ctx: T.Context,
                              buffer: StringBuilder,
                              continue: (T.Context, ListBuffer[HtmlToken], String) => Pull[F, HtmlToken, Unit],
                              chunkAcc: ListBuffer[HtmlToken]): Pull[F, HtmlToken, Unit] =
    Pull
      .loopEither[F,
                  HtmlToken,
                  (T.Context, ListBuffer[HtmlToken], StringBuilder),
                  (T.Context, ListBuffer[HtmlToken], StringBuilder)] { case (ctx, chunkAcc, buffer) =>
        ensureNext(ctx, chunkAcc) {
          case Some((ctx, chunkAcc)) =>
            val current = buffer.result()
            val c = T.current(ctx)
            if (Entities.isPrefix(current + c))
              Pull.pure((T.advance(ctx), chunkAcc, buffer.addOne(c)).asLeft)
            else
              Pull.pure((ctx, chunkAcc, buffer).asRight)
          case None =>
            Pull.pure((T.emptyContext, new ListBuffer[HtmlToken], buffer).asRight)
        }
      }((ctx, chunkAcc, buffer))
      .flatMap { case (ctx, chunkAcc, buffer) =>
        val name = buffer.result()
        Entities.all.get(name) match {
          case Some(value) =>
            continue(ctx, chunkAcc, value)
          case None =>
            ambiguousAmpersandState(ctx, buffer, continue, chunkAcc)
        }
      }

  def ambiguousAmpersandState(ctx: T.Context,
                              buffer: StringBuilder,
                              continue: (T.Context, ListBuffer[HtmlToken], String) => Pull[F, HtmlToken, Unit],
                              chunkAcc: ListBuffer[HtmlToken]): Pull[F, HtmlToken, Unit] =
    ensureNext(ctx, chunkAcc) {
      case Some((ctx, chunkAcc)) =>
        (T.current(ctx): @switch) match {
          case ';' =>
            continue(ctx, chunkAcc, buffer.result())
          case c =>
            if (isAsciiAlpha(c))
              ambiguousAmpersandState(T.advance(ctx), buffer.addOne(c), continue, chunkAcc)
            else
              continue(ctx, chunkAcc, buffer.result())
        }
      case None =>
        continue(ctx, chunkAcc, buffer.result())
    }

  def numericCharacterReference(ctx: T.Context,
                                buffer: StringBuilder,
                                continue: (T.Context, ListBuffer[HtmlToken], String) => Pull[F, HtmlToken, Unit],
                                chunkAcc: ListBuffer[HtmlToken]): Pull[F, HtmlToken, Unit] =
    ensureNext(ctx, chunkAcc) {
      case Some((ctx, chunkAcc)) =>
        val c = T.current(ctx)
        (c: @switch) match {
          case 'x' | 'X' =>
            hexadecimalCharacterReferenceStart(T.advance(ctx), buffer.addOne(c), 0, continue, chunkAcc)
          case _ =>
            decimalCharacterReferenceStart(ctx, buffer, 0, continue, chunkAcc)
        }
      case None =>
        decimalCharacterReferenceStart(T.emptyContext, buffer, 0, continue, new ListBuffer)
    }

  def hexadecimalCharacterReferenceStart(
      ctx: T.Context,
      buffer: StringBuilder,
      value: Int,
      continue: (T.Context, ListBuffer[HtmlToken], String) => Pull[F, HtmlToken, Unit],
      chunkAcc: ListBuffer[HtmlToken]): Pull[F, HtmlToken, Unit] =
    ensureNext(ctx, chunkAcc) {
      case Some((ctx, chunkAcc)) =>
        if (isAsciiHex(T.current(ctx)))
          hexadecimalCharacterReference(ctx, value, continue, chunkAcc)
        else
          continue(T.emptyContext, new ListBuffer, buffer.result())
      case None =>
        continue(T.emptyContext, new ListBuffer, buffer.result())
    }

  def decimalCharacterReferenceStart(ctx: T.Context,
                                     buffer: StringBuilder,
                                     value: Int,
                                     continue: (T.Context, ListBuffer[HtmlToken], String) => Pull[F, HtmlToken, Unit],
                                     chunkAcc: ListBuffer[HtmlToken]): Pull[F, HtmlToken, Unit] =
    ensureNext(ctx, chunkAcc) {
      case Some((ctx, chunkAcc)) =>
        if (isAsciiDigit(T.current(ctx)))
          decimalCharacterReference(ctx, value, continue, chunkAcc)
        else
          continue(T.emptyContext, new ListBuffer, buffer.result())
      case None =>
        continue(T.emptyContext, new ListBuffer, buffer.result())
    }

  def hexadecimalCharacterReference(ctx: T.Context,
                                    value: Int,
                                    continue: (T.Context, ListBuffer[HtmlToken], String) => Pull[F, HtmlToken, Unit],
                                    chunkAcc: ListBuffer[HtmlToken]): Pull[F, HtmlToken, Unit] =
    ensureNext(ctx, chunkAcc) {
      case Some((ctx, chunkAcc)) =>
        val c = T.current(ctx)
        (c: @switch) match {
          case ';' =>
            numericCharacterReferenceEnd(T.advance(ctx), value, continue, chunkAcc)
          case _ =>
            val idx = hexdigits.indexOf(c.toLower)
            if (idx >= 0)
              hexadecimalCharacterReference(T.advance(ctx), (value << 4) | (0xff & idx), continue, chunkAcc)
            else
              numericCharacterReferenceEnd(ctx, value, continue, chunkAcc)
        }
      case None =>
        numericCharacterReferenceEnd(T.emptyContext, value, continue, new ListBuffer)
    }

  def decimalCharacterReference(ctx: T.Context,
                                value: Int,
                                continue: (T.Context, ListBuffer[HtmlToken], String) => Pull[F, HtmlToken, Unit],
                                chunkAcc: ListBuffer[HtmlToken]): Pull[F, HtmlToken, Unit] =
    ensureNext(ctx, chunkAcc) {
      case Some((ctx, chunkAcc)) =>
        val c = T.current(ctx)
        (c: @switch) match {
          case ';' =>
            numericCharacterReferenceEnd(T.advance(ctx), value, continue, chunkAcc)
          case _ =>
            val idx = digits.indexOf(c.toLower)
            if (idx >= 0)
              decimalCharacterReference(T.advance(ctx), (value * 10) | (0xff & idx), continue, chunkAcc)
            else
              numericCharacterReferenceEnd(ctx, value, continue, chunkAcc)
        }
      case None =>
        numericCharacterReferenceEnd(T.emptyContext, value, continue, new ListBuffer)
    }

  def numericCharacterReferenceEnd(ctx: T.Context,
                                   value: Int,
                                   continue: (T.Context, ListBuffer[HtmlToken], String) => Pull[F, HtmlToken, Unit],
                                   chunkAcc: ListBuffer[HtmlToken]): Pull[F, HtmlToken, Unit] =
    ensureNext(ctx, chunkAcc) { in =>
      val value1 =
        if (value == 0x00) 0xfffd
        else if (value > 0x10ffff) 0xfffd
        else if (value >= 0xd800 && value <= 0xdfff) 0xfffd
        else if (isNonCharacter(value)) value
        else if (isControl(value) && !wspace.contains(value)) value
        else codes.getOrElse(value, value)
      in match {
        case Some((ctx, chunkAcc)) =>
          continue(ctx, chunkAcc, Character.toString(value1))
        case None =>
          continue(T.emptyContext, new ListBuffer, Character.toString(value1))
      }
    }

  def tagOpen(ctx: T.Context, opens: List[String], chunkAcc: ListBuffer[HtmlToken]): Pull[F, HtmlToken, Unit] =
    ensureNext(ctx, chunkAcc) {
      case Some((ctx, chunkAcc)) =>
        (T.current(ctx): @switch) match {
          case '!' =>
            markupDeclarationOpen(T.advance(ctx), opens, chunkAcc)
          case '/' =>
            endTagOpen(T.advance(ctx), opens, chunkAcc)
          case '?' =>
            bogusComment(ctx, new StringBuilder, opens, chunkAcc)
          case c =>
            if (isAsciiAlpha(c))
              tagName(ctx, true, new StringBuilder, opens, chunkAcc)
            else
              data(ctx, opens, chunkAcc += HtmlToken.Character('<'))
        }
      case None =>
        Pull.output1(HtmlToken.Character('<'))
    }

  def endTagOpen(ctx: T.Context, opens: List[String], chunkAcc: ListBuffer[HtmlToken]): Pull[F, HtmlToken, Unit] =
    ensureNext(ctx, chunkAcc) {
      case Some((ctx, chunkAcc)) =>
        (T.current(ctx): @switch) match {
          case '>' =>
            data(T.advance(ctx), opens, chunkAcc)
          case c =>
            if (isAsciiAlpha(c))
              tagName(ctx, false, new StringBuilder, opens, chunkAcc)
            else
              bogusComment(ctx, new StringBuilder, opens, chunkAcc)
        }
      case None =>
        Pull.output(Chunk(HtmlToken.Character('<'), HtmlToken.Character('/')))
    }

  def bogusComment(ctx: T.Context,
                   content: StringBuilder,
                   opens: List[String],
                   chunkAcc: ListBuffer[HtmlToken]): Pull[F, HtmlToken, Unit] =
    ensureNextOrDone(ctx, chunkAcc) { (ctx, chunkAcc) =>
      (T.current(ctx): @switch) match {
        case '>' =>
          data(T.advance(ctx), opens, chunkAcc += HtmlToken.Comment(content.result()))
        case '\u0000' =>
          bogusComment(T.advance(ctx), content.addOne('\ufffd'), opens, chunkAcc)
        case c =>
          bogusComment(T.advance(ctx), content.addOne(c), opens, chunkAcc)
      }
    }

  def tagName(ctx: T.Context,
              open: Boolean,
              name: StringBuilder,
              opens: List[String],
              chunkAcc: ListBuffer[HtmlToken]): Pull[F, HtmlToken, Unit] =
    ensureNextOrDone(ctx, chunkAcc) { (ctx, chunkAcc) =>
      (T.current(ctx): @switch) match {
        case '\t' | '\r' | '\n' | ' ' =>
          beforeAttributeName(T.advance(ctx), open, name.result(), Map.empty, opens, chunkAcc)
        case '/' =>
          selfClosingStartTag(T.advance(ctx), open, name.result(), Map.empty, opens, chunkAcc)
        case '>' =>
          val n = name.result()
          if (open)
            data(T.advance(ctx), n :: opens, chunkAcc += HtmlToken.OpenTag(n, Map.empty))
          else
            data(T.advance(ctx), opens, chunkAcc += HtmlToken.EndTag(n))
        case '\u0000' =>
          tagName(T.advance(ctx), open, name.addOne('\ufffd'), opens, chunkAcc)
        case c =>
          if (isAsciiAlpha(c))
            tagName(T.advance(ctx), open, name.addOne(c.toLower), opens, chunkAcc)
          else
            tagName(T.advance(ctx), open, name.addOne(c), opens, chunkAcc)

      }
    }

  def beforeAttributeName(ctx: T.Context,
                          open: Boolean,
                          name: String,
                          attributes: Map[String, String],
                          opens: List[String],
                          chunkAcc: ListBuffer[HtmlToken]): Pull[F, HtmlToken, Unit] =
    ensureNext(ctx, chunkAcc) {
      case Some((ctx, chunkAcc)) =>
        (T.current(ctx): @switch) match {
          case '\t' | '\r' | '\n' | ' ' =>
            beforeAttributeName(T.advance(ctx), open, name, attributes, opens, chunkAcc)
          case '/' | '>' =>
            afterAttributeName(ctx, open, name, "", attributes, opens, chunkAcc)
          case '=' =>
            attributeName(T.advance(ctx), open, name, new StringBuilder("="), attributes, opens, chunkAcc)
          case _ =>
            attributeName(ctx, open, name, new StringBuilder, attributes, opens, chunkAcc)
        }
      case None =>
        afterAttributeName(T.emptyContext, open, name, "", attributes, opens, new ListBuffer)
    }

  def attributeName(ctx: T.Context,
                    open: Boolean,
                    name: String,
                    attr: StringBuilder,
                    attributes: Map[String, String],
                    opens: List[String],
                    chunkAcc: ListBuffer[HtmlToken]): Pull[F, HtmlToken, Unit] =
    ensureNext(ctx, chunkAcc) {
      case Some((ctx, chunkAcc)) =>
        (T.current(ctx): @switch) match {
          case '\t' | '\r' | '\n' | ' ' | '/' | '>' =>
            afterAttributeName(T.advance(ctx), open, name, attr.result(), attributes, opens, chunkAcc)
          case '=' =>
            beforeAttributeValue(T.advance(ctx), open, name, attr.result(), attributes, opens, chunkAcc)
          case '\u0000' =>
            attributeName(T.advance(ctx), open, name, attr.addOne('\ufffd'), attributes, opens, chunkAcc)
          case c =>
            attributeName(T.advance(ctx), open, name, attr.addOne(c), attributes, opens, chunkAcc)
        }
      case None =>
        afterAttributeName(T.emptyContext, open, name, attr.result(), attributes, opens, new ListBuffer)
    }

  def afterAttributeName(ctx: T.Context,
                         open: Boolean,
                         name: String,
                         attr: String,
                         attributes: Map[String, String],
                         opens: List[String],
                         chunkAcc: ListBuffer[HtmlToken]): Pull[F, HtmlToken, Unit] =
    ensureNextOrDone(ctx, chunkAcc) { (ctx, chunkAcc) =>
      (T.current(ctx): @switch) match {
        case '\t' | '\r' | '\n' | ' ' =>
          afterAttributeName(T.advance(ctx), open, name, attr, attributes, opens, chunkAcc)
        case '/' =>
          val attributes1 =
            if (attr.isEmpty || attributes.contains(attr))
              attributes
            else
              attributes.updated(attr, "")
          selfClosingStartTag(T.advance(ctx), open, name, attributes1, opens, chunkAcc)
        case '=' =>
          beforeAttributeValue(T.advance(ctx), open, name, attr, attributes, opens, chunkAcc)
        case '>' =>
          val attributes1 =
            if (attr.isEmpty || attributes.contains(attr))
              attributes
            else
              attributes.updated(attr, "")
          if (open)
            data(T.advance(ctx), name :: opens, chunkAcc += HtmlToken.OpenTag(name, attributes1))
          else
            data(T.advance(ctx), opens, chunkAcc += HtmlToken.EndTag(name))
        case _ =>
          val attributes1 =
            if (attr.isEmpty || attributes.contains(attr))
              attributes
            else
              attributes.updated(attr, "")
          attributeName(ctx, open, name, new StringBuilder, attributes1, opens, chunkAcc)
      }
    }

  def beforeAttributeValue(ctx: T.Context,
                           open: Boolean,
                           name: String,
                           attr: String,
                           attributes: Map[String, String],
                           opens: List[String],
                           chunkAcc: ListBuffer[HtmlToken]): Pull[F, HtmlToken, Unit] =
    ensureNextOrDone(ctx, chunkAcc) { (ctx, chunkAcc) =>
      (T.current(ctx): @switch) match {
        case '\t' | '\r' | '\n' | ' ' =>
          beforeAttributeValue(T.advance(ctx), open, name, attr, attributes, opens, chunkAcc)
        case '"' =>
          attributeValueDoubleQuoted(T.advance(ctx), open, name, attr, new StringBuilder, attributes, opens, chunkAcc)
        case '\'' =>
          attributeValueSingleQuoted(T.advance(ctx), open, name, attr, new StringBuilder, attributes, opens, chunkAcc)
        case '>' =>
          val attributes1 =
            if (attr.isEmpty || attributes.contains(attr))
              attributes
            else
              attributes.updated(attr, "")
          if (open)
            data(T.advance(ctx), name :: opens, chunkAcc += HtmlToken.OpenTag(name, attributes1))
          else
            data(T.advance(ctx), opens, chunkAcc += HtmlToken.EndTag(name))
        case _ =>
          attributeValueUnquoted(ctx, open, name, attr, new StringBuilder, attributes, opens, chunkAcc)
      }
    }

  def attributeValueDoubleQuoted(ctx: T.Context,
                                 open: Boolean,
                                 name: String,
                                 attr: String,
                                 value: StringBuilder,
                                 attributes: Map[String, String],
                                 opens: List[String],
                                 chunkAcc: ListBuffer[HtmlToken]): Pull[F, HtmlToken, Unit] =
    ensureNextOrDone(ctx, chunkAcc) { (ctx, chunkAcc) =>
      (T.current(ctx): @switch) match {
        case '"' =>
          val attributes1 =
            if (attributes.contains(attr))
              attributes
            else
              attributes.updated(attr, value.result())
          afterAttributeValueQuoted(T.advance(ctx), open, name, attributes1, opens, chunkAcc)
        case '&' =>
          def kont(ctx: T.Context, chunkAcc: ListBuffer[HtmlToken], toFlush: String) =
            attributeValueDoubleQuoted(ctx, open, name, attr, value.addAll(toFlush), attributes, opens, chunkAcc)

          characterReference(T.advance(ctx), kont, chunkAcc)
        case '\u0000' =>
          attributeValueDoubleQuoted(T.advance(ctx),
                                     open,
                                     name,
                                     attr,
                                     value.addOne('\ufffd'),
                                     attributes,
                                     opens,
                                     chunkAcc)
        case c =>
          attributeValueDoubleQuoted(T.advance(ctx), open, name, attr, value.addOne(c), attributes, opens, chunkAcc)
      }
    }

  def attributeValueSingleQuoted(ctx: T.Context,
                                 open: Boolean,
                                 name: String,
                                 attr: String,
                                 value: StringBuilder,
                                 attributes: Map[String, String],
                                 opens: List[String],
                                 chunkAcc: ListBuffer[HtmlToken]): Pull[F, HtmlToken, Unit] =
    ensureNextOrDone(ctx, chunkAcc) { (ctx, chunkAcc) =>
      (T.current(ctx): @switch) match {
        case '\'' =>
          val attributes1 =
            if (attributes.contains(attr))
              attributes
            else
              attributes.updated(attr, value.result())
          afterAttributeValueQuoted(T.advance(ctx), open, name, attributes1, opens, chunkAcc)
        case '&' =>
          def kont(ctx: T.Context, chunkAcc: ListBuffer[HtmlToken], toFlush: String) =
            attributeValueSingleQuoted(ctx, open, name, attr, value.addAll(toFlush), attributes, opens, chunkAcc)

          characterReference(T.advance(ctx), kont, chunkAcc)
        case '\u0000' =>
          attributeValueSingleQuoted(T.advance(ctx),
                                     open,
                                     name,
                                     attr,
                                     value.addOne('\ufffd'),
                                     attributes,
                                     opens,
                                     chunkAcc)
        case c =>
          attributeValueSingleQuoted(T.advance(ctx), open, name, attr, value.addOne(c), attributes, opens, chunkAcc)
      }
    }

  def attributeValueUnquoted(ctx: T.Context,
                             open: Boolean,
                             name: String,
                             attr: String,
                             value: StringBuilder,
                             attributes: Map[String, String],
                             opens: List[String],
                             chunkAcc: ListBuffer[HtmlToken]): Pull[F, HtmlToken, Unit] =
    ensureNextOrDone(ctx, chunkAcc) { (ctx, chunkAcc) =>
      (T.current(ctx): @switch) match {
        case '\t' | '\r' | '\n' | ' ' =>
          val attributes1 =
            if (attributes.contains(attr))
              attributes
            else
              attributes.updated(attr, value.result())
          beforeAttributeName(T.advance(ctx), open, name, attributes1, opens, chunkAcc)
        case '&' =>
          def kont(ctx: T.Context, chunkAcc: ListBuffer[HtmlToken], toFlush: String) =
            attributeValueUnquoted(ctx, open, name, attr, value.addAll(toFlush), attributes, opens, chunkAcc)

          characterReference(T.advance(ctx), kont, chunkAcc)
        case '>' =>
          val attributes1 =
            if (attributes.contains(attr))
              attributes
            else
              attributes.updated(attr, value.result())
          if (open)
            data(T.advance(ctx), name :: opens, chunkAcc += HtmlToken.OpenTag(name, attributes1))
          else
            data(T.advance(ctx), opens, chunkAcc += HtmlToken.EndTag(name))
        case '\u0000' =>
          attributeValueDoubleQuoted(T.advance(ctx),
                                     open,
                                     name,
                                     attr,
                                     value.addOne('\ufffd'),
                                     attributes,
                                     opens,
                                     chunkAcc)
        case c =>
          attributeValueUnquoted(T.advance(ctx), open, name, attr, value.addOne(c), attributes, opens, chunkAcc)
      }
    }

  def afterAttributeValueQuoted(ctx: T.Context,
                                open: Boolean,
                                name: String,
                                attributes: Map[String, String],
                                opens: List[String],
                                chunkAcc: ListBuffer[HtmlToken]): Pull[F, HtmlToken, Unit] =
    ensureNextOrDone(ctx, chunkAcc) { (ctx, chunkAcc) =>
      (T.current(ctx): @switch) match {
        case '\t' | '\r' | '\n' | ' ' =>
          beforeAttributeName(T.advance(ctx), open, name, attributes, opens, chunkAcc)
        case '/' =>
          selfClosingStartTag(T.advance(ctx), open, name, attributes, opens, chunkAcc)
        case '>' =>
          if (open)
            data(T.advance(ctx), name :: opens, chunkAcc += HtmlToken.OpenTag(name, attributes))
          else
            data(T.advance(ctx), opens, chunkAcc += HtmlToken.EndTag(name))
        case _ =>
          beforeAttributeName(ctx, open, name, attributes, opens, chunkAcc)
      }
    }

  def selfClosingStartTag(ctx: T.Context,
                          open: Boolean,
                          name: String,
                          attributes: Map[String, String],
                          opens: List[String],
                          chunkAcc: ListBuffer[HtmlToken]): Pull[F, HtmlToken, Unit] =
    ensureNextOrDone(ctx, chunkAcc) { (ctx, chunkAcc) =>
      (T.current(ctx): @switch) match {
        case '>' =>
          if (open)
            data(T.advance(ctx), opens, chunkAcc += HtmlToken.OpenTag(name, attributes, true))
          else
            data(T.advance(ctx), opens, chunkAcc += HtmlToken.EndTag(name))
        case _ =>
          beforeAttributeName(ctx, open, name, attributes, opens, chunkAcc)
      }
    }

  def markupDeclarationOpen(ctx: T.Context,
                            opens: List[String],
                            chunkAcc: ListBuffer[HtmlToken]): Pull[F, HtmlToken, Unit] =
    readFromRadixTree(ctx, chunkAcc, markupDecls)
      .flatMap {
        case (ctx, chunkAcc, "--") =>
          commentStart(ctx, new StringBuilder, opens, chunkAcc)
        case (ctx, chunkAcc, "[CDATA[") =>
          opens match {
            case o :: _ if elements.contains(o) =>
              bogusComment(ctx, new StringBuilder("[CDATA["), opens, chunkAcc)
            case _ =>
              cdataSection(ctx, opens, chunkAcc)
          }
        case (ctx, chunkAcc, name) if name.equalsIgnoreCase("doctype") =>
          doctype(ctx, opens, chunkAcc)
        case (ctx, chunkAcc, pushback) =>
          bogusComment(T.pushback(ctx, pushback), new StringBuilder, opens, chunkAcc)
      }

  def doctype(ctx: T.Context, opens: List[String], chunkAcc: ListBuffer[HtmlToken]): Pull[F, HtmlToken, Unit] =
    ensureNext(ctx, chunkAcc) {
      case Some((ctx, chunkAcc)) =>
        (T.current(ctx): @switch) match {
          case '\t' | '\r' | '\n' | ' ' =>
            beforeDoctypeName(T.advance(ctx), opens, chunkAcc)
          case '>' =>
            beforeDoctypeName(ctx, opens, chunkAcc)
          case _ =>
            doctypeName(ctx, new StringBuilder, opens, chunkAcc)
        }
      case None =>
        Pull.output1(HtmlToken.Doctype(true, "", none, none))
    }

  def beforeDoctypeName(ctx: T.Context,
                        opens: List[String],
                        chunkAcc: ListBuffer[HtmlToken]): Pull[F, HtmlToken, Unit] =
    ensureNext(ctx, chunkAcc) {
      case Some((ctx, chunkAcc)) =>
        (T.current(ctx): @switch) match {
          case '\t' | '\r' | '\n' | ' ' =>
            beforeDoctypeName(T.advance(ctx), opens, chunkAcc)
          case '>' =>
            data(T.advance(ctx), opens, chunkAcc += HtmlToken.Doctype(true, "", none, none))
          case '\u0000' =>
            doctypeName(T.advance(ctx), new StringBuilder("\ufffd"), opens, chunkAcc)
          case c =>
            if (isAsciiUpperAlpha(c))
              doctypeName(T.advance(ctx), new StringBuilder(s"${c.toLower}"), opens, chunkAcc)
            else
              doctypeName(T.advance(ctx), new StringBuilder(s"$c"), opens, chunkAcc)
        }
      case None =>
        Pull.output1(HtmlToken.Doctype(true, "", none, none))
    }

  def doctypeName(ctx: T.Context,
                  name: StringBuilder,
                  opens: List[String],
                  chunkAcc: ListBuffer[HtmlToken]): Pull[F, HtmlToken, Unit] =
    ensureNext(ctx, chunkAcc) {
      case Some((ctx, chunkAcc)) =>
        (T.current(ctx): @switch) match {
          case '\t' | '\r' | '\n' | ' ' =>
            afterDoctypeName(T.advance(ctx), name.result(), opens, chunkAcc)
          case '>' =>
            data(T.advance(ctx), opens, chunkAcc += HtmlToken.Doctype(false, name.result(), none, none))
          case '\u0000' =>
            doctypeName(T.advance(ctx), new StringBuilder("\ufffd"), opens, chunkAcc)
          case c =>
            if (isAsciiUpperAlpha(c))
              doctypeName(T.advance(ctx), new StringBuilder(s"${c.toLower}"), opens, chunkAcc)
            else
              doctypeName(T.advance(ctx), new StringBuilder(s"$c"), opens, chunkAcc)
        }
      case None =>
        Pull.output1(HtmlToken.Doctype(true, name.result(), none, none))
    }

  def afterDoctypeName(ctx: T.Context,
                       name: String,
                       opens: List[String],
                       chunkAcc: ListBuffer[HtmlToken]): Pull[F, HtmlToken, Unit] =
    ensureNext(ctx, chunkAcc) {
      case Some((ctx, chunkAcc)) =>
        (T.current(ctx): @switch) match {
          case '\t' | '\r' | '\n' | ' ' =>
            afterDoctypeName(T.advance(ctx), name, opens, chunkAcc)
          case '>' =>
            data(T.advance(ctx), opens, chunkAcc += HtmlToken.Doctype(false, name, none, none))
          case _ =>
            readFromRadixTree(ctx, chunkAcc, doctypeVisibility).flatMap { case (ctx, chunkAcc, vis) =>
              vis.toUpperCase() match {
                case "PUBLIC" => afterDoctypePublicKeyword(ctx, name, opens, chunkAcc)
                case "SYSTEM" => afterDoctypeSystemKeyword(ctx, name, opens, chunkAcc)
                case pushback => bogusDoctype(T.pushback(ctx, pushback), true, name, none, none, opens, chunkAcc)
              }
            }
        }
      case None =>
        Pull.output1(HtmlToken.Doctype(true, name, none, none))
    }

  def afterDoctypePublicKeyword(ctx: T.Context,
                                name: String,
                                opens: List[String],
                                chunkAcc: ListBuffer[HtmlToken]): Pull[F, HtmlToken, Unit] =
    ensureNext(ctx, chunkAcc) {
      case Some((ctx, chunkAcc)) =>
        (T.current(ctx): @switch) match {
          case '\t' | '\r' | '\n' | ' ' =>
            beforeDoctypePublicIdentifier(T.advance(ctx), name, opens, chunkAcc)
          case '>' =>
            data(T.advance(ctx), opens, chunkAcc += HtmlToken.Doctype(true, name, none, none))
          case '"' =>
            doctypePublicIdentifierDoubleQuoted(T.advance(ctx), name, new StringBuilder, opens, chunkAcc)
          case '\'' =>
            doctypePublicIdentifierSingleQuoted(T.advance(ctx), name, new StringBuilder, opens, chunkAcc)
          case _ =>
            bogusDoctype(ctx, true, name, none, none, opens, chunkAcc)
        }
      case None =>
        Pull.output1(HtmlToken.Doctype(true, name, none, none))
    }

  def beforeDoctypePublicIdentifier(ctx: T.Context,
                                    name: String,
                                    opens: List[String],
                                    chunkAcc: ListBuffer[HtmlToken]): Pull[F, HtmlToken, Unit] =
    ensureNext(ctx, chunkAcc) {
      case Some((ctx, chunkAcc)) =>
        (T.current(ctx): @switch) match {
          case '\t' | '\r' | '\n' | ' ' =>
            beforeDoctypePublicIdentifier(T.advance(ctx), name, opens, chunkAcc)
          case '>' =>
            data(T.advance(ctx), opens, chunkAcc += HtmlToken.Doctype(true, name, none, none))
          case '"' =>
            doctypePublicIdentifierDoubleQuoted(T.advance(ctx), name, new StringBuilder, opens, chunkAcc)
          case '\'' =>
            doctypePublicIdentifierSingleQuoted(T.advance(ctx), name, new StringBuilder, opens, chunkAcc)
          case _ =>
            bogusDoctype(ctx, true, name, none, none, opens, chunkAcc)
        }
      case None =>
        Pull.output1(HtmlToken.Doctype(true, name, none, none))
    }

  def doctypePublicIdentifierDoubleQuoted(ctx: T.Context,
                                          name: String,
                                          id: StringBuilder,
                                          opens: List[String],
                                          chunkAcc: ListBuffer[HtmlToken]): Pull[F, HtmlToken, Unit] =
    ensureNext(ctx, chunkAcc) {
      case Some((ctx, chunkAcc)) =>
        (T.current(ctx): @switch) match {
          case '"' => afterDoctypePublicIdentifier(T.advance(ctx), name, id.result(), opens, chunkAcc)
          case '\u0000' =>
            doctypePublicIdentifierDoubleQuoted(T.advance(ctx), name, id.addOne('\ufffd'), opens, chunkAcc)
          case '>' =>
            data(T.advance(ctx), opens, chunkAcc += HtmlToken.Doctype(true, name, id.result().some, none))
          case c =>
            doctypePublicIdentifierDoubleQuoted(T.advance(ctx), name, id.addOne(c), opens, chunkAcc)
        }
      case None =>
        Pull.output1(HtmlToken.Doctype(true, name, id.result().some, none))
    }

  def doctypePublicIdentifierSingleQuoted(ctx: T.Context,
                                          name: String,
                                          id: StringBuilder,
                                          opens: List[String],
                                          chunkAcc: ListBuffer[HtmlToken]): Pull[F, HtmlToken, Unit] =
    ensureNext(ctx, chunkAcc) {
      case Some((ctx, chunkAcc)) =>
        (T.current(ctx): @switch) match {
          case '\'' => afterDoctypePublicIdentifier(T.advance(ctx), name, id.result(), opens, chunkAcc)
          case '\u0000' =>
            doctypePublicIdentifierSingleQuoted(T.advance(ctx), name, id.addOne('\ufffd'), opens, chunkAcc)
          case '>' =>
            data(T.advance(ctx), opens, chunkAcc += HtmlToken.Doctype(true, name, id.result().some, none))
          case c =>
            doctypePublicIdentifierSingleQuoted(T.advance(ctx), name, id.addOne(c), opens, chunkAcc)
        }
      case None =>
        Pull.output1(HtmlToken.Doctype(true, name, id.result().some, none))
    }

  def afterDoctypePublicIdentifier(ctx: T.Context,
                                   name: String,
                                   id: String,
                                   opens: List[String],
                                   chunkAcc: ListBuffer[HtmlToken]): Pull[F, HtmlToken, Unit] =
    ensureNext(ctx, chunkAcc) {
      case Some((ctx, chunkAcc)) =>
        (T.current(ctx): @switch) match {
          case '\t' | '\r' | '\n' | ' ' =>
            betweenDoctypePublicAndSystemIdentifiers(T.advance(ctx), name, id, opens, chunkAcc)
          case '>' =>
            data(T.advance(ctx), opens, chunkAcc += HtmlToken.Doctype(false, name, id.some, none))
          case '"' =>
            doctypeSystemIdentifierDoubleQuoted(T.advance(ctx), name, id.some, new StringBuilder, opens, chunkAcc)
          case '\'' =>
            doctypeSystemIdentifierSingleQuoted(T.advance(ctx), name, id.some, new StringBuilder, opens, chunkAcc)
          case _ =>
            bogusDoctype(ctx, true, name, id.some, none, opens, chunkAcc)
        }
      case None =>
        Pull.output1(HtmlToken.Doctype(true, name, id.some, none))
    }

  def betweenDoctypePublicAndSystemIdentifiers(ctx: T.Context,
                                               name: String,
                                               id: String,
                                               opens: List[String],
                                               chunkAcc: ListBuffer[HtmlToken]): Pull[F, HtmlToken, Unit] =
    ensureNext(ctx, chunkAcc) {
      case Some((ctx, chunkAcc)) =>
        (T.current(ctx): @switch) match {
          case '\t' | '\r' | '\n' | ' ' =>
            betweenDoctypePublicAndSystemIdentifiers(T.advance(ctx), name, id, opens, chunkAcc)
          case '>' =>
            data(T.advance(ctx), opens, chunkAcc += HtmlToken.Doctype(false, name, id.some, none))
          case '"' =>
            doctypeSystemIdentifierDoubleQuoted(T.advance(ctx), name, id.some, new StringBuilder, opens, chunkAcc)
          case '\'' =>
            doctypeSystemIdentifierSingleQuoted(T.advance(ctx), name, id.some, new StringBuilder, opens, chunkAcc)
          case _ =>
            bogusDoctype(ctx, true, name, id.some, none, opens, chunkAcc)
        }
      case None =>
        Pull.output1(HtmlToken.Doctype(true, name, id.some, none))
    }

  def afterDoctypeSystemKeyword(ctx: T.Context,
                                name: String,
                                opens: List[String],
                                chunkAcc: ListBuffer[HtmlToken]): Pull[F, HtmlToken, Unit] =
    ensureNext(ctx, chunkAcc) {
      case Some((ctx, chunkAcc)) =>
        (T.current(ctx): @switch) match {
          case '\t' | '\r' | '\n' | ' ' =>
            afterDoctypeSystemKeyword(T.advance(ctx), name, opens, chunkAcc)
          case '>' =>
            data(T.advance(ctx), opens, chunkAcc += HtmlToken.Doctype(false, name, none, none))
          case '"' =>
            doctypeSystemIdentifierDoubleQuoted(T.advance(ctx), name, none, new StringBuilder, opens, chunkAcc)
          case '\'' =>
            doctypeSystemIdentifierSingleQuoted(T.advance(ctx), name, none, new StringBuilder, opens, chunkAcc)
          case _ =>
            bogusDoctype(ctx, true, name, none, none, opens, chunkAcc)
        }
      case None =>
        Pull.output1(HtmlToken.Doctype(true, name, none, none))
    }

  def doctypeSystemIdentifierDoubleQuoted(ctx: T.Context,
                                          name: String,
                                          publicid: Option[String],
                                          id: StringBuilder,
                                          opens: List[String],
                                          chunkAcc: ListBuffer[HtmlToken]): Pull[F, HtmlToken, Unit] =
    ensureNext(ctx, chunkAcc) {
      case Some((ctx, chunkAcc)) =>
        (T.current(ctx): @switch) match {
          case '"' =>
            afterDoctypeSystemIdentifier(T.advance(ctx), name, publicid, id.result(), opens, chunkAcc)
          case '\u0000' =>
            doctypeSystemIdentifierDoubleQuoted(T.advance(ctx), name, publicid, id.addOne('\ufffd'), opens, chunkAcc)
          case '>' =>
            data(T.advance(ctx), opens, chunkAcc += HtmlToken.Doctype(true, name, publicid, id.result().some))
          case c =>
            doctypeSystemIdentifierDoubleQuoted(T.advance(ctx), name, publicid, id.addOne(c), opens, chunkAcc)
        }
      case None =>
        Pull.output1(HtmlToken.Doctype(true, name, publicid, id.result().some))
    }

  def doctypeSystemIdentifierSingleQuoted(ctx: T.Context,
                                          name: String,
                                          publicid: Option[String],
                                          id: StringBuilder,
                                          opens: List[String],
                                          chunkAcc: ListBuffer[HtmlToken]): Pull[F, HtmlToken, Unit] =
    ensureNext(ctx, chunkAcc) {
      case Some((ctx, chunkAcc)) =>
        (T.current(ctx): @switch) match {
          case '\'' =>
            afterDoctypeSystemIdentifier(T.advance(ctx), name, publicid, id.result(), opens, chunkAcc)
          case '\u0000' =>
            doctypeSystemIdentifierSingleQuoted(T.advance(ctx), name, publicid, id.addOne('\ufffd'), opens, chunkAcc)
          case '>' =>
            data(T.advance(ctx), opens, chunkAcc += HtmlToken.Doctype(true, name, publicid, id.result().some))
          case c =>
            doctypeSystemIdentifierSingleQuoted(T.advance(ctx), name, publicid, id.addOne(c), opens, chunkAcc)
        }
      case None =>
        Pull.output1(HtmlToken.Doctype(true, name, publicid, id.result().some))
    }

  def afterDoctypeSystemIdentifier(ctx: T.Context,
                                   name: String,
                                   publicid: Option[String],
                                   systemid: String,
                                   opens: List[String],
                                   chunkAcc: ListBuffer[HtmlToken]): Pull[F, HtmlToken, Unit] =
    ensureNext(ctx, chunkAcc) {
      case Some((ctx, chunkAcc)) =>
        (T.current(ctx): @switch) match {
          case '\t' | '\r' | '\n' | ' ' =>
            afterDoctypeSystemIdentifier(T.advance(ctx), name, publicid, systemid, opens, chunkAcc)
          case '>' =>
            data(T.advance(ctx), opens, chunkAcc += HtmlToken.Doctype(false, name, publicid, systemid.some))
          case _ =>
            bogusDoctype(ctx, false, name, publicid, systemid.some, opens, chunkAcc)
        }
      case None =>
        Pull.output1(HtmlToken.Doctype(true, name, publicid, systemid.some))
    }

  def bogusDoctype(ctx: T.Context,
                   forceQuirks: Boolean,
                   name: String,
                   publicid: Option[String],
                   systemid: Option[String],
                   opens: List[String],
                   chunkAcc: ListBuffer[HtmlToken]): Pull[F, HtmlToken, Unit] =
    ensureNext(ctx, chunkAcc) {
      case Some((ctx, chunkAcc)) =>
        (T.current(ctx): @switch) match {
          case '>' =>
            data(T.advance(ctx), opens, chunkAcc += HtmlToken.Doctype(forceQuirks, name, publicid, systemid))
          case _ =>
            bogusDoctype(T.advance(ctx), forceQuirks, name, publicid, systemid, opens, chunkAcc)
        }
      case None =>
        Pull.output1(HtmlToken.Doctype(forceQuirks, name, publicid, systemid))
    }

  def cdataSection(ctx: T.Context, opens: List[String], chunkAcc: ListBuffer[HtmlToken]): Pull[F, HtmlToken, Unit] =
    ensureNextOrDone(ctx, chunkAcc) { (ctx, chunkAcc) =>
      (T.current(ctx): @switch) match {
        case ']' => cdataSectionBracket(T.advance(ctx), opens, chunkAcc)
        case c   => cdataSection(T.advance(ctx), opens, chunkAcc += HtmlToken.Character(c))
      }
    }

  def cdataSectionBracket(ctx: T.Context,
                          opens: List[String],
                          chunkAcc: ListBuffer[HtmlToken]): Pull[F, HtmlToken, Unit] =
    ensureNext(ctx, chunkAcc) {
      case Some((ctx, chunkAcc)) =>
        (T.current(ctx): @switch) match {
          case ']' => cdataSectionEnd(ctx, opens, chunkAcc)
          case _   => cdataSection(ctx, opens, chunkAcc += HtmlToken.Character(']'))
        }
      case None =>
        cdataSection(T.emptyContext, opens, new ListBuffer += HtmlToken.Character(']'))
    }

  def cdataSectionEnd(ctx: T.Context, opens: List[String], chunkAcc: ListBuffer[HtmlToken]): Pull[F, HtmlToken, Unit] =
    ensureNext(ctx, chunkAcc) {
      case Some((ctx, chunkAcc)) =>
        (T.current(ctx): @switch) match {
          case ']' => cdataSectionEnd(T.advance(ctx), opens, chunkAcc += HtmlToken.Character(']'))
          case '>' => data(T.advance(ctx), opens, chunkAcc)
          case _   => cdataSection(ctx, opens, chunkAcc ++= List(HtmlToken.Character(']'), HtmlToken.Character(']')))
        }
      case None =>
        cdataSection(T.emptyContext, opens, new ListBuffer ++= List(HtmlToken.Character(']'), HtmlToken.Character(']')))
    }

  def commentStart(ctx: T.Context,
                   content: StringBuilder,
                   opens: List[String],
                   chunkAcc: ListBuffer[HtmlToken]): Pull[F, HtmlToken, Unit] =
    ensureNextOrDone(ctx, chunkAcc) { (ctx, chunkAcc) =>
      (T.current(ctx): @switch) match {
        case '-' =>
          commentStartDash(T.advance(ctx), content, opens, chunkAcc)
        case '>' =>
          data(T.advance(ctx), opens, chunkAcc += HtmlToken.Comment(content.result()))
        case _ =>
          comment(ctx, content, opens, chunkAcc)
      }
    }

  def commentStartDash(ctx: T.Context,
                       content: StringBuilder,
                       opens: List[String],
                       chunkAcc: ListBuffer[HtmlToken]): Pull[F, HtmlToken, Unit] =
    ensureNextOrDone(ctx, chunkAcc) { (ctx, chunkAcc) =>
      (T.current(ctx): @switch) match {
        case '-' =>
          commentEnd(T.advance(ctx), content, opens, chunkAcc)
        case '>' =>
          data(T.advance(ctx), opens, chunkAcc += HtmlToken.Comment(content.result()))
        case _ =>
          comment(ctx, content.addOne('-'), opens, chunkAcc)
      }
    }

  def comment(ctx: T.Context,
              content: StringBuilder,
              opens: List[String],
              chunkAcc: ListBuffer[HtmlToken]): Pull[F, HtmlToken, Unit] =
    ensureNext(ctx, chunkAcc) {
      case Some((ctx, chunkAcc)) =>
        (T.current(ctx): @switch) match {
          case '<' =>
            commentLessThanSign(T.advance(ctx), content.addOne('<'), opens, chunkAcc)
          case '-' =>
            commentEndDash(T.advance(ctx), content, opens, chunkAcc)
          case '\u0000' =>
            comment(T.advance(ctx), content.addOne('\ufffd'), opens, chunkAcc)
          case c =>
            comment(T.advance(ctx), content.addOne(c), opens, chunkAcc)
        }
      case None =>
        Pull.output1(HtmlToken.Comment(content.result()))
    }

  def commentLessThanSign(ctx: T.Context,
                          content: StringBuilder,
                          opens: List[String],
                          chunkAcc: ListBuffer[HtmlToken]): Pull[F, HtmlToken, Unit] =
    ensureNextOrDone(ctx, chunkAcc) { (ctx, chunkAcc) =>
      (T.current(ctx): @switch) match {
        case '!' =>
          commentLessThanSignBang(T.advance(ctx), content.addOne('!'), opens, chunkAcc)
        case '<' =>
          commentLessThanSign(T.advance(ctx), content.addOne('<'), opens, chunkAcc)
        case _ =>
          comment(ctx, content, opens, chunkAcc)
      }
    }

  def commentLessThanSignBang(ctx: T.Context,
                              content: StringBuilder,
                              opens: List[String],
                              chunkAcc: ListBuffer[HtmlToken]): Pull[F, HtmlToken, Unit] =
    ensureNextOrDone(ctx, chunkAcc) { (ctx, chunkAcc) =>
      (T.current(ctx): @switch) match {
        case '-' =>
          commentLessThanSignBangDash(T.advance(ctx), content, opens, chunkAcc)
        case _ =>
          comment(ctx, content, opens, chunkAcc)
      }
    }

  def commentLessThanSignBangDash(ctx: T.Context,
                                  content: StringBuilder,
                                  opens: List[String],
                                  chunkAcc: ListBuffer[HtmlToken]): Pull[F, HtmlToken, Unit] =
    ensureNextOrDone(ctx, chunkAcc) { (ctx, chunkAcc) =>
      (T.current(ctx): @switch) match {
        case '-' =>
          commentLessThanSignBangDashDash(T.advance(ctx), content, opens, chunkAcc)
        case _ =>
          commentEndDash(ctx, content, opens, chunkAcc)
      }
    }

  def commentLessThanSignBangDashDash(ctx: T.Context,
                                      content: StringBuilder,
                                      opens: List[String],
                                      chunkAcc: ListBuffer[HtmlToken]): Pull[F, HtmlToken, Unit] =
    ensureNext(ctx, chunkAcc) {
      case Some((ctx, chunkAcc)) =>
        (T.current(ctx): @switch) match {
          case '>' =>
            commentEnd(ctx, content, opens, chunkAcc)
          case _ =>
            commentEnd(ctx, content, opens, chunkAcc)
        }
      case None =>
        commentEnd(T.emptyContext, content, opens, new ListBuffer)
    }

  def commentEndDash(ctx: T.Context,
                     content: StringBuilder,
                     opens: List[String],
                     chunkAcc: ListBuffer[HtmlToken]): Pull[F, HtmlToken, Unit] =
    ensureNext(ctx, chunkAcc) {
      case Some((ctx, chunkAcc)) =>
        (T.current(ctx): @switch) match {
          case '-' =>
            commentEnd(T.advance(ctx), content, opens, chunkAcc)
          case _ =>
            comment(ctx, content.addOne('-'), opens, chunkAcc)
        }
      case None =>
        Pull.output1(HtmlToken.Comment(content.result()))
    }

  def commentEnd(ctx: T.Context,
                 content: StringBuilder,
                 opens: List[String],
                 chunkAcc: ListBuffer[HtmlToken]): Pull[F, HtmlToken, Unit] =
    ensureNext(ctx, chunkAcc) {
      case Some((ctx, chunkAcc)) =>
        (T.current(ctx): @switch) match {
          case '>' =>
            data(T.advance(ctx), opens, chunkAcc += HtmlToken.Comment(content.result()))
          case '!' =>
            commentEndBang(T.advance(ctx), content, opens, chunkAcc)
          case '-' =>
            commentEnd(T.advance(ctx), content.addOne('-'), opens, chunkAcc)
          case _ =>
            comment(ctx, content.addAll("--"), opens, chunkAcc)
        }
      case None =>
        Pull.output1(HtmlToken.Comment(content.result()))
    }

  def commentEndBang(ctx: T.Context,
                     content: StringBuilder,
                     opens: List[String],
                     chunkAcc: ListBuffer[HtmlToken]): Pull[F, HtmlToken, Unit] =
    ensureNext(ctx, chunkAcc) {
      case Some((ctx, chunkAcc)) =>
        (T.current(ctx): @switch) match {
          case '-' =>
            commentEndDash(ctx, content.addAll("--!"), opens, chunkAcc)
          case '>' =>
            data(T.advance(ctx), opens, chunkAcc += HtmlToken.Comment(content.result()))
        }
      case None =>
        Pull.output1(HtmlToken.Comment(content.result()))
    }

  def rcdataKont(opens: List[String])(ctx: T.Context, chunkAcc: ListBuffer[HtmlToken], toFlush: String) =
    rcdata(ctx, opens, chunkAcc ++= toFlush.map(HtmlToken.Character(_)))

  def rcdata(ctx: T.Context, opens: List[String], chunkAcc: ListBuffer[HtmlToken]): Pull[F, HtmlToken, Unit] =
    ensureNextOrDone(ctx, chunkAcc) { (ctx, chunkAcc) =>
      (T.current(ctx): @switch) match {
        case '&' =>
          characterReference(T.advance(ctx), rcdataKont(opens), chunkAcc)
        case '<' => rcdataLessThanSign(T.advance(ctx), opens, chunkAcc)
        case '\u0000' =>
          rcdata(T.advance(ctx), opens, chunkAcc += HtmlToken.Character('\ufffd'))
        case c =>
          rcdata(T.advance(ctx), opens, chunkAcc += HtmlToken.Character(c))
      }
    }

  def rawText(ctx: T.Context, opens: List[String], chunkAcc: ListBuffer[HtmlToken]): Pull[F, HtmlToken, Unit] =
    ensureNextOrDone(ctx, chunkAcc) { (ctx, chunkAcc) =>
      (T.current(ctx): @switch) match {
        case '&' => characterReference(T.advance(ctx), rcdataKont(opens), chunkAcc)
        case '<' => rawTextLessThanSign(T.advance(ctx), opens, chunkAcc)
        case '\u0000' =>
          rcdata(T.advance(ctx), opens, chunkAcc += HtmlToken.Character('\ufffd'))
        case c =>
          rcdata(T.advance(ctx), opens, chunkAcc += HtmlToken.Character(c))
      }
    }

  def rcdataLessThanSign(ctx: T.Context,
                         opens: List[String],
                         chunkAcc: ListBuffer[HtmlToken]): Pull[F, HtmlToken, Unit] =
    ensureNextOrDone(ctx, chunkAcc) { (ctx, chunkAcc) =>
      (T.current(ctx): @switch) match {
        case '/' =>
          rcdataEndTagOpen(T.advance(ctx), new StringBuilder, opens, chunkAcc)
        case c =>
          rcdata(ctx, opens, chunkAcc += HtmlToken.Character('<'))
      }
    }

  def rcdataEndTagOpen(ctx: T.Context,
                       buffer: StringBuilder,
                       opens: List[String],
                       chunkAcc: ListBuffer[HtmlToken]): Pull[F, HtmlToken, Unit] =
    ensureNextOrDone(ctx, chunkAcc) { (ctx, chunkAcc) =>
      val c = T.current(ctx)
      if (isAsciiAlpha(c)) {
        rcdataEndTagName(ctx, new StringBuilder, opens, chunkAcc)
      } else {
        rcdata(ctx, opens, chunkAcc ++= List(HtmlToken.Character('<'), HtmlToken.Character('/')))
      }
    }

  def rcdataEndTagName(ctx: T.Context,
                       name: StringBuilder,
                       opens: List[String],
                       chunkAcc: ListBuffer[HtmlToken]): Pull[F, HtmlToken, Unit] =
    ensureNextOrDone(ctx, chunkAcc) { (ctx, chunkAcc) =>
      (T.current(ctx): @switch) match {
        case '\t' | '\r' | '\n' | ' ' =>
          val n = name.result()
          opens match {
            case open :: opens if open == n =>
              beforeAttributeName(T.advance(ctx), false, n, Map.empty, opens, chunkAcc)
            case _ =>
              rcdata(
                ctx,
                opens,
                chunkAcc += HtmlToken.Character('<') += HtmlToken.Character('/') ++= name.map(HtmlToken.Character(_)))
          }
        case '/' =>
          val n = name.result()
          opens match {
            case open :: opens if open == n =>
              selfClosingStartTag(T.advance(ctx), false, n, Map.empty, opens, chunkAcc)
            case _ =>
              rcdata(
                ctx,
                opens,
                chunkAcc += HtmlToken.Character('<') += HtmlToken.Character('/') ++= name.map(HtmlToken.Character(_)))
          }
        case '>' =>
          val n = name.result()
          opens match {
            case open :: opens if open == n =>
              data(T.advance(ctx), opens, chunkAcc += HtmlToken.EndTag(n))
            case _ =>
              rcdata(
                ctx,
                opens,
                chunkAcc += HtmlToken.Character('<') += HtmlToken.Character('/') ++= name.map(HtmlToken.Character(_)))
          }
        case c =>
          if (isAsciiAlpha(c))
            rcdataEndTagName(T.advance(ctx), name.addOne(c.toLower), opens, chunkAcc)
          else
            rcdata(
              ctx,
              opens,
              chunkAcc += HtmlToken.Character('<') += HtmlToken.Character('/') ++= name.map(HtmlToken.Character(_)))
      }
    }

  def rawTextLessThanSign(ctx: T.Context,
                          opens: List[String],
                          chunkAcc: ListBuffer[HtmlToken]): Pull[F, HtmlToken, Unit] =
    ensureNextOrDone(ctx, chunkAcc) { (ctx, chunkAcc) =>
      (T.current(ctx): @switch) match {
        case '/' =>
          rawTextEndTagOpen(T.advance(ctx), opens, chunkAcc)
        case c =>
          rawText(ctx, opens, chunkAcc += HtmlToken.Character('<'))
      }
    }

  def rawTextEndTagOpen(ctx: T.Context,
                        opens: List[String],
                        chunkAcc: ListBuffer[HtmlToken]): Pull[F, HtmlToken, Unit] =
    ensureNextOrDone(ctx, chunkAcc) { (ctx, chunkAcc) =>
      val c = T.current(ctx)
      if (isAsciiAlpha(c))
        rawTextEndTagName(ctx, new StringBuilder, opens, chunkAcc)
      else
        rawText(ctx, opens, chunkAcc ++= List(HtmlToken.Character('<'), HtmlToken.Character('/')))
    }

  def rawTextEndTagName(ctx: T.Context,
                        name: StringBuilder,
                        opens: List[String],
                        chunkAcc: ListBuffer[HtmlToken]): Pull[F, HtmlToken, Unit] =
    ensureNextOrDone(ctx, chunkAcc) { (ctx, chunkAcc) =>
      (T.current(ctx): @switch) match {
        case '\t' | '\r' | '\n' | ' ' =>
          val n = name.result()
          opens match {
            case open :: opens if open == n =>
              beforeAttributeName(T.advance(ctx), false, n, Map.empty, opens, chunkAcc)
            case _ =>
              rawText(
                ctx,
                opens,
                chunkAcc += HtmlToken.Character('<') += HtmlToken.Character('/') ++= name.map(HtmlToken.Character(_)))
          }
        case '/' =>
          val n = name.result()
          opens match {
            case open :: opens if open == n =>
              selfClosingStartTag(T.advance(ctx), false, n, Map.empty, opens, chunkAcc)
            case _ =>
              rawText(
                ctx,
                opens,
                chunkAcc += HtmlToken.Character('<') += HtmlToken.Character('/') ++= name.map(HtmlToken.Character(_)))
          }
        case '>' =>
          val n = name.result()
          opens match {
            case open :: opens if open == n =>
              data(T.advance(ctx), opens, chunkAcc += HtmlToken.EndTag(n))
            case _ =>
              rawText(
                ctx,
                opens,
                chunkAcc += HtmlToken.Character('<') += HtmlToken.Character('/') ++= name.map(HtmlToken.Character(_)))
          }
        case c =>
          if (isAsciiAlpha(c))
            rawTextEndTagName(T.advance(ctx), name.addOne(c.toLower), opens, chunkAcc)
          else
            rawText(
              ctx,
              opens,
              chunkAcc += HtmlToken.Character('<') += HtmlToken.Character('/') ++= name.map(HtmlToken.Character(_)))
      }
    }

  def scriptDataLessThanSign(ctx: T.Context,
                             opens: List[String],
                             chunkAcc: ListBuffer[HtmlToken]): Pull[F, HtmlToken, Unit] =
    ensureNextOrDone(ctx, chunkAcc) { (ctx, chunkAcc) =>
      (T.current(ctx): @switch) match {
        case '/' =>
          scriptDataEndTagOpen(T.advance(ctx), opens, chunkAcc)
        case '!' =>
          scriptDataEscapeStart(T.advance(ctx),
                                opens,
                                chunkAcc ++= List(HtmlToken.Character('<'), HtmlToken.Character('!')))
        case c =>
          scriptData(ctx, opens, chunkAcc += HtmlToken.Character('<'))
      }
    }

  def scriptDataEndTagOpen(ctx: T.Context,
                           opens: List[String],
                           chunkAcc: ListBuffer[HtmlToken]): Pull[F, HtmlToken, Unit] =
    ensureNextOrDone(ctx, chunkAcc) { (ctx, chunkAcc) =>
      val c = T.current(ctx)
      if (isAsciiAlpha(c))
        scriptDataEndTagName(ctx, new StringBuilder, opens, chunkAcc)
      else
        scriptData(ctx, opens, chunkAcc ++= List(HtmlToken.Character('<'), HtmlToken.Character('/')))
    }

  def scriptDataEscapeStart(ctx: T.Context,
                            opens: List[String],
                            chunkAcc: ListBuffer[HtmlToken]): Pull[F, HtmlToken, Unit] =
    ensureNextOrDone(ctx, chunkAcc) { (ctx, chunkAcc) =>
      (T.current(ctx): @switch) match {
        case '-' =>
          scriptDataEscapeStartDash(T.advance(ctx), opens, chunkAcc)
        case _ =>
          scriptData(ctx, opens, chunkAcc)
      }
    }

  def scriptDataEscapeStartDash(ctx: T.Context,
                                opens: List[String],
                                chunkAcc: ListBuffer[HtmlToken]): Pull[F, HtmlToken, Unit] =
    ensureNextOrDone(ctx, chunkAcc) { (ctx, chunkAcc) =>
      (T.current(ctx): @switch) match {
        case '-' =>
          scriptDataEscapeStartDash(T.advance(ctx), opens, chunkAcc += HtmlToken.Character('-'))
        case _ =>
          scriptData(ctx, opens, chunkAcc)
      }
    }

  def scriptDataEscaped(ctx: T.Context,
                        opens: List[String],
                        chunkAcc: ListBuffer[HtmlToken]): Pull[F, HtmlToken, Unit] =
    ensureNextOrDone(ctx, chunkAcc) { (ctx, chunkAcc) =>
      (T.current(ctx): @switch) match {
        case '-' =>
          scriptDataEscapedDash(T.advance(ctx), opens, chunkAcc += HtmlToken.Character('-'))
        case '<' =>
          scriptDataEscapedLessThanSign(T.advance(ctx), opens, chunkAcc)
        case '\u0000' =>
          scriptDataEscaped(T.advance(ctx), opens, chunkAcc += HtmlToken.Character('\ufffd'))
        case c =>
          scriptDataEscaped(T.advance(ctx), opens, chunkAcc += HtmlToken.Character(c))
      }
    }

  def scriptDataEscapedDash(ctx: T.Context,
                            opens: List[String],
                            chunkAcc: ListBuffer[HtmlToken]): Pull[F, HtmlToken, Unit] =
    ensureNextOrDone(ctx, chunkAcc) { (ctx, chunkAcc) =>
      (T.current(ctx): @switch) match {
        case '-' =>
          scriptDataEscapedDashDash(T.advance(ctx), opens, chunkAcc += HtmlToken.Character('-'))
        case '<' =>
          scriptDataEscapedLessThanSign(T.advance(ctx), opens, chunkAcc)
        case '\u0000' =>
          scriptDataEscaped(T.advance(ctx), opens, chunkAcc += HtmlToken.Character('\ufffd'))
        case c =>
          scriptDataEscaped(T.advance(ctx), opens, chunkAcc += HtmlToken.Character(c))
      }
    }

  def scriptDataEscapedDashDash(ctx: T.Context,
                                opens: List[String],
                                chunkAcc: ListBuffer[HtmlToken]): Pull[F, HtmlToken, Unit] =
    ensureNextOrDone(ctx, chunkAcc) { (ctx, chunkAcc) =>
      (T.current(ctx): @switch) match {
        case '-' =>
          scriptDataEscapedDashDash(T.advance(ctx), opens, chunkAcc += HtmlToken.Character('-'))
        case '<' =>
          scriptDataEscapedLessThanSign(T.advance(ctx), opens, chunkAcc)
        case '>' =>
          scriptData(T.advance(ctx), opens, chunkAcc += HtmlToken.Character('>'))
        case c =>
          scriptDataEscaped(T.advance(ctx), opens, chunkAcc += HtmlToken.Character(c))
      }
    }

  def scriptDataEscapedLessThanSign(ctx: T.Context,
                                    opens: List[String],
                                    chunkAcc: ListBuffer[HtmlToken]): Pull[F, HtmlToken, Unit] =
    ensureNextOrDone(ctx, chunkAcc) { (ctx, chunkAcc) =>
      (T.current(ctx): @switch) match {
        case '/' =>
          scriptDataEscapedEndTagOpen(T.advance(ctx), opens, chunkAcc)
        case c =>
          if (isAsciiAlpha(c))
            scriptDataDoubleEscapeStart(ctx, new StringBuilder, opens, chunkAcc += HtmlToken.Character('<'))
          else
            scriptDataEscaped(ctx, opens, chunkAcc += HtmlToken.Character('<'))
      }
    }

  def scriptDataEscapedEndTagOpen(ctx: T.Context,
                                  opens: List[String],
                                  chunkAcc: ListBuffer[HtmlToken]): Pull[F, HtmlToken, Unit] =
    ensureNextOrDone(ctx, chunkAcc) { (ctx, chunkAcc) =>
      val c = T.current(ctx)
      if (isAsciiAlpha(c))
        scriptDataEscapedEndTagName(ctx, new StringBuilder, opens, chunkAcc)
      else
        scriptDataEscaped(ctx, opens, chunkAcc += HtmlToken.Character('<'))
    }

  def scriptDataEscapedEndTagName(ctx: T.Context,
                                  name: StringBuilder,
                                  opens: List[String],
                                  chunkAcc: ListBuffer[HtmlToken]): Pull[F, HtmlToken, Unit] =
    ensureNextOrDone(ctx, chunkAcc) { (ctx, chunkAcc) =>
      (T.current(ctx): @switch) match {
        case '\t' | '\r' | '\n' | ' ' =>
          val n = name.result()
          opens match {
            case open :: opens if open == n =>
              beforeAttributeName(T.advance(ctx), false, n, Map.empty, opens, chunkAcc)
            case _ =>
              scriptDataEscaped(ctx,
                                opens,
                                chunkAcc ++= List(HtmlToken.Character('<'), HtmlToken.Character('/')) ++= name.map(
                                  HtmlToken.Character(_)))
          }
        case '/' =>
          val n = name.result()
          opens match {
            case open :: opens if open == n =>
              selfClosingStartTag(T.advance(ctx), false, n, Map.empty, opens, chunkAcc)
            case _ =>
              scriptDataEscaped(ctx,
                                opens,
                                chunkAcc ++= List(HtmlToken.Character('<'), HtmlToken.Character('/')) ++= name.map(
                                  HtmlToken.Character(_)))
          }
        case '>' =>
          val n = name.result()
          opens match {
            case open :: opens if open == n =>
              data(T.advance(ctx), opens, chunkAcc += HtmlToken.EndTag(n))
            case _ =>
              scriptDataEscaped(ctx,
                                opens,
                                chunkAcc ++= List(HtmlToken.Character('<'), HtmlToken.Character('/')) ++= name.map(
                                  HtmlToken.Character(_)))
          }
        case c =>
          if (isAsciiAlpha(c))
            scriptDataEscapedEndTagName(T.advance(ctx), name.addOne(c.toLower), opens, chunkAcc)
          else
            scriptDataEscaped(ctx,
                              opens,
                              chunkAcc ++= List(HtmlToken.Character('<'), HtmlToken.Character('/')) ++= name.map(
                                HtmlToken.Character(_)))
      }
    }

  def scriptDataDoubleEscapeStart(ctx: T.Context,
                                  buffer: StringBuilder,
                                  opens: List[String],
                                  chunkAcc: ListBuffer[HtmlToken]): Pull[F, HtmlToken, Unit] =
    ensureNextOrDone(ctx, chunkAcc) { (ctx, chunkAcc) =>
      val c = T.current(ctx)
      (c: @switch) match {
        case '\t' | '\r' | '\n' | ' ' | '/' | '>' =>
          val b = buffer.result()
          (b: @switch) match {
            case "script" =>
              scriptDataDoubleEscaped(T.advance(ctx), opens, chunkAcc += HtmlToken.Character(c))
            case _ =>
              scriptDataEscaped(T.advance(ctx), opens, chunkAcc += HtmlToken.Character(c))
          }
        case c =>
          if (isAsciiAlpha(c))
            scriptDataDoubleEscapeStart(T.advance(ctx),
                                        buffer.addOne(c.toLower),
                                        opens,
                                        chunkAcc += HtmlToken.Character(c))
          else
            scriptDataEscaped(ctx, opens, chunkAcc)
      }
    }

  def scriptDataDoubleEscaped(ctx: T.Context,
                              opens: List[String],
                              chunkAcc: ListBuffer[HtmlToken]): Pull[F, HtmlToken, Unit] =
    ensureNextOrDone(ctx, chunkAcc) { (ctx, chunkAcc) =>
      (T.current(ctx): @switch) match {
        case '-' =>
          scriptDataDoubleEscapedDash(T.advance(ctx), opens, chunkAcc += HtmlToken.Character('-'))
        case '<' =>
          scriptDataDoubleEscapedLessThanSign(T.advance(ctx), opens, chunkAcc += HtmlToken.Character('<'))
        case '\u0000' =>
          scriptDataDoubleEscaped(T.advance(ctx), opens, chunkAcc += HtmlToken.Character('\ufffd'))
        case c =>
          scriptDataDoubleEscaped(T.advance(ctx), opens, chunkAcc += HtmlToken.Character(c))
      }
    }

  def scriptDataDoubleEscapedDash(ctx: T.Context,
                                  opens: List[String],
                                  chunkAcc: ListBuffer[HtmlToken]): Pull[F, HtmlToken, Unit] =
    ensureNextOrDone(ctx, chunkAcc) { (ctx, chunkAcc) =>
      (T.current(ctx): @switch) match {
        case '-' =>
          scriptDataDoubleEscapedDashDash(T.advance(ctx), opens, chunkAcc += HtmlToken.Character('-'))
        case '<' =>
          scriptDataDoubleEscapedLessThanSign(T.advance(ctx), opens, chunkAcc += HtmlToken.Character('<'))
        case '\u0000' =>
          scriptDataDoubleEscaped(T.advance(ctx), opens, chunkAcc += HtmlToken.Character('\ufffd'))
        case c =>
          scriptDataDoubleEscaped(T.advance(ctx), opens, chunkAcc += HtmlToken.Character(c))
      }
    }

  def scriptDataDoubleEscapedLessThanSign(ctx: T.Context,
                                          opens: List[String],
                                          chunkAcc: ListBuffer[HtmlToken]): Pull[F, HtmlToken, Unit] =
    ensureNextOrDone(ctx, chunkAcc) { (ctx, chunkAcc) =>
      (T.current(ctx): @switch) match {
        case '/' =>
          scriptDataDoubleEscapeEnd(T.advance(ctx), new StringBuilder, opens, chunkAcc += HtmlToken.Character('/'))
        case _ =>
          scriptDataDoubleEscaped(ctx, opens, chunkAcc)
      }
    }

  def scriptDataDoubleEscapeEnd(ctx: T.Context,
                                buffer: StringBuilder,
                                opens: List[String],
                                chunkAcc: ListBuffer[HtmlToken]): Pull[F, HtmlToken, Unit] =
    ensureNextOrDone(ctx, chunkAcc) { (ctx, chunkAcc) =>
      val c = T.current(ctx)
      (c: @switch) match {
        case '\t' | '\r' | '\n' | ' ' | '/' | '>' =>
          val b = buffer.result()
          (b: @switch) match {
            case "script" =>
              scriptDataEscaped(T.advance(ctx), opens, chunkAcc += HtmlToken.Character(c))
            case _ =>
              scriptDataDoubleEscaped(T.advance(ctx), opens, chunkAcc += HtmlToken.Character(c))
          }
        case _ =>
          if (isAsciiAlpha(c))
            scriptDataDoubleEscapeEnd(T.advance(ctx), buffer.addOne(c), opens, chunkAcc += HtmlToken.Character(c))
          else
            scriptDataDoubleEscaped(ctx, opens, chunkAcc)
      }
    }

  def scriptDataDoubleEscapedDashDash(ctx: T.Context,
                                      opens: List[String],
                                      chunkAcc: ListBuffer[HtmlToken]): Pull[F, HtmlToken, Unit] =
    ensureNextOrDone(ctx, chunkAcc) { (ctx, chunkAcc) =>
      (T.current(ctx): @switch) match {
        case '-' =>
          scriptDataDoubleEscapedDashDash(T.advance(ctx), opens, chunkAcc += HtmlToken.Character('-'))
        case '<' =>
          scriptDataDoubleEscapedLessThanSign(T.advance(ctx), opens, chunkAcc += HtmlToken.Character('<'))
        case '>' =>
          scriptData(T.advance(ctx), opens, chunkAcc += HtmlToken.Character('>'))
        case '\u0000' =>
          scriptDataDoubleEscaped(T.advance(ctx), opens, chunkAcc += HtmlToken.Character('\ufffd'))
        case c =>
          scriptDataDoubleEscaped(T.advance(ctx), opens, chunkAcc += HtmlToken.Character(c))
      }
    }

  def scriptDataEndTagName(ctx: T.Context,
                           name: StringBuilder,
                           opens: List[String],
                           chunkAcc: ListBuffer[HtmlToken]): Pull[F, HtmlToken, Unit] =
    ensureNextOrDone(ctx, chunkAcc) { (ctx, chunkAcc) =>
      (T.current(ctx): @switch) match {
        case '\t' | '\r' | '\n' | ' ' =>
          val n = name.result()
          opens match {
            case open :: opens if open == n =>
              beforeAttributeName(T.advance(ctx), false, n, Map.empty, opens, chunkAcc)
            case _ =>
              scriptData(ctx,
                         opens,
                         chunkAcc ++= List(HtmlToken.Character('<'), HtmlToken.Character('/')) ++= name.map(
                           HtmlToken.Character(_)))
          }
        case '/' =>
          val n = name.result()
          opens match {
            case open :: opens if open == n =>
              selfClosingStartTag(T.advance(ctx), false, n, Map.empty, opens, chunkAcc)
            case _ =>
              scriptData(ctx,
                         opens,
                         chunkAcc ++= List(HtmlToken.Character('<'), HtmlToken.Character('/')) ++= name.map(
                           HtmlToken.Character(_)))
          }
        case '>' =>
          val n = name.result()
          opens match {
            case open :: opens if open == n =>
              data(T.advance(ctx), opens, chunkAcc += HtmlToken.EndTag(n))
            case _ =>
              scriptData(ctx,
                         opens,
                         chunkAcc ++= List(HtmlToken.Character('<'), HtmlToken.Character('/')) ++= name.map(
                           HtmlToken.Character(_)))
          }
        case c =>
          scriptData(
            ctx,
            opens,
            chunkAcc ++= List(HtmlToken.Character('<'), HtmlToken.Character('/')) ++= name.map(HtmlToken.Character(_)))
      }
    }

  def scriptData(ctx: T.Context, opens: List[String], chunkAcc: ListBuffer[HtmlToken]): Pull[F, HtmlToken, Unit] =
    ensureNextOrDone(ctx, chunkAcc) { (ctx, chunkAcc) =>
      (T.current(ctx): @switch) match {
        case '<' =>
          scriptDataLessThanSign(T.advance(ctx), opens, chunkAcc)
        case '\u0000' =>
          scriptData(T.advance(ctx), opens, chunkAcc += HtmlToken.Character('\ufffd'))
        case c =>
          scriptData(T.advance(ctx), opens, chunkAcc += HtmlToken.Character(c))
      }
    }

  def plainText(ctx: T.Context, opens: List[String], chunkAcc: ListBuffer[HtmlToken]): Pull[F, HtmlToken, Unit] =
    ensureNextOrDone(ctx, chunkAcc) { (ctx, chunkAcc) =>
      (T.current(ctx): @switch) match {
        case '\u0000' =>
          plainText(T.advance(ctx), opens, chunkAcc += HtmlToken.Character('\ufffd'))
        case c =>
          plainText(T.advance(ctx), opens, chunkAcc += HtmlToken.Character(c))
      }
    }

  def data(ctx: T.Context, opens: List[String], chunkAcc: ListBuffer[HtmlToken]): Pull[F, HtmlToken, Unit] =
    ensureNextOrDone(ctx, chunkAcc) { (ctx, chunkAcc) =>
      (T.current(ctx): @switch) match {
        case '&' =>
          def kont(ctx: T.Context, chunkAcc: ListBuffer[HtmlToken], toFlush: String) =
            data(ctx, opens, chunkAcc ++= toFlush.map(HtmlToken.Character(_)))

          characterReference(T.advance(ctx), kont, chunkAcc)
        case '<' => tagOpen(T.advance(ctx), opens, chunkAcc)
        case '\u0000' =>
          data(T.advance(ctx), opens, chunkAcc)
        case c =>
          data(T.advance(ctx), opens, chunkAcc += HtmlToken.Character(c))
      }
    }

  def pipe: Pipe[F, T, HtmlToken] = { s =>
    Stream.suspend(Stream.emit(T.create(s))).flatMap(data(_, Nil, new ListBuffer).stream)
  }

  private def isAsciiUpperAlpha(c: Char) =
    'A' <= c && c <= 'Z'

  private def isAsciiAlpha(c: Char) =
    ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z')

  private val hexdigits = "0123456789abcdef"

  private def isAsciiHex(c: Char) =
    hexdigits.indexOf(c.toLower) >= 0

  private val digits = "0123456789"

  private def isAsciiDigit(c: Char) =
    digits.indexOf(c.toLower) >= 0

  private val nonchars = Set(0xfffe, 0xffff, 0x1fffe, 0x1ffff, 0x2fffe, 0x2ffff, 0x3fffe, 0x3ffff, 0x4fffe, 0x4ffff,
                             0x5fffe, 0x5ffff, 0x6fffe, 0x6ffff, 0x7fffe, 0x7ffff, 0x8fffe, 0x8ffff, 0x9fffe, 0x9ffff,
                             0xafffe, 0xaffff, 0xbfffe, 0xbffff, 0xcfffe, 0xcffff, 0xdfffe, 0xdffff, 0xefffe, 0xeffff,
                             0xffffe, 0xfffff, 0x10fffe, 0x10ffff)

  private def isNonCharacter(i: Int) =
    (0xfdd0 <= i && i <= 0xfdef) || nonchars.contains(i)

  private def isControl(i: Int) =
    (0x0000 <= i && i <= 0x001f) || (0x007f <= i && i <= 0x009f)

  private val wspace = Set(0x0009, 0x000a, 0x000c, 0x000d, 0x0020)

  private val codes = Map(
    0x80 -> 0x20ac,
    0x82 -> 0x201a,
    0x83 -> 0x0192,
    0x84 -> 0x201e,
    0x85 -> 0x2026,
    0x86 -> 0x2020,
    0x87 -> 0x2021,
    0x88 -> 0x02c6,
    0x89 -> 0x2030,
    0x8a -> 0x0160,
    0x8b -> 0x2039,
    0x8c -> 0x0152,
    0x8e -> 0x017d,
    0x91 -> 0x2018,
    0x92 -> 0x2019,
    0x93 -> 0x201c,
    0x94 -> 0x201d,
    0x95 -> 0x2022,
    0x96 -> 0x2013,
    0x97 -> 0x2014,
    0x98 -> 0x02dc,
    0x99 -> 0x2122,
    0x9a -> 0x0161,
    0x9b -> 0x203a,
    0x9c -> 0x0153,
    0x9e -> 0x017e,
    0x9f -> 0x0178
  )

  private def allCases(s: String): List[String] = {
    def loop(todo: List[(String, Int)], acc: List[String]): List[String] =
      todo match {
        case Nil => acc
        case (prefix, idx) :: todo =>
          if (idx >= s.length()) {
            loop(todo, prefix :: acc)
          } else {
            val c = s.charAt(idx)
            loop((prefix + c.toLower, idx + 1) :: (prefix + c.toUpper, idx + 1) :: todo, acc)
          }
      }
    loop(List("" -> 0), Nil)
  }

  private val markupDecls =
    RadixNode.fromSortedStrings(NonEmptyList("[CDATA[", "--" :: allCases("doctype")).sorted)

  private val doctypeVisibility =
    RadixNode.fromSortedStrings(NonEmptyList.fromListUnsafe(allCases("public") ++ allCases("system")))

  private val elements = Set(
    "a",
    "abbr",
    "address",
    "area",
    "article",
    "aside",
    "audio",
    "b",
    "base",
    "bdi",
    "bdo",
    "blockquote",
    "body",
    "br",
    "button",
    "canvas",
    "caption",
    "cite",
    "code",
    "col",
    "colgroup",
    "data",
    "datalist",
    "dd",
    "del",
    "details",
    "dfn",
    "dialog",
    "div",
    "dl",
    "dt",
    "em",
    "embed",
    "fieldset",
    "figcaption",
    "figure",
    "footer",
    "form",
    "h1",
    "h2",
    "h3",
    "h4",
    "h5",
    "h6",
    "head",
    "header",
    "hgroup",
    "hr",
    "html",
    "i",
    "iframe",
    "img",
    "input",
    "ins",
    "kbd",
    "label",
    "legend",
    "li",
    "link",
    "main",
    "map",
    "mark",
    "math",
    "menu",
    "menuitem",
    "meta",
    "meter",
    "nav",
    "noscript",
    "object",
    "ol",
    "optgroup",
    "option",
    "output",
    "p",
    "param",
    "picture",
    "pre",
    "progress",
    "q",
    "rb",
    "rp",
    "rt",
    "rtc",
    "ruby",
    "s",
    "samp",
    "script",
    "section",
    "select",
    "slot",
    "small",
    "source",
    "span",
    "strong",
    "style",
    "sub",
    "summary",
    "sup",
    "svg",
    "table",
    "tbody",
    "td",
    "template",
    "textarea",
    "tfoot",
    "th",
    "thead",
    "time",
    "title",
    "tr",
    "track",
    "u",
    "ul",
    "var",
    "video",
    "wbr"
  )

}
