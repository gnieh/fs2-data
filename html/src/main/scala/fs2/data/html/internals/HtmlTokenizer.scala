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

case class Pulled[F[_], Ctx, V](ctx: Ctx,
                                chunkAcc: ListBuffer[V],
                                opens: List[String],
                                token: Option[HtmlToken],
                                next: (Ctx, List[String], ListBuffer[V]) => Pull[F, V, Pulled[F, Ctx, V]]) {
  def pushOpen(name: String): Pulled[F, Ctx, V] =
    copy(opens = name :: opens)
}

private[html] class HtmlTokenizer[F[_], T](implicit F: RaiseThrowable[F], val T: CharLikeChunks[F, T]) {

  def done[V](opens: List[String]): Pull[F, V, Pulled[F, T.Context, V]] =
    Pull.pure(Pulled[F, T.Context, V](T.emptyContext, new ListBuffer, opens, none, (_, opens, _) => done[V](opens)))

  def done[V](opens: List[String], emit: List[HtmlToken]): Pull[F, V, Pulled[F, T.Context, V]] =
    emit.foldRight(done[V](_: List[String])) { (token, next) => opens =>
      Pull.pure(
        Pulled[F, T.Context, V](T.emptyContext, new ListBuffer, opens, token.some, (_, opens, _) => next(opens)))
    }(opens)

  def emitAll[V](emit: List[HtmlToken],
                 next: (T.Context, List[String], ListBuffer[V]) => Pull[F, V, Pulled[F, T.Context, V]])
      : (T.Context, List[String], ListBuffer[V]) => Pull[F, V, Pulled[F, T.Context, V]] =
    emit.foldRight(next) { (token, next) => (ctx, opens, chunkAcc) =>
      Pull.pure(Pulled[F, T.Context, V](ctx, chunkAcc, opens, token.some, next))
    }

  def ensureNext[V, Res](ctx: T.Context, chunkAcc: ListBuffer[V])(
      kont: Option[(T.Context, ListBuffer[V])] => Pull[F, V, Res]): Pull[F, V, Res] =
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

  def ensureNextOrDone[V](ctx: T.Context, opens: List[String], chunkAcc: ListBuffer[V])(
      kont: (T.Context, ListBuffer[V]) => Pull[F, V, Pulled[F, T.Context, V]]): Pull[F, V, Pulled[F, T.Context, V]] =
    ensureNext(ctx, chunkAcc) {
      case Some((ctx, chunkAcc)) => kont(ctx, chunkAcc)
      case None                  => done(opens)
    }

  private def readFromRadixTree[V](ctx: T.Context, chunkAcc: ListBuffer[V], tree: RadixNode) =
    Pull
      .loopEither[F, V, (T.Context, ListBuffer[V], StringBuilder), (T.Context, ListBuffer[V], String)] {
        case (ctx, chunkAcc, acc) =>
          ensureNext(ctx, chunkAcc) {
            case Some((ctx, chunkAcc)) =>
              val current = acc.result()
              val c = T.current(ctx)
              if (tree.isPrefix(current + c))
                Pull.pure((T.advance(ctx), chunkAcc, acc.addOne(c)).asLeft)
              else
                Pull.pure((ctx, chunkAcc, acc.result()).asRight)
            case None =>
              Pull.pure((T.emptyContext, new ListBuffer[V], acc.result()).asRight)
          }
      }(ctx, chunkAcc, new StringBuilder)

  def characterReference[V](ctx: T.Context,
                            continue: (T.Context, ListBuffer[V], String) => Pull[F, V, Pulled[F, T.Context, V]],
                            chunkAcc: ListBuffer[V]): Pull[F, V, Pulled[F, T.Context, V]] =
    ensureNextOrDone(ctx, Nil, chunkAcc) { (ctx, chunkAcc) =>
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

  def namedCharacterReference[V](ctx: T.Context,
                                 buffer: StringBuilder,
                                 continue: (T.Context, ListBuffer[V], String) => Pull[F, V, Pulled[F, T.Context, V]],
                                 chunkAcc: ListBuffer[V]): Pull[F, V, Pulled[F, T.Context, V]] =
    Pull
      .loopEither[F, V, (T.Context, ListBuffer[V], StringBuilder), (T.Context, ListBuffer[V], StringBuilder)] {
        case (ctx, chunkAcc, buffer) =>
          ensureNext(ctx, chunkAcc) {
            case Some((ctx, chunkAcc)) =>
              val current = buffer.result()
              val c = T.current(ctx)
              if (Entities.isPrefix(current + c))
                Pull.pure((T.advance(ctx), chunkAcc, buffer.addOne(c)).asLeft)
              else
                Pull.pure((ctx, chunkAcc, buffer).asRight)
            case None =>
              Pull.pure((T.emptyContext, new ListBuffer[V], buffer).asRight)
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

  def ambiguousAmpersandState[V](ctx: T.Context,
                                 buffer: StringBuilder,
                                 continue: (T.Context, ListBuffer[V], String) => Pull[F, V, Pulled[F, T.Context, V]],
                                 chunkAcc: ListBuffer[V]): Pull[F, V, Pulled[F, T.Context, V]] =
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

  def numericCharacterReference[V](ctx: T.Context,
                                   buffer: StringBuilder,
                                   continue: (T.Context, ListBuffer[V], String) => Pull[F, V, Pulled[F, T.Context, V]],
                                   chunkAcc: ListBuffer[V]): Pull[F, V, Pulled[F, T.Context, V]] =
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

  def hexadecimalCharacterReferenceStart[V](
      ctx: T.Context,
      buffer: StringBuilder,
      value: Int,
      continue: (T.Context, ListBuffer[V], String) => Pull[F, V, Pulled[F, T.Context, V]],
      chunkAcc: ListBuffer[V]): Pull[F, V, Pulled[F, T.Context, V]] =
    ensureNext(ctx, chunkAcc) {
      case Some((ctx, chunkAcc)) =>
        if (isAsciiHex(T.current(ctx)))
          hexadecimalCharacterReference(ctx, value, continue, chunkAcc)
        else
          continue(T.emptyContext, new ListBuffer, buffer.result())
      case None =>
        continue(T.emptyContext, new ListBuffer, buffer.result())
    }

  def decimalCharacterReferenceStart[V](
      ctx: T.Context,
      buffer: StringBuilder,
      value: Int,
      continue: (T.Context, ListBuffer[V], String) => Pull[F, V, Pulled[F, T.Context, V]],
      chunkAcc: ListBuffer[V]): Pull[F, V, Pulled[F, T.Context, V]] =
    ensureNext(ctx, chunkAcc) {
      case Some((ctx, chunkAcc)) =>
        if (isAsciiDigit(T.current(ctx)))
          decimalCharacterReference(ctx, value, continue, chunkAcc)
        else
          continue(T.emptyContext, new ListBuffer, buffer.result())
      case None =>
        continue(T.emptyContext, new ListBuffer, buffer.result())
    }

  def hexadecimalCharacterReference[V](
      ctx: T.Context,
      value: Int,
      continue: (T.Context, ListBuffer[V], String) => Pull[F, V, Pulled[F, T.Context, V]],
      chunkAcc: ListBuffer[V]): Pull[F, V, Pulled[F, T.Context, V]] =
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

  def decimalCharacterReference[V](ctx: T.Context,
                                   value: Int,
                                   continue: (T.Context, ListBuffer[V], String) => Pull[F, V, Pulled[F, T.Context, V]],
                                   chunkAcc: ListBuffer[V]): Pull[F, V, Pulled[F, T.Context, V]] =
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

  def numericCharacterReferenceEnd[V](
      ctx: T.Context,
      value: Int,
      continue: (T.Context, ListBuffer[V], String) => Pull[F, V, Pulled[F, T.Context, V]],
      chunkAcc: ListBuffer[V]): Pull[F, V, Pulled[F, T.Context, V]] =
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

  def tagOpen[V](ctx: T.Context, opens: List[String], chunkAcc: ListBuffer[V]): Pull[F, V, Pulled[F, T.Context, V]] =
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
              Pull.pure(Pulled[F, T.Context, V](ctx, chunkAcc, opens, HtmlToken.Character('<').some, data(_, _, _)))
        }
      case None =>
        done(opens, List(HtmlToken.Character('<')))
    }

  def endTagOpen[V](ctx: T.Context, opens: List[String], chunkAcc: ListBuffer[V]): Pull[F, V, Pulled[F, T.Context, V]] =
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
        done(opens, List(HtmlToken.Character('<'), HtmlToken.Character('/')))
    }

  def bogusComment[V](ctx: T.Context,
                      content: StringBuilder,
                      opens: List[String],
                      chunkAcc: ListBuffer[V]): Pull[F, V, Pulled[F, T.Context, V]] =
    ensureNextOrDone(ctx, opens, chunkAcc) { (ctx, chunkAcc) =>
      (T.current(ctx): @switch) match {
        case '>' =>
          Pull.pure(
            Pulled[F, T.Context, V](T.advance(ctx),
                                    chunkAcc,
                                    opens,
                                    HtmlToken.Comment(content.result()).some,
                                    data(_, _, _)))
        case '\u0000' =>
          bogusComment(T.advance(ctx), content.addOne('\ufffd'), opens, chunkAcc)
        case c =>
          bogusComment(T.advance(ctx), content.addOne(c), opens, chunkAcc)
      }
    }

  def tagName[V](ctx: T.Context,
                 open: Boolean,
                 name: StringBuilder,
                 opens: List[String],
                 chunkAcc: ListBuffer[V]): Pull[F, V, Pulled[F, T.Context, V]] =
    ensureNextOrDone(ctx, opens, chunkAcc) { (ctx, chunkAcc) =>
      (T.current(ctx): @switch) match {
        case '\t' | '\r' | '\n' | ' ' =>
          beforeAttributeName(T.advance(ctx), open, name.result(), Map.empty, opens, chunkAcc)
        case '/' =>
          selfClosingStartTag(T.advance(ctx), open, name.result(), Map.empty, opens, chunkAcc)
        case '>' =>
          val n = name.result()
          if (open)
            Pull.pure(
              Pulled[F, T.Context, V](T.advance(ctx),
                                      chunkAcc,
                                      opens,
                                      HtmlToken.OpenTag(n, Map.empty, false).some,
                                      data(_, _, _)))
          else
            Pull.pure(Pulled[F, T.Context, V](T.advance(ctx), chunkAcc, opens, HtmlToken.EndTag(n).some, data(_, _, _)))
        case '\u0000' =>
          tagName(T.advance(ctx), open, name.addOne('\ufffd'), opens, chunkAcc)
        case c =>
          if (isAsciiAlpha(c))
            tagName(T.advance(ctx), open, name.addOne(c.toLower), opens, chunkAcc)
          else
            tagName(T.advance(ctx), open, name.addOne(c), opens, chunkAcc)

      }
    }

  def beforeAttributeName[V](ctx: T.Context,
                             open: Boolean,
                             name: String,
                             attributes: Map[String, String],
                             opens: List[String],
                             chunkAcc: ListBuffer[V]): Pull[F, V, Pulled[F, T.Context, V]] =
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

  def attributeName[V](ctx: T.Context,
                       open: Boolean,
                       name: String,
                       attr: StringBuilder,
                       attributes: Map[String, String],
                       opens: List[String],
                       chunkAcc: ListBuffer[V]): Pull[F, V, Pulled[F, T.Context, V]] =
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

  def afterAttributeName[V](ctx: T.Context,
                            open: Boolean,
                            name: String,
                            attr: String,
                            attributes: Map[String, String],
                            opens: List[String],
                            chunkAcc: ListBuffer[V]): Pull[F, V, Pulled[F, T.Context, V]] =
    ensureNextOrDone(ctx, opens, chunkAcc) { (ctx, chunkAcc) =>
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
            Pull.pure(
              Pulled[F, T.Context, V](T.advance(ctx),
                                      chunkAcc,
                                      opens,
                                      HtmlToken.OpenTag(name, attributes1, false).some,
                                      data(_, _, _)))
          else
            Pull.pure(
              Pulled[F, T.Context, V](T.advance(ctx), chunkAcc, opens, HtmlToken.EndTag(name).some, data(_, _, _)))
        case _ =>
          val attributes1 =
            if (attr.isEmpty || attributes.contains(attr))
              attributes
            else
              attributes.updated(attr, "")
          attributeName(ctx, open, name, new StringBuilder, attributes1, opens, chunkAcc)
      }
    }

  def beforeAttributeValue[V](ctx: T.Context,
                              open: Boolean,
                              name: String,
                              attr: String,
                              attributes: Map[String, String],
                              opens: List[String],
                              chunkAcc: ListBuffer[V]): Pull[F, V, Pulled[F, T.Context, V]] =
    ensureNextOrDone(ctx, opens, chunkAcc) { (ctx, chunkAcc) =>
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
            Pull.pure(
              Pulled[F, T.Context, V](T.advance(ctx),
                                      chunkAcc,
                                      opens,
                                      HtmlToken.OpenTag(name, attributes1, false).some,
                                      data(_, _, _)))
          else
            Pull.pure(
              Pulled[F, T.Context, V](T.advance(ctx), chunkAcc, opens, HtmlToken.EndTag(name).some, data(_, _, _)))
        case _ =>
          attributeValueUnquoted(ctx, open, name, attr, new StringBuilder, attributes, opens, chunkAcc)
      }
    }

  def attributeValueDoubleQuoted[V](ctx: T.Context,
                                    open: Boolean,
                                    name: String,
                                    attr: String,
                                    value: StringBuilder,
                                    attributes: Map[String, String],
                                    opens: List[String],
                                    chunkAcc: ListBuffer[V]): Pull[F, V, Pulled[F, T.Context, V]] =
    ensureNextOrDone(ctx, opens, chunkAcc) { (ctx, chunkAcc) =>
      (T.current(ctx): @switch) match {
        case '"' =>
          val attributes1 =
            if (attributes.contains(attr))
              attributes
            else
              attributes.updated(attr, value.result())
          afterAttributeValueQuoted(T.advance(ctx), open, name, attributes1, opens, chunkAcc)
        case '&' =>
          def kont[V](ctx: T.Context, chunkAcc: ListBuffer[V], toFlush: String) =
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

  def attributeValueSingleQuoted[V](ctx: T.Context,
                                    open: Boolean,
                                    name: String,
                                    attr: String,
                                    value: StringBuilder,
                                    attributes: Map[String, String],
                                    opens: List[String],
                                    chunkAcc: ListBuffer[V]): Pull[F, V, Pulled[F, T.Context, V]] =
    ensureNextOrDone(ctx, opens, chunkAcc) { (ctx, chunkAcc) =>
      (T.current(ctx): @switch) match {
        case '\'' =>
          val attributes1 =
            if (attributes.contains(attr))
              attributes
            else
              attributes.updated(attr, value.result())
          afterAttributeValueQuoted(T.advance(ctx), open, name, attributes1, opens, chunkAcc)
        case '&' =>
          def kont[V](ctx: T.Context, chunkAcc: ListBuffer[V], toFlush: String) =
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

  def attributeValueUnquoted[V](ctx: T.Context,
                                open: Boolean,
                                name: String,
                                attr: String,
                                value: StringBuilder,
                                attributes: Map[String, String],
                                opens: List[String],
                                chunkAcc: ListBuffer[V]): Pull[F, V, Pulled[F, T.Context, V]] =
    ensureNextOrDone(ctx, opens, chunkAcc) { (ctx, chunkAcc) =>
      (T.current(ctx): @switch) match {
        case '\t' | '\r' | '\n' | ' ' =>
          val attributes1 =
            if (attributes.contains(attr))
              attributes
            else
              attributes.updated(attr, value.result())
          beforeAttributeName(T.advance(ctx), open, name, attributes1, opens, chunkAcc)
        case '&' =>
          def kont[V](ctx: T.Context, chunkAcc: ListBuffer[V], toFlush: String) =
            attributeValueUnquoted(ctx, open, name, attr, value.addAll(toFlush), attributes, opens, chunkAcc)

          characterReference(T.advance(ctx), kont, chunkAcc)
        case '>' =>
          val attributes1 =
            if (attributes.contains(attr))
              attributes
            else
              attributes.updated(attr, value.result())
          if (open)
            Pull.pure(
              Pulled[F, T.Context, V](T.advance(ctx),
                                      chunkAcc,
                                      opens,
                                      HtmlToken.OpenTag(name, attributes1, false).some,
                                      data(_, _, _)))
          else
            Pull.pure(
              Pulled[F, T.Context, V](T.advance(ctx), chunkAcc, opens, HtmlToken.EndTag(name).some, data(_, _, _)))
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

  def afterAttributeValueQuoted[V](ctx: T.Context,
                                   open: Boolean,
                                   name: String,
                                   attributes: Map[String, String],
                                   opens: List[String],
                                   chunkAcc: ListBuffer[V]): Pull[F, V, Pulled[F, T.Context, V]] =
    ensureNextOrDone(ctx, opens, chunkAcc) { (ctx, chunkAcc) =>
      (T.current(ctx): @switch) match {
        case '\t' | '\r' | '\n' | ' ' =>
          beforeAttributeName(T.advance(ctx), open, name, attributes, opens, chunkAcc)
        case '/' =>
          selfClosingStartTag(T.advance(ctx), open, name, attributes, opens, chunkAcc)
        case '>' =>
          if (open)
            Pull.pure(
              Pulled[F, T.Context, V](T.advance(ctx),
                                      chunkAcc,
                                      opens,
                                      HtmlToken.OpenTag(name, attributes, false).some,
                                      data(_, _, _)))
          else
            Pull.pure(
              Pulled[F, T.Context, V](T.advance(ctx), chunkAcc, opens, HtmlToken.EndTag(name).some, data(_, _, _)))
        case _ =>
          beforeAttributeName(ctx, open, name, attributes, opens, chunkAcc)
      }
    }

  def selfClosingStartTag[V](ctx: T.Context,
                             open: Boolean,
                             name: String,
                             attributes: Map[String, String],
                             opens: List[String],
                             chunkAcc: ListBuffer[V]): Pull[F, V, Pulled[F, T.Context, V]] =
    ensureNextOrDone(ctx, opens, chunkAcc) { (ctx, chunkAcc) =>
      (T.current(ctx): @switch) match {
        case '>' =>
          if (open)
            Pull.pure(
              Pulled[F, T.Context, V](T.advance(ctx),
                                      chunkAcc,
                                      opens,
                                      HtmlToken.OpenTag(name, attributes, true).some,
                                      data(_, _, _)))
          else
            Pull.pure(
              Pulled[F, T.Context, V](T.advance(ctx), chunkAcc, opens, HtmlToken.EndTag(name).some, data(_, _, _)))
        case _ =>
          beforeAttributeName(ctx, open, name, attributes, opens, chunkAcc)
      }
    }

  def markupDeclarationOpen[V](ctx: T.Context,
                               opens: List[String],
                               chunkAcc: ListBuffer[V]): Pull[F, V, Pulled[F, T.Context, V]] =
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

  def doctype[V](ctx: T.Context, opens: List[String], chunkAcc: ListBuffer[V]): Pull[F, V, Pulled[F, T.Context, V]] =
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
        Pull.pure(
          Pulled[F, T.Context, V](T.emptyContext,
                                  new ListBuffer,
                                  opens,
                                  HtmlToken.Doctype(true, "", none, none).some,
                                  (_, opens, _) => done(opens)))
    }

  def beforeDoctypeName[V](ctx: T.Context,
                           opens: List[String],
                           chunkAcc: ListBuffer[V]): Pull[F, V, Pulled[F, T.Context, V]] =
    ensureNext(ctx, chunkAcc) {
      case Some((ctx, chunkAcc)) =>
        (T.current(ctx): @switch) match {
          case '\t' | '\r' | '\n' | ' ' =>
            beforeDoctypeName(T.advance(ctx), opens, chunkAcc)
          case '>' =>
            Pull.pure(
              Pulled[F, T.Context, V](T.advance(ctx),
                                      chunkAcc,
                                      opens,
                                      HtmlToken.Doctype(true, "", none, none).some,
                                      data(_, _, _)))
          case '\u0000' =>
            doctypeName(T.advance(ctx), new StringBuilder("\ufffd"), opens, chunkAcc)
          case c =>
            if (isAsciiUpperAlpha(c))
              doctypeName(T.advance(ctx), new StringBuilder(s"${c.toLower}"), opens, chunkAcc)
            else
              doctypeName(T.advance(ctx), new StringBuilder(s"$c"), opens, chunkAcc)
        }
      case None =>
        Pull.pure(
          Pulled[F, T.Context, V](T.emptyContext,
                                  new ListBuffer,
                                  opens,
                                  HtmlToken.Doctype(true, "", none, none).some,
                                  (_, opens, _) => done(opens)))
    }

  def doctypeName[V](ctx: T.Context,
                     name: StringBuilder,
                     opens: List[String],
                     chunkAcc: ListBuffer[V]): Pull[F, V, Pulled[F, T.Context, V]] =
    ensureNext(ctx, chunkAcc) {
      case Some((ctx, chunkAcc)) =>
        (T.current(ctx): @switch) match {
          case '\t' | '\r' | '\n' | ' ' =>
            afterDoctypeName(T.advance(ctx), name.result(), opens, chunkAcc)
          case '>' =>
            Pull.pure(
              Pulled[F, T.Context, V](T.advance(ctx),
                                      chunkAcc,
                                      opens,
                                      HtmlToken.Doctype(false, name.result(), none, none).some,
                                      data(_, _, _)))
          case '\u0000' =>
            doctypeName(T.advance(ctx), new StringBuilder("\ufffd"), opens, chunkAcc)
          case c =>
            if (isAsciiUpperAlpha(c))
              doctypeName(T.advance(ctx), new StringBuilder(s"${c.toLower}"), opens, chunkAcc)
            else
              doctypeName(T.advance(ctx), new StringBuilder(s"$c"), opens, chunkAcc)
        }
      case None =>
        Pull.pure(
          Pulled[F, T.Context, V](T.emptyContext,
                                  new ListBuffer,
                                  opens,
                                  HtmlToken.Doctype(true, name.result(), none, none).some,
                                  (_, opens, _) => done(opens)))
    }

  def afterDoctypeName[V](ctx: T.Context,
                          name: String,
                          opens: List[String],
                          chunkAcc: ListBuffer[V]): Pull[F, V, Pulled[F, T.Context, V]] =
    ensureNext(ctx, chunkAcc) {
      case Some((ctx, chunkAcc)) =>
        (T.current(ctx): @switch) match {
          case '\t' | '\r' | '\n' | ' ' =>
            afterDoctypeName(T.advance(ctx), name, opens, chunkAcc)
          case '>' =>
            Pull.pure(
              Pulled[F, T.Context, V](T.advance(ctx),
                                      chunkAcc,
                                      opens,
                                      HtmlToken.Doctype(false, name, none, none).some,
                                      data(_, _, _)))
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
        Pull.pure(
          Pulled[F, T.Context, V](T.emptyContext,
                                  new ListBuffer,
                                  opens,
                                  HtmlToken.Doctype(true, name, none, none).some,
                                  (_, opens, _) => done(opens)))
    }

  def afterDoctypePublicKeyword[V](ctx: T.Context,
                                   name: String,
                                   opens: List[String],
                                   chunkAcc: ListBuffer[V]): Pull[F, V, Pulled[F, T.Context, V]] =
    ensureNext(ctx, chunkAcc) {
      case Some((ctx, chunkAcc)) =>
        (T.current(ctx): @switch) match {
          case '\t' | '\r' | '\n' | ' ' =>
            beforeDoctypePublicIdentifier(T.advance(ctx), name, opens, chunkAcc)
          case '>' =>
            Pull.pure(
              Pulled[F, T.Context, V](T.advance(ctx),
                                      chunkAcc,
                                      opens,
                                      HtmlToken.Doctype(true, name, none, none).some,
                                      data(_, _, _)))
          case '"' =>
            doctypePublicIdentifierDoubleQuoted(T.advance(ctx), name, new StringBuilder, opens, chunkAcc)
          case '\'' =>
            doctypePublicIdentifierSingleQuoted(T.advance(ctx), name, new StringBuilder, opens, chunkAcc)
          case _ =>
            bogusDoctype(ctx, true, name, none, none, opens, chunkAcc)
        }
      case None =>
        Pull.pure(
          Pulled[F, T.Context, V](T.emptyContext,
                                  new ListBuffer,
                                  opens,
                                  HtmlToken.Doctype(true, name, none, none).some,
                                  (_, opens, _) => done(opens)))
    }

  def beforeDoctypePublicIdentifier[V](ctx: T.Context,
                                       name: String,
                                       opens: List[String],
                                       chunkAcc: ListBuffer[V]): Pull[F, V, Pulled[F, T.Context, V]] =
    ensureNext(ctx, chunkAcc) {
      case Some((ctx, chunkAcc)) =>
        (T.current(ctx): @switch) match {
          case '\t' | '\r' | '\n' | ' ' =>
            beforeDoctypePublicIdentifier(T.advance(ctx), name, opens, chunkAcc)
          case '>' =>
            Pull.pure(
              Pulled[F, T.Context, V](T.advance(ctx),
                                      chunkAcc,
                                      opens,
                                      HtmlToken.Doctype(true, name, none, none).some,
                                      data(_, _, _)))
          case '"' =>
            doctypePublicIdentifierDoubleQuoted(T.advance(ctx), name, new StringBuilder, opens, chunkAcc)
          case '\'' =>
            doctypePublicIdentifierSingleQuoted(T.advance(ctx), name, new StringBuilder, opens, chunkAcc)
          case _ =>
            bogusDoctype(ctx, true, name, none, none, opens, chunkAcc)
        }
      case None =>
        Pull.pure(
          Pulled[F, T.Context, V](T.emptyContext,
                                  new ListBuffer,
                                  opens,
                                  HtmlToken.Doctype(true, name, none, none).some,
                                  (_, opens, _) => done(opens)))
    }

  def doctypePublicIdentifierDoubleQuoted[V](ctx: T.Context,
                                             name: String,
                                             id: StringBuilder,
                                             opens: List[String],
                                             chunkAcc: ListBuffer[V]): Pull[F, V, Pulled[F, T.Context, V]] =
    ensureNext(ctx, chunkAcc) {
      case Some((ctx, chunkAcc)) =>
        (T.current(ctx): @switch) match {
          case '"' => afterDoctypePublicIdentifier(T.advance(ctx), name, id.result(), opens, chunkAcc)
          case '\u0000' =>
            doctypePublicIdentifierDoubleQuoted(T.advance(ctx), name, id.addOne('\ufffd'), opens, chunkAcc)
          case '>' =>
            Pull.pure(
              Pulled[F, T.Context, V](T.advance(ctx),
                                      chunkAcc,
                                      opens,
                                      HtmlToken.Doctype(true, name, id.result().some, none).some,
                                      data(_, _, _)))
          case c =>
            doctypePublicIdentifierDoubleQuoted(T.advance(ctx), name, id.addOne(c), opens, chunkAcc)
        }
      case None =>
        Pull.pure(
          Pulled[F, T.Context, V](T.emptyContext,
                                  new ListBuffer,
                                  opens,
                                  HtmlToken.Doctype(true, name, id.result().some, none).some,
                                  (_, opens, _) => done(opens)))
    }

  def doctypePublicIdentifierSingleQuoted[V](ctx: T.Context,
                                             name: String,
                                             id: StringBuilder,
                                             opens: List[String],
                                             chunkAcc: ListBuffer[V]): Pull[F, V, Pulled[F, T.Context, V]] =
    ensureNext(ctx, chunkAcc) {
      case Some((ctx, chunkAcc)) =>
        (T.current(ctx): @switch) match {
          case '\'' => afterDoctypePublicIdentifier(T.advance(ctx), name, id.result(), opens, chunkAcc)
          case '\u0000' =>
            doctypePublicIdentifierSingleQuoted(T.advance(ctx), name, id.addOne('\ufffd'), opens, chunkAcc)
          case '>' =>
            Pull.pure(
              Pulled[F, T.Context, V](T.advance(ctx),
                                      chunkAcc,
                                      opens,
                                      HtmlToken.Doctype(true, name, id.result().some, none).some,
                                      data(_, _, _)))
          case c =>
            doctypePublicIdentifierSingleQuoted(T.advance(ctx), name, id.addOne(c), opens, chunkAcc)
        }
      case None =>
        Pull.pure(
          Pulled[F, T.Context, V](T.emptyContext,
                                  new ListBuffer,
                                  opens,
                                  HtmlToken.Doctype(true, name, id.result().some, none).some,
                                  (_, opens, _) => done(opens)))
    }

  def afterDoctypePublicIdentifier[V](ctx: T.Context,
                                      name: String,
                                      id: String,
                                      opens: List[String],
                                      chunkAcc: ListBuffer[V]): Pull[F, V, Pulled[F, T.Context, V]] =
    ensureNext(ctx, chunkAcc) {
      case Some((ctx, chunkAcc)) =>
        (T.current(ctx): @switch) match {
          case '\t' | '\r' | '\n' | ' ' =>
            betweenDoctypePublicAndSystemIdentifiers(T.advance(ctx), name, id, opens, chunkAcc)
          case '>' =>
            Pull.pure(
              Pulled[F, T.Context, V](T.advance(ctx),
                                      chunkAcc,
                                      opens,
                                      HtmlToken.Doctype(false, name, id.some, none).some,
                                      data(_, _, _)))
          case '"' =>
            doctypeSystemIdentifierDoubleQuoted(T.advance(ctx), name, id.some, new StringBuilder, opens, chunkAcc)
          case '\'' =>
            doctypeSystemIdentifierSingleQuoted(T.advance(ctx), name, id.some, new StringBuilder, opens, chunkAcc)
          case _ =>
            bogusDoctype(ctx, true, name, id.some, none, opens, chunkAcc)
        }
      case None =>
        Pull.pure(
          Pulled[F, T.Context, V](T.emptyContext,
                                  new ListBuffer,
                                  opens,
                                  HtmlToken.Doctype(true, name, id.some, none).some,
                                  (_, opens, _) => done(opens)))
    }

  def betweenDoctypePublicAndSystemIdentifiers[V](ctx: T.Context,
                                                  name: String,
                                                  id: String,
                                                  opens: List[String],
                                                  chunkAcc: ListBuffer[V]): Pull[F, V, Pulled[F, T.Context, V]] =
    ensureNext(ctx, chunkAcc) {
      case Some((ctx, chunkAcc)) =>
        (T.current(ctx): @switch) match {
          case '\t' | '\r' | '\n' | ' ' =>
            betweenDoctypePublicAndSystemIdentifiers(T.advance(ctx), name, id, opens, chunkAcc)
          case '>' =>
            Pull.pure(
              Pulled[F, T.Context, V](T.advance(ctx),
                                      chunkAcc,
                                      opens,
                                      HtmlToken.Doctype(false, name, id.some, none).some,
                                      data(_, _, _)))
          case '"' =>
            doctypeSystemIdentifierDoubleQuoted(T.advance(ctx), name, id.some, new StringBuilder, opens, chunkAcc)
          case '\'' =>
            doctypeSystemIdentifierSingleQuoted(T.advance(ctx), name, id.some, new StringBuilder, opens, chunkAcc)
          case _ =>
            bogusDoctype(ctx, true, name, id.some, none, opens, chunkAcc)
        }
      case None =>
        Pull.pure(
          Pulled[F, T.Context, V](T.emptyContext,
                                  new ListBuffer,
                                  opens,
                                  HtmlToken.Doctype(true, name, id.some, none).some,
                                  (_, opens, _) => done(opens)))
    }

  def afterDoctypeSystemKeyword[V](ctx: T.Context,
                                   name: String,
                                   opens: List[String],
                                   chunkAcc: ListBuffer[V]): Pull[F, V, Pulled[F, T.Context, V]] =
    ensureNext(ctx, chunkAcc) {
      case Some((ctx, chunkAcc)) =>
        (T.current(ctx): @switch) match {
          case '\t' | '\r' | '\n' | ' ' =>
            afterDoctypeSystemKeyword(T.advance(ctx), name, opens, chunkAcc)
          case '>' =>
            Pull.pure(
              Pulled[F, T.Context, V](T.advance(ctx),
                                      chunkAcc,
                                      opens,
                                      HtmlToken.Doctype(false, name, none, none).some,
                                      data(_, _, _)))
          case '"' =>
            doctypeSystemIdentifierDoubleQuoted(T.advance(ctx), name, none, new StringBuilder, opens, chunkAcc)
          case '\'' =>
            doctypeSystemIdentifierSingleQuoted(T.advance(ctx), name, none, new StringBuilder, opens, chunkAcc)
          case _ =>
            bogusDoctype(ctx, true, name, none, none, opens, chunkAcc)
        }
      case None =>
        Pull.pure(
          Pulled[F, T.Context, V](T.emptyContext,
                                  new ListBuffer,
                                  opens,
                                  HtmlToken.Doctype(true, name, none, none).some,
                                  (_, opens, _) => done(opens)))
    }

  def doctypeSystemIdentifierDoubleQuoted[V](ctx: T.Context,
                                             name: String,
                                             publicid: Option[String],
                                             id: StringBuilder,
                                             opens: List[String],
                                             chunkAcc: ListBuffer[V]): Pull[F, V, Pulled[F, T.Context, V]] =
    ensureNext(ctx, chunkAcc) {
      case Some((ctx, chunkAcc)) =>
        (T.current(ctx): @switch) match {
          case '"' =>
            afterDoctypeSystemIdentifier(T.advance(ctx), name, publicid, id.result(), opens, chunkAcc)
          case '\u0000' =>
            doctypeSystemIdentifierDoubleQuoted(T.advance(ctx), name, publicid, id.addOne('\ufffd'), opens, chunkAcc)
          case '>' =>
            Pull.pure(
              Pulled[F, T.Context, V](T.advance(ctx),
                                      chunkAcc,
                                      opens,
                                      HtmlToken.Doctype(true, name, publicid, id.result().some).some,
                                      data(_, _, _)))
          case c =>
            doctypeSystemIdentifierDoubleQuoted(T.advance(ctx), name, publicid, id.addOne(c), opens, chunkAcc)
        }
      case None =>
        Pull.pure(
          Pulled[F, T.Context, V](T.emptyContext,
                                  new ListBuffer,
                                  opens,
                                  HtmlToken.Doctype(true, name, publicid, id.result().some).some,
                                  (_, opens, _) => done(opens)))
    }

  def doctypeSystemIdentifierSingleQuoted[V](ctx: T.Context,
                                             name: String,
                                             publicid: Option[String],
                                             id: StringBuilder,
                                             opens: List[String],
                                             chunkAcc: ListBuffer[V]): Pull[F, V, Pulled[F, T.Context, V]] =
    ensureNext(ctx, chunkAcc) {
      case Some((ctx, chunkAcc)) =>
        (T.current(ctx): @switch) match {
          case '\'' =>
            afterDoctypeSystemIdentifier(T.advance(ctx), name, publicid, id.result(), opens, chunkAcc)
          case '\u0000' =>
            doctypeSystemIdentifierSingleQuoted(T.advance(ctx), name, publicid, id.addOne('\ufffd'), opens, chunkAcc)
          case '>' =>
            Pull.pure(
              Pulled[F, T.Context, V](T.advance(ctx),
                                      chunkAcc,
                                      opens,
                                      HtmlToken.Doctype(true, name, publicid, id.result().some).some,
                                      data(_, _, _)))
          case c =>
            doctypeSystemIdentifierSingleQuoted(T.advance(ctx), name, publicid, id.addOne(c), opens, chunkAcc)
        }
      case None =>
        Pull.pure(
          Pulled[F, T.Context, V](T.emptyContext,
                                  new ListBuffer,
                                  opens,
                                  HtmlToken.Doctype(true, name, publicid, id.result().some).some,
                                  (_, opens, _) => done(opens)))
    }

  def afterDoctypeSystemIdentifier[V](ctx: T.Context,
                                      name: String,
                                      publicid: Option[String],
                                      systemid: String,
                                      opens: List[String],
                                      chunkAcc: ListBuffer[V]): Pull[F, V, Pulled[F, T.Context, V]] =
    ensureNext(ctx, chunkAcc) {
      case Some((ctx, chunkAcc)) =>
        (T.current(ctx): @switch) match {
          case '\t' | '\r' | '\n' | ' ' =>
            afterDoctypeSystemIdentifier(T.advance(ctx), name, publicid, systemid, opens, chunkAcc)
          case '>' =>
            Pull.pure(
              Pulled[F, T.Context, V](T.advance(ctx),
                                      chunkAcc,
                                      opens,
                                      HtmlToken.Doctype(false, name, publicid, systemid.some).some,
                                      data(_, _, _)))
          case _ =>
            bogusDoctype(ctx, false, name, publicid, systemid.some, opens, chunkAcc)
        }
      case None =>
        Pull.pure(
          Pulled[F, T.Context, V](T.emptyContext,
                                  new ListBuffer,
                                  opens,
                                  HtmlToken.Doctype(true, name, publicid, systemid.some).some,
                                  (_, opens, _) => done(opens)))
    }

  def bogusDoctype[V](ctx: T.Context,
                      forceQuirks: Boolean,
                      name: String,
                      publicid: Option[String],
                      systemid: Option[String],
                      opens: List[String],
                      chunkAcc: ListBuffer[V]): Pull[F, V, Pulled[F, T.Context, V]] =
    ensureNext(ctx, chunkAcc) {
      case Some((ctx, chunkAcc)) =>
        (T.current(ctx): @switch) match {
          case '>' =>
            Pull.pure(
              Pulled[F, T.Context, V](T.advance(ctx),
                                      chunkAcc,
                                      opens,
                                      HtmlToken.Doctype(forceQuirks, name, publicid, systemid).some,
                                      data(_, _, _)))
          case _ =>
            bogusDoctype(T.advance(ctx), forceQuirks, name, publicid, systemid, opens, chunkAcc)
        }
      case None =>
        Pull.pure(
          Pulled[F, T.Context, V](T.emptyContext,
                                  new ListBuffer,
                                  opens,
                                  HtmlToken.Doctype(forceQuirks, name, publicid, systemid).some,
                                  (_, opens, _) => done(opens)))
    }

  def cdataSection[V](ctx: T.Context,
                      opens: List[String],
                      chunkAcc: ListBuffer[V]): Pull[F, V, Pulled[F, T.Context, V]] =
    ensureNextOrDone(ctx, opens, chunkAcc) { (ctx, chunkAcc) =>
      (T.current(ctx): @switch) match {
        case ']' => cdataSectionBracket(T.advance(ctx), opens, chunkAcc)
        case c =>
          Pull.pure(
            Pulled[F, T.Context, V](T.advance(ctx),
                                    chunkAcc,
                                    opens,
                                    HtmlToken.Character(c).some,
                                    cdataSection(_, _, _)))
      }
    }

  def cdataSectionBracket[V](ctx: T.Context,
                             opens: List[String],
                             chunkAcc: ListBuffer[V]): Pull[F, V, Pulled[F, T.Context, V]] =
    ensureNext(ctx, chunkAcc) {
      case Some((ctx, chunkAcc)) =>
        (T.current(ctx): @switch) match {
          case ']' => cdataSectionEnd(ctx, opens, chunkAcc)
          case _ =>
            Pull.pure(
              Pulled[F, T.Context, V](ctx, chunkAcc, opens, HtmlToken.Character(']').some, cdataSection(_, _, _)))
        }
      case None =>
        Pull.pure(Pulled(T.emptyContext, new ListBuffer, opens, HtmlToken.Character(']').some, cdataSection(_, _, _)))
    }

  def cdataSectionEnd[V](ctx: T.Context,
                         opens: List[String],
                         chunkAcc: ListBuffer[V]): Pull[F, V, Pulled[F, T.Context, V]] =
    ensureNext(ctx, chunkAcc) {
      case Some((ctx, chunkAcc)) =>
        (T.current(ctx): @switch) match {
          case ']' =>
            Pull.pure(
              Pulled[F, T.Context, V](T.advance(ctx),
                                      chunkAcc,
                                      opens,
                                      HtmlToken.Character(']').some,
                                      cdataSectionEnd(_, _, _)))
          case '>' => data(T.advance(ctx), opens, chunkAcc)
          case _ =>
            emitAll[V](List(HtmlToken.Character(']'), HtmlToken.Character(']')), cdataSection(_, _, _))(ctx,
                                                                                                        opens,
                                                                                                        chunkAcc)
        }
      case None =>
        emitAll[V](List(HtmlToken.Character(']'), HtmlToken.Character(']')), cdataSection(_, _, _))(T.emptyContext,
                                                                                                    opens,
                                                                                                    new ListBuffer)
    }

  def commentStart[V](ctx: T.Context,
                      content: StringBuilder,
                      opens: List[String],
                      chunkAcc: ListBuffer[V]): Pull[F, V, Pulled[F, T.Context, V]] =
    ensureNextOrDone(ctx, opens, chunkAcc) { (ctx, chunkAcc) =>
      (T.current(ctx): @switch) match {
        case '-' =>
          commentStartDash(T.advance(ctx), content, opens, chunkAcc)
        case '>' =>
          Pull.pure(
            Pulled[F, T.Context, V](T.advance(ctx),
                                    chunkAcc,
                                    opens,
                                    HtmlToken.Comment(content.result()).some,
                                    data(_, _, _)))
        case _ =>
          comment(ctx, content, opens, chunkAcc)
      }
    }

  def commentStartDash[V](ctx: T.Context,
                          content: StringBuilder,
                          opens: List[String],
                          chunkAcc: ListBuffer[V]): Pull[F, V, Pulled[F, T.Context, V]] =
    ensureNextOrDone(ctx, opens, chunkAcc) { (ctx, chunkAcc) =>
      (T.current(ctx): @switch) match {
        case '-' =>
          commentEnd(T.advance(ctx), content, opens, chunkAcc)
        case '>' =>
          Pull.pure(
            Pulled[F, T.Context, V](T.advance(ctx),
                                    chunkAcc,
                                    opens,
                                    HtmlToken.Comment(content.result()).some,
                                    data(_, _, _)))
        case _ =>
          comment(ctx, content.addOne('-'), opens, chunkAcc)
      }
    }

  def comment[V](ctx: T.Context,
                 content: StringBuilder,
                 opens: List[String],
                 chunkAcc: ListBuffer[V]): Pull[F, V, Pulled[F, T.Context, V]] =
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
        Pull.pure(
          Pulled[F, T.Context, V](T.emptyContext,
                                  new ListBuffer,
                                  opens,
                                  HtmlToken.Comment(content.result()).some,
                                  (_, opens, _) => done(opens)))
    }

  def commentLessThanSign[V](ctx: T.Context,
                             content: StringBuilder,
                             opens: List[String],
                             chunkAcc: ListBuffer[V]): Pull[F, V, Pulled[F, T.Context, V]] =
    ensureNextOrDone(ctx, opens, chunkAcc) { (ctx, chunkAcc) =>
      (T.current(ctx): @switch) match {
        case '!' =>
          commentLessThanSignBang(T.advance(ctx), content.addOne('!'), opens, chunkAcc)
        case '<' =>
          commentLessThanSign(T.advance(ctx), content.addOne('<'), opens, chunkAcc)
        case _ =>
          comment(ctx, content, opens, chunkAcc)
      }
    }

  def commentLessThanSignBang[V](ctx: T.Context,
                                 content: StringBuilder,
                                 opens: List[String],
                                 chunkAcc: ListBuffer[V]): Pull[F, V, Pulled[F, T.Context, V]] =
    ensureNextOrDone(ctx, opens, chunkAcc) { (ctx, chunkAcc) =>
      (T.current(ctx): @switch) match {
        case '-' =>
          commentLessThanSignBangDash(T.advance(ctx), content, opens, chunkAcc)
        case _ =>
          comment(ctx, content, opens, chunkAcc)
      }
    }

  def commentLessThanSignBangDash[V](ctx: T.Context,
                                     content: StringBuilder,
                                     opens: List[String],
                                     chunkAcc: ListBuffer[V]): Pull[F, V, Pulled[F, T.Context, V]] =
    ensureNextOrDone(ctx, opens, chunkAcc) { (ctx, chunkAcc) =>
      (T.current(ctx): @switch) match {
        case '-' =>
          commentLessThanSignBangDashDash(T.advance(ctx), content, opens, chunkAcc)
        case _ =>
          commentEndDash(ctx, content, opens, chunkAcc)
      }
    }

  def commentLessThanSignBangDashDash[V](ctx: T.Context,
                                         content: StringBuilder,
                                         opens: List[String],
                                         chunkAcc: ListBuffer[V]): Pull[F, V, Pulled[F, T.Context, V]] =
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

  def commentEndDash[V](ctx: T.Context,
                        content: StringBuilder,
                        opens: List[String],
                        chunkAcc: ListBuffer[V]): Pull[F, V, Pulled[F, T.Context, V]] =
    ensureNext(ctx, chunkAcc) {
      case Some((ctx, chunkAcc)) =>
        (T.current(ctx): @switch) match {
          case '-' =>
            commentEnd(T.advance(ctx), content, opens, chunkAcc)
          case _ =>
            comment(ctx, content.addOne('-'), opens, chunkAcc)
        }
      case None =>
        Pull.pure(
          Pulled[F, T.Context, V](T.emptyContext,
                                  new ListBuffer,
                                  opens,
                                  HtmlToken.Comment(content.result()).some,
                                  (_, opens, _) => done(opens)))
    }

  def commentEnd[V](ctx: T.Context,
                    content: StringBuilder,
                    opens: List[String],
                    chunkAcc: ListBuffer[V]): Pull[F, V, Pulled[F, T.Context, V]] =
    ensureNext(ctx, chunkAcc) {
      case Some((ctx, chunkAcc)) =>
        (T.current(ctx): @switch) match {
          case '>' =>
            Pull.pure(
              Pulled[F, T.Context, V](T.advance(ctx),
                                      chunkAcc,
                                      opens,
                                      HtmlToken.Comment(content.result()).some,
                                      data(_, _, _)))
          case '!' =>
            commentEndBang(T.advance(ctx), content, opens, chunkAcc)
          case '-' =>
            commentEnd(T.advance(ctx), content.addOne('-'), opens, chunkAcc)
          case _ =>
            comment(ctx, content.addAll("--"), opens, chunkAcc)
        }
      case None =>
        Pull.pure(
          Pulled[F, T.Context, V](T.emptyContext,
                                  new ListBuffer,
                                  opens,
                                  HtmlToken.Comment(content.result()).some,
                                  (_, opens, _) => done(opens)))
    }

  def commentEndBang[V](ctx: T.Context,
                        content: StringBuilder,
                        opens: List[String],
                        chunkAcc: ListBuffer[V]): Pull[F, V, Pulled[F, T.Context, V]] =
    ensureNext(ctx, chunkAcc) {
      case Some((ctx, chunkAcc)) =>
        (T.current(ctx): @switch) match {
          case '-' =>
            commentEndDash(ctx, content.addAll("--!"), opens, chunkAcc)
          case '>' =>
            Pull.pure(
              Pulled[F, T.Context, V](T.advance(ctx),
                                      chunkAcc,
                                      opens,
                                      HtmlToken.Comment(content.result()).some,
                                      data(_, _, _)))
        }
      case None =>
        Pull.pure(
          Pulled[F, T.Context, V](T.emptyContext,
                                  new ListBuffer,
                                  opens,
                                  HtmlToken.Comment(content.result()).some,
                                  (_, opens, _) => done(opens)))
    }

  def rcdataKont[V](opens: List[String])(ctx: T.Context,
                                         chunkAcc: ListBuffer[V],
                                         toFlush: String): Pull[F, V, Pulled[F, T.Context, V]] =
    emitAll(toFlush.map(HtmlToken.Character(_)).toList, rcdata[V](_, _, _))(ctx, opens, chunkAcc)

  def rcdata[V](ctx: T.Context, opens: List[String], chunkAcc: ListBuffer[V]): Pull[F, V, Pulled[F, T.Context, V]] =
    ensureNextOrDone(ctx, opens, chunkAcc) { (ctx, chunkAcc) =>
      (T.current(ctx): @switch) match {
        case '&' =>
          characterReference(T.advance(ctx), rcdataKont[V](opens), chunkAcc)
        case '<' => rcdataLessThanSign(T.advance(ctx), opens, chunkAcc)
        case '\u0000' =>
          Pull.pure(
            Pulled[F, T.Context, V](T.advance(ctx),
                                    chunkAcc,
                                    opens,
                                    HtmlToken.Character('\ufffd').some,
                                    rcdata(_, _, _)))
        case c =>
          Pull.pure(
            Pulled[F, T.Context, V](T.advance(ctx), chunkAcc, opens, HtmlToken.Character(c).some, rcdata(_, _, _)))
      }
    }

  def rawText[V](ctx: T.Context, opens: List[String], chunkAcc: ListBuffer[V]): Pull[F, V, Pulled[F, T.Context, V]] =
    ensureNextOrDone(ctx, opens, chunkAcc) { (ctx, chunkAcc) =>
      (T.current(ctx): @switch) match {
        case '&' => characterReference(T.advance(ctx), rcdataKont(opens), chunkAcc)
        case '<' => rawTextLessThanSign(T.advance(ctx), opens, chunkAcc)
        case '\u0000' =>
          Pull.pure(
            Pulled[F, T.Context, V](T.advance(ctx),
                                    chunkAcc,
                                    opens,
                                    HtmlToken.Character('\ufffd').some,
                                    rcdata(_, _, _)))
        case c =>
          Pull.pure(
            Pulled[F, T.Context, V](T.advance(ctx), chunkAcc, opens, HtmlToken.Character(c).some, rcdata(_, _, _)))
      }
    }

  def rcdataLessThanSign[V](ctx: T.Context,
                            opens: List[String],
                            chunkAcc: ListBuffer[V]): Pull[F, V, Pulled[F, T.Context, V]] =
    ensureNextOrDone(ctx, opens, chunkAcc) { (ctx, chunkAcc) =>
      (T.current(ctx): @switch) match {
        case '/' =>
          rcdataEndTagOpen(T.advance(ctx), new StringBuilder, opens, chunkAcc)
        case c =>
          Pull.pure(Pulled[F, T.Context, V](ctx, chunkAcc, opens, HtmlToken.Character('<').some, rcdata(_, _, _)))
      }
    }

  def rcdataEndTagOpen[V](ctx: T.Context,
                          buffer: StringBuilder,
                          opens: List[String],
                          chunkAcc: ListBuffer[V]): Pull[F, V, Pulled[F, T.Context, V]] =
    ensureNextOrDone(ctx, opens, chunkAcc) { (ctx, chunkAcc) =>
      val c = T.current(ctx)
      if (isAsciiAlpha(c)) {
        rcdataEndTagName(ctx, new StringBuilder, opens, chunkAcc)
      } else {
        emitAll[V](List(HtmlToken.Character('<'), HtmlToken.Character('/')), rcdata(_, _, _))(ctx, opens, chunkAcc)
      }
    }

  def rcdataEndTagName[V](ctx: T.Context,
                          name: StringBuilder,
                          opens: List[String],
                          chunkAcc: ListBuffer[V]): Pull[F, V, Pulled[F, T.Context, V]] =
    ensureNextOrDone(ctx, opens, chunkAcc) { (ctx, chunkAcc) =>
      (T.current(ctx): @switch) match {
        case '\t' | '\r' | '\n' | ' ' =>
          val n = name.result()
          opens match {
            case open :: _ if open == n =>
              beforeAttributeName(T.advance(ctx), false, n, Map.empty, opens, chunkAcc)
            case _ =>
              emitAll[V](
                HtmlToken.Character('<') :: HtmlToken.Character('/') :: name.map(HtmlToken.Character(_)).toList,
                rcdata(_, _, _))(ctx, opens, chunkAcc)
          }
        case '/' =>
          val n = name.result()
          opens match {
            case open :: _ if open == n =>
              selfClosingStartTag(T.advance(ctx), false, n, Map.empty, opens, chunkAcc)
            case _ =>
              emitAll[V](
                HtmlToken.Character('<') :: HtmlToken.Character('/') :: name.map(HtmlToken.Character(_)).toList,
                rcdata(_, _, _))(ctx, opens, chunkAcc)
          }
        case '>' =>
          val n = name.result()
          opens match {
            case open :: _ if open == n =>
              Pull.pure(
                Pulled[F, T.Context, V](T.advance(ctx), chunkAcc, opens, HtmlToken.EndTag(n).some, data(_, _, _)))
            case _ =>
              emitAll[V](
                HtmlToken.Character('<') :: HtmlToken.Character('/') :: name.map(HtmlToken.Character(_)).toList,
                rcdata(_, _, _))(ctx, opens, chunkAcc)
          }
        case c =>
          if (isAsciiAlpha(c))
            rcdataEndTagName(T.advance(ctx), name.addOne(c.toLower), opens, chunkAcc)
          else
            emitAll[V](HtmlToken.Character('<') :: HtmlToken.Character('/') :: name.map(HtmlToken.Character(_)).toList,
                       rcdata(_, _, _))(ctx, opens, chunkAcc)
      }
    }

  def rawTextLessThanSign[V](ctx: T.Context,
                             opens: List[String],
                             chunkAcc: ListBuffer[V]): Pull[F, V, Pulled[F, T.Context, V]] =
    ensureNextOrDone(ctx, opens, chunkAcc) { (ctx, chunkAcc) =>
      (T.current(ctx): @switch) match {
        case '/' =>
          rawTextEndTagOpen(T.advance(ctx), opens, chunkAcc)
        case c =>
          Pull.pure(Pulled[F, T.Context, V](ctx, chunkAcc, opens, HtmlToken.Character('<').some, rawText(_, _, _)))
      }
    }

  def rawTextEndTagOpen[V](ctx: T.Context,
                           opens: List[String],
                           chunkAcc: ListBuffer[V]): Pull[F, V, Pulled[F, T.Context, V]] =
    ensureNextOrDone(ctx, opens, chunkAcc) { (ctx, chunkAcc) =>
      val c = T.current(ctx)
      if (isAsciiAlpha(c))
        rawTextEndTagName(ctx, new StringBuilder, opens, chunkAcc)
      else
        emitAll[V](List(HtmlToken.Character('<'), HtmlToken.Character('/')), rawText(_, _, _))(ctx, opens, chunkAcc)
    }

  def rawTextEndTagName[V](ctx: T.Context,
                           name: StringBuilder,
                           opens: List[String],
                           chunkAcc: ListBuffer[V]): Pull[F, V, Pulled[F, T.Context, V]] =
    ensureNextOrDone(ctx, opens, chunkAcc) { (ctx, chunkAcc) =>
      (T.current(ctx): @switch) match {
        case '\t' | '\r' | '\n' | ' ' =>
          val n = name.result()
          opens match {
            case open :: _ if open == n =>
              beforeAttributeName(T.advance(ctx), false, n, Map.empty, opens, chunkAcc)
            case _ =>
              emitAll[V](
                HtmlToken.Character('<') :: HtmlToken.Character('/') :: name.map(HtmlToken.Character(_)).toList,
                rawText(_, _, _))(ctx, opens, chunkAcc)
          }
        case '/' =>
          val n = name.result()
          opens match {
            case open :: _ if open == n =>
              selfClosingStartTag(T.advance(ctx), false, n, Map.empty, opens, chunkAcc)
            case _ =>
              emitAll[V](
                HtmlToken.Character('<') :: HtmlToken.Character('/') :: name.map(HtmlToken.Character(_)).toList,
                rawText(_, _, _))(ctx, opens, chunkAcc)
          }
        case '>' =>
          val n = name.result()
          opens match {
            case open :: _ if open == n =>
              Pull.pure(
                Pulled[F, T.Context, V](T.advance(ctx), chunkAcc, opens, HtmlToken.EndTag(n).some, data(_, _, _)))
            case _ =>
              emitAll[V](
                HtmlToken.Character('<') :: HtmlToken.Character('/') :: name.map(HtmlToken.Character(_)).toList,
                rawText(_, _, _))(ctx, opens, chunkAcc)
          }
        case c =>
          if (isAsciiAlpha(c))
            rawTextEndTagName(T.advance(ctx), name.addOne(c.toLower), opens, chunkAcc)
          else
            emitAll[V](HtmlToken.Character('<') :: HtmlToken.Character('/') :: name.map(HtmlToken.Character(_)).toList,
                       rawText(_, _, _))(ctx, opens, chunkAcc)
      }
    }

  def scriptDataLessThanSign[V](ctx: T.Context,
                                opens: List[String],
                                chunkAcc: ListBuffer[V]): Pull[F, V, Pulled[F, T.Context, V]] =
    ensureNextOrDone(ctx, opens, chunkAcc) { (ctx, chunkAcc) =>
      (T.current(ctx): @switch) match {
        case '/' =>
          scriptDataEndTagOpen(T.advance(ctx), opens, chunkAcc)
        case '!' =>
          emitAll[V](List(HtmlToken.Character('<'), HtmlToken.Character('!')), scriptDataEscapeStart(_, _, _))(
            T.advance(ctx),
            opens,
            chunkAcc)
        case c =>
          Pull.pure(Pulled[F, T.Context, V](ctx, chunkAcc, opens, HtmlToken.Character('<').some, scriptData(_, _, _)))
      }
    }

  def scriptDataEndTagOpen[V](ctx: T.Context,
                              opens: List[String],
                              chunkAcc: ListBuffer[V]): Pull[F, V, Pulled[F, T.Context, V]] =
    ensureNextOrDone(ctx, opens, chunkAcc) { (ctx, chunkAcc) =>
      val c = T.current(ctx)
      if (isAsciiAlpha(c))
        scriptDataEndTagName(ctx, new StringBuilder, opens, chunkAcc)
      else
        emitAll[V](List(HtmlToken.Character('<'), HtmlToken.Character('/')), scriptData(_, _, _))(ctx, opens, chunkAcc)
    }

  def scriptDataEscapeStart[V](ctx: T.Context,
                               opens: List[String],
                               chunkAcc: ListBuffer[V]): Pull[F, V, Pulled[F, T.Context, V]] =
    ensureNextOrDone(ctx, opens, chunkAcc) { (ctx, chunkAcc) =>
      (T.current(ctx): @switch) match {
        case '-' =>
          scriptDataEscapeStartDash(T.advance(ctx), opens, chunkAcc)
        case _ =>
          scriptData(ctx, opens, chunkAcc)
      }
    }

  def scriptDataEscapeStartDash[V](ctx: T.Context,
                                   opens: List[String],
                                   chunkAcc: ListBuffer[V]): Pull[F, V, Pulled[F, T.Context, V]] =
    ensureNextOrDone(ctx, opens, chunkAcc) { (ctx, chunkAcc) =>
      (T.current(ctx): @switch) match {
        case '-' =>
          Pull.pure(
            Pulled[F, T.Context, V](T.advance(ctx),
                                    chunkAcc,
                                    opens,
                                    HtmlToken.Character('-').some,
                                    scriptDataEscapeStartDash(_, _, _)))
        case _ =>
          scriptData(ctx, opens, chunkAcc)
      }
    }

  def scriptDataEscaped[V](ctx: T.Context,
                           opens: List[String],
                           chunkAcc: ListBuffer[V]): Pull[F, V, Pulled[F, T.Context, V]] =
    ensureNextOrDone(ctx, opens, chunkAcc) { (ctx, chunkAcc) =>
      (T.current(ctx): @switch) match {
        case '-' =>
          Pull.pure(
            Pulled[F, T.Context, V](T.advance(ctx),
                                    chunkAcc,
                                    opens,
                                    HtmlToken.Character('-').some,
                                    scriptDataEscapedDash(_, _, _)))
        case '<' =>
          scriptDataEscapedLessThanSign(T.advance(ctx), opens, chunkAcc)
        case '\u0000' =>
          Pull.pure(
            Pulled[F, T.Context, V](T.advance(ctx),
                                    chunkAcc,
                                    opens,
                                    HtmlToken.Character('\ufffd').some,
                                    scriptDataEscaped(_, _, _)))
        case c =>
          Pull.pure(
            Pulled[F, T.Context, V](T.advance(ctx),
                                    chunkAcc,
                                    opens,
                                    HtmlToken.Character(c).some,
                                    scriptDataEscaped(_, _, _)))
      }
    }

  def scriptDataEscapedDash[V](ctx: T.Context,
                               opens: List[String],
                               chunkAcc: ListBuffer[V]): Pull[F, V, Pulled[F, T.Context, V]] =
    ensureNextOrDone(ctx, opens, chunkAcc) { (ctx, chunkAcc) =>
      (T.current(ctx): @switch) match {
        case '-' =>
          Pull.pure(
            Pulled[F, T.Context, V](T.advance(ctx),
                                    chunkAcc,
                                    opens,
                                    HtmlToken.Character('-').some,
                                    scriptDataEscapedDashDash(_, _, _)))
        case '<' =>
          scriptDataEscapedLessThanSign(T.advance(ctx), opens, chunkAcc)
        case '\u0000' =>
          Pull.pure(
            Pulled[F, T.Context, V](T.advance(ctx),
                                    chunkAcc,
                                    opens,
                                    HtmlToken.Character('\ufffd').some,
                                    scriptDataEscaped(_, _, _)))
        case c =>
          Pull.pure(
            Pulled[F, T.Context, V](T.advance(ctx),
                                    chunkAcc,
                                    opens,
                                    HtmlToken.Character(c).some,
                                    scriptDataEscaped(_, _, _)))
      }
    }

  def scriptDataEscapedDashDash[V](ctx: T.Context,
                                   opens: List[String],
                                   chunkAcc: ListBuffer[V]): Pull[F, V, Pulled[F, T.Context, V]] =
    ensureNextOrDone(ctx, opens, chunkAcc) { (ctx, chunkAcc) =>
      (T.current(ctx): @switch) match {
        case '-' =>
          Pull.pure(
            Pulled[F, T.Context, V](T.advance(ctx),
                                    chunkAcc,
                                    opens,
                                    HtmlToken.Character('-').some,
                                    scriptDataEscapedDashDash(_, _, _)))
        case '<' =>
          scriptDataEscapedLessThanSign(T.advance(ctx), opens, chunkAcc)
        case '>' =>
          Pull.pure(
            Pulled[F, T.Context, V](T.advance(ctx),
                                    chunkAcc,
                                    opens,
                                    HtmlToken.Character('>').some,
                                    scriptData(_, _, _)))
        case c =>
          Pull.pure(
            Pulled[F, T.Context, V](T.advance(ctx),
                                    chunkAcc,
                                    opens,
                                    HtmlToken.Character(c).some,
                                    scriptDataEscaped(_, _, _)))
      }
    }

  def scriptDataEscapedLessThanSign[V](ctx: T.Context,
                                       opens: List[String],
                                       chunkAcc: ListBuffer[V]): Pull[F, V, Pulled[F, T.Context, V]] =
    ensureNextOrDone(ctx, opens, chunkAcc) { (ctx, chunkAcc) =>
      (T.current(ctx): @switch) match {
        case '/' =>
          scriptDataEscapedEndTagOpen(T.advance(ctx), opens, chunkAcc)
        case c =>
          if (isAsciiAlpha(c))
            Pull.pure(
              Pulled[F, T.Context, V](ctx,
                                      chunkAcc,
                                      opens,
                                      HtmlToken.Character('<').some,
                                      scriptDataDoubleEscapeStart(_, new StringBuilder, _, _)))
          else
            Pull.pure(
              Pulled[F, T.Context, V](ctx, chunkAcc, opens, HtmlToken.Character('<').some, scriptDataEscaped(_, _, _)))
      }
    }

  def scriptDataEscapedEndTagOpen[V](ctx: T.Context,
                                     opens: List[String],
                                     chunkAcc: ListBuffer[V]): Pull[F, V, Pulled[F, T.Context, V]] =
    ensureNextOrDone(ctx, opens, chunkAcc) { (ctx, chunkAcc) =>
      val c = T.current(ctx)
      if (isAsciiAlpha(c))
        scriptDataEscapedEndTagName(ctx, new StringBuilder, opens, chunkAcc)
      else
        Pull.pure(
          Pulled[F, T.Context, V](ctx, chunkAcc, opens, HtmlToken.Character('<').some, scriptDataEscaped(_, _, _)))
    }

  def scriptDataEscapedEndTagName[V](ctx: T.Context,
                                     name: StringBuilder,
                                     opens: List[String],
                                     chunkAcc: ListBuffer[V]): Pull[F, V, Pulled[F, T.Context, V]] =
    ensureNextOrDone(ctx, opens, chunkAcc) { (ctx, chunkAcc) =>
      (T.current(ctx): @switch) match {
        case '\t' | '\r' | '\n' | ' ' =>
          val n = name.result()
          opens match {
            case open :: _ if open == n =>
              beforeAttributeName(T.advance(ctx), false, n, Map.empty, opens, chunkAcc)
            case _ =>
              emitAll[V](
                HtmlToken.Character('<') :: HtmlToken.Character('/') :: name.map(HtmlToken.Character(_)).toList,
                scriptDataEscaped(_, _, _))(ctx, opens, chunkAcc)
          }
        case '/' =>
          val n = name.result()
          opens match {
            case open :: _ if open == n =>
              selfClosingStartTag(T.advance(ctx), false, n, Map.empty, opens, chunkAcc)
            case _ =>
              emitAll[V](
                HtmlToken.Character('<') :: HtmlToken.Character('/') :: name.map(HtmlToken.Character(_)).toList,
                scriptDataEscaped(_, _, _))(ctx, opens, chunkAcc)
          }
        case '>' =>
          val n = name.result()
          opens match {
            case open :: _ if open == n =>
              Pull.pure(
                Pulled[F, T.Context, V](T.advance(ctx), chunkAcc, opens, HtmlToken.EndTag(n).some, data(_, _, _)))
            case _ =>
              emitAll[V](
                HtmlToken.Character('<') :: HtmlToken.Character('/') :: name.map(HtmlToken.Character(_)).toList,
                scriptDataEscaped(_, _, _))(ctx, opens, chunkAcc)
          }
        case c =>
          if (isAsciiAlpha(c))
            scriptDataEscapedEndTagName(T.advance(ctx), name.addOne(c.toLower), opens, chunkAcc)
          else
            emitAll[V](HtmlToken.Character('<') :: HtmlToken.Character('/') :: name.map(HtmlToken.Character(_)).toList,
                       scriptDataEscaped(_, _, _))(ctx, opens, chunkAcc)
      }
    }

  def scriptDataDoubleEscapeStart[V](ctx: T.Context,
                                     buffer: StringBuilder,
                                     opens: List[String],
                                     chunkAcc: ListBuffer[V]): Pull[F, V, Pulled[F, T.Context, V]] =
    ensureNextOrDone(ctx, opens, chunkAcc) { (ctx, chunkAcc) =>
      val c = T.current(ctx)
      (c: @switch) match {
        case '\t' | '\r' | '\n' | ' ' | '/' | '>' =>
          val b = buffer.result()
          (b: @switch) match {
            case "script" =>
              Pull.pure(
                Pulled[F, T.Context, V](T.advance(ctx),
                                        chunkAcc,
                                        opens,
                                        HtmlToken.Character(c).some,
                                        scriptDataDoubleEscaped(_, _, _)))
            case _ =>
              Pull.pure(
                Pulled[F, T.Context, V](T.advance(ctx),
                                        chunkAcc,
                                        opens,
                                        HtmlToken.Character(c).some,
                                        scriptDataEscaped(_, _, _)))
          }
        case c =>
          if (isAsciiAlpha(c))
            Pull.pure(
              Pulled(T.advance(ctx),
                     chunkAcc,
                     opens,
                     HtmlToken.Character(c).some,
                     scriptDataDoubleEscapeStart(_, buffer.addOne(c.toLower), _, _)))
          else
            scriptDataEscaped(ctx, opens, chunkAcc)
      }
    }

  def scriptDataDoubleEscaped[V](ctx: T.Context,
                                 opens: List[String],
                                 chunkAcc: ListBuffer[V]): Pull[F, V, Pulled[F, T.Context, V]] =
    ensureNextOrDone(ctx, opens, chunkAcc) { (ctx, chunkAcc) =>
      (T.current(ctx): @switch) match {
        case '-' =>
          Pull.pure(
            Pulled[F, T.Context, V](T.advance(ctx),
                                    chunkAcc,
                                    opens,
                                    HtmlToken.Character('-').some,
                                    scriptDataDoubleEscapedDash(_, _, _)))
        case '<' =>
          Pull.pure(
            Pulled[F, T.Context, V](T.advance(ctx),
                                    chunkAcc,
                                    opens,
                                    HtmlToken.Character('<').some,
                                    scriptDataDoubleEscapedLessThanSign(_, _, _)))
        case '\u0000' =>
          Pull.pure(
            Pulled[F, T.Context, V](T.advance(ctx),
                                    chunkAcc,
                                    opens,
                                    HtmlToken.Character('\ufffd').some,
                                    scriptDataDoubleEscaped(_, _, _)))
        case c =>
          Pull.pure(
            Pulled[F, T.Context, V](T.advance(ctx),
                                    chunkAcc,
                                    opens,
                                    HtmlToken.Character(c).some,
                                    scriptDataDoubleEscaped(_, _, _)))
      }
    }

  def scriptDataDoubleEscapedDash[V](ctx: T.Context,
                                     opens: List[String],
                                     chunkAcc: ListBuffer[V]): Pull[F, V, Pulled[F, T.Context, V]] =
    ensureNextOrDone(ctx, opens, chunkAcc) { (ctx, chunkAcc) =>
      (T.current(ctx): @switch) match {
        case '-' =>
          Pull.pure(
            Pulled[F, T.Context, V](T.advance(ctx),
                                    chunkAcc,
                                    opens,
                                    HtmlToken.Character('-').some,
                                    scriptDataDoubleEscapedDashDash(_, _, _)))
        case '<' =>
          Pull.pure(
            Pulled[F, T.Context, V](T.advance(ctx),
                                    chunkAcc,
                                    opens,
                                    HtmlToken.Character('<').some,
                                    scriptDataDoubleEscapedLessThanSign(_, _, _)))
        case '\u0000' =>
          Pull.pure(
            Pulled[F, T.Context, V](T.advance(ctx),
                                    chunkAcc,
                                    opens,
                                    HtmlToken.Character('\ufffd').some,
                                    scriptDataDoubleEscaped(_, _, _)))
        case c =>
          Pull.pure(
            Pulled[F, T.Context, V](T.advance(ctx),
                                    chunkAcc,
                                    opens,
                                    HtmlToken.Character(c).some,
                                    scriptDataDoubleEscaped(_, _, _)))
      }
    }

  def scriptDataDoubleEscapedLessThanSign[V](ctx: T.Context,
                                             opens: List[String],
                                             chunkAcc: ListBuffer[V]): Pull[F, V, Pulled[F, T.Context, V]] =
    ensureNextOrDone(ctx, opens, chunkAcc) { (ctx, chunkAcc) =>
      (T.current(ctx): @switch) match {
        case '/' =>
          Pull.pure(
            Pulled[F, T.Context, V](T.advance(ctx),
                                    chunkAcc,
                                    opens,
                                    HtmlToken.Character('/').some,
                                    scriptDataDoubleEscapeEnd(_, new StringBuilder, _, _)))
        case _ =>
          scriptDataDoubleEscaped(ctx, opens, chunkAcc)
      }
    }

  def scriptDataDoubleEscapeEnd[V](ctx: T.Context,
                                   buffer: StringBuilder,
                                   opens: List[String],
                                   chunkAcc: ListBuffer[V]): Pull[F, V, Pulled[F, T.Context, V]] =
    ensureNextOrDone(ctx, opens, chunkAcc) { (ctx, chunkAcc) =>
      val c = T.current(ctx)
      (c: @switch) match {
        case '\t' | '\r' | '\n' | ' ' | '/' | '>' =>
          val b = buffer.result()
          (b: @switch) match {
            case "script" =>
              Pull.pure(
                Pulled[F, T.Context, V](T.advance(ctx),
                                        chunkAcc,
                                        opens,
                                        HtmlToken.Character(c).some,
                                        scriptDataEscaped(_, _, _)))
            case _ =>
              Pull.pure(
                Pulled[F, T.Context, V](T.advance(ctx),
                                        chunkAcc,
                                        opens,
                                        HtmlToken.Character(c).some,
                                        scriptDataDoubleEscaped(_, _, _)))
          }
        case _ =>
          if (isAsciiAlpha(c))
            Pull.pure(
              Pulled[F, T.Context, V](T.advance(ctx),
                                      chunkAcc,
                                      opens,
                                      HtmlToken.Character(c).some,
                                      scriptDataDoubleEscapeEnd(_, buffer.addOne(c), _, _)))
          else
            scriptDataDoubleEscaped(ctx, opens, chunkAcc)
      }
    }

  def scriptDataDoubleEscapedDashDash[V](ctx: T.Context,
                                         opens: List[String],
                                         chunkAcc: ListBuffer[V]): Pull[F, V, Pulled[F, T.Context, V]] =
    ensureNextOrDone(ctx, opens, chunkAcc) { (ctx, chunkAcc) =>
      (T.current(ctx): @switch) match {
        case '-' =>
          Pull.pure(
            Pulled[F, T.Context, V](T.advance(ctx),
                                    chunkAcc,
                                    opens,
                                    HtmlToken.Character('-').some,
                                    scriptDataDoubleEscapedDashDash(_, _, _)))
        case '<' =>
          Pull.pure(
            Pulled[F, T.Context, V](T.advance(ctx),
                                    chunkAcc,
                                    opens,
                                    HtmlToken.Character('<').some,
                                    scriptDataDoubleEscapedLessThanSign(_, _, _)))
        case '>' =>
          Pull.pure(
            Pulled[F, T.Context, V](T.advance(ctx),
                                    chunkAcc,
                                    opens,
                                    HtmlToken.Character('>').some,
                                    scriptData(_, _, _)))
        case '\u0000' =>
          Pull.pure(
            Pulled[F, T.Context, V](T.advance(ctx),
                                    chunkAcc,
                                    opens,
                                    HtmlToken.Character('\ufffd').some,
                                    scriptDataDoubleEscaped(_, _, _)))
        case c =>
          Pull.pure(
            Pulled[F, T.Context, V](T.advance(ctx),
                                    chunkAcc,
                                    opens,
                                    HtmlToken.Character(c).some,
                                    scriptDataDoubleEscaped(_, _, _)))
      }
    }

  def scriptDataEndTagName[V](ctx: T.Context,
                              name: StringBuilder,
                              opens: List[String],
                              chunkAcc: ListBuffer[V]): Pull[F, V, Pulled[F, T.Context, V]] =
    ensureNextOrDone(ctx, opens, chunkAcc) { (ctx, chunkAcc) =>
      (T.current(ctx): @switch) match {
        case '\t' | '\r' | '\n' | ' ' =>
          val n = name.result()
          opens match {
            case open :: _ if open == n =>
              beforeAttributeName(T.advance(ctx), false, n, Map.empty, opens, chunkAcc)
            case _ =>
              emitAll[V](
                HtmlToken.Character('<') :: HtmlToken.Character('/') :: name.map(HtmlToken.Character(_)).toList,
                scriptData(_, _, _))(ctx, opens, chunkAcc)
          }
        case '/' =>
          val n = name.result()
          opens match {
            case open :: _ if open == n =>
              selfClosingStartTag(T.advance(ctx), false, n, Map.empty, opens, chunkAcc)
            case _ =>
              emitAll[V](
                HtmlToken.Character('<') :: HtmlToken.Character('/') :: name.map(HtmlToken.Character(_)).toList,
                scriptData(_, _, _))(ctx, opens, chunkAcc)
          }
        case '>' =>
          val n = name.result()
          opens match {
            case open :: _ if open == n =>
              Pull.pure(
                Pulled[F, T.Context, V](T.advance(ctx), chunkAcc, opens, HtmlToken.EndTag(n).some, data(_, _, _)))
            case _ =>
              emitAll[V](
                HtmlToken.Character('<') :: HtmlToken.Character('/') :: name.map(HtmlToken.Character(_)).toList,
                scriptData(_, _, _))(ctx, opens, chunkAcc)
          }
        case c =>
          emitAll[V](HtmlToken.Character('<') :: HtmlToken.Character('/') :: name.map(HtmlToken.Character(_)).toList,
                     scriptData(_, _, _))(ctx, opens, chunkAcc)
      }
    }

  def scriptData[V](ctx: T.Context, opens: List[String], chunkAcc: ListBuffer[V]): Pull[F, V, Pulled[F, T.Context, V]] =
    ensureNextOrDone(ctx, opens, chunkAcc) { (ctx, chunkAcc) =>
      (T.current(ctx): @switch) match {
        case '<' =>
          scriptDataLessThanSign(T.advance(ctx), opens, chunkAcc)
        case '\u0000' =>
          Pull.pure(
            Pulled[F, T.Context, V](T.advance(ctx),
                                    chunkAcc,
                                    opens,
                                    HtmlToken.Character('\ufffd').some,
                                    scriptData(_, _, _)))
        case c =>
          Pull.pure(
            Pulled[F, T.Context, V](T.advance(ctx), chunkAcc, opens, HtmlToken.Character(c).some, scriptData(_, _, _)))
      }
    }

  def plainText[V](ctx: T.Context, opens: List[String], chunkAcc: ListBuffer[V]): Pull[F, V, Pulled[F, T.Context, V]] =
    ensureNextOrDone(ctx, opens, chunkAcc) { (ctx, chunkAcc) =>
      (T.current(ctx): @switch) match {
        case '\u0000' =>
          Pull.pure(
            Pulled[F, T.Context, V](T.advance(ctx),
                                    chunkAcc,
                                    opens,
                                    HtmlToken.Character('\ufffd').some,
                                    plainText(_, _, _)))
        case c =>
          Pull.pure(
            Pulled[F, T.Context, V](T.advance(ctx), chunkAcc, opens, HtmlToken.Character(c).some, plainText(_, _, _)))
      }
    }

  def data[V](ctx: T.Context, opens: List[String], chunkAcc: ListBuffer[V]): Pull[F, V, Pulled[F, T.Context, V]] =
    ensureNextOrDone(ctx, opens, chunkAcc) { (ctx, chunkAcc) =>
      (T.current(ctx): @switch) match {
        case '&' =>
          def kont[V](ctx: T.Context, chunkAcc: ListBuffer[V], toFlush: String) =
            emitAll[V](toFlush.map(HtmlToken.Character(_)).toList, data(_, _, _))(ctx, opens, chunkAcc)

          characterReference(T.advance(ctx), kont, chunkAcc)
        case '<' => tagOpen(T.advance(ctx), opens, chunkAcc)
        case '\u0000' =>
          data(T.advance(ctx), opens, chunkAcc)
        case c =>
          Pull.pure(Pulled(T.advance(ctx), chunkAcc, opens, HtmlToken.Character(c).some, data(_, _, _)))
      }
    }

  def pipe: Pipe[F, T, HtmlToken] = { s =>
    Stream
      .suspend(Stream.emit(T.create(s)))
      .flatMap(ctx =>
        Pull
          .loop[F, HtmlToken, Pull[F, HtmlToken, Pulled[F, T.Context, HtmlToken]]] { pull =>
            pull.flatMap {
              case Pulled(ctx, chunkAcc, opens, Some(token), next) =>
                val opens1 = (token, opens) match {
                  case (HtmlToken.OpenTag(n, _, _), _) =>
                    n :: opens
                  case (HtmlToken.EndTag(n1), n2 :: opens) if n1 == n2 =>
                    opens
                  case _ =>
                    opens
                }
                Pull.pure(next(ctx, opens1, chunkAcc += token).some)
              case Pulled(_, _, _, None, _) =>
                Pull.pure(None)
            }
          }(data(ctx, Nil, new ListBuffer))
          .stream)
  }

  private def isAsciiUpperAlpha[V](c: Char) =
    'A' <= c && c <= 'Z'

  private def isAsciiAlpha[V](c: Char) =
    ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z')

  private val hexdigits = "0123456789abcdef"

  private def isAsciiHex[V](c: Char) =
    hexdigits.indexOf(c.toLower) >= 0

  private val digits = "0123456789"

  private def isAsciiDigit[V](c: Char) =
    digits.indexOf(c.toLower) >= 0

  private val nonchars = Set(0xfffe, 0xffff, 0x1fffe, 0x1ffff, 0x2fffe, 0x2ffff, 0x3fffe, 0x3ffff, 0x4fffe, 0x4ffff,
                             0x5fffe, 0x5ffff, 0x6fffe, 0x6ffff, 0x7fffe, 0x7ffff, 0x8fffe, 0x8ffff, 0x9fffe, 0x9ffff,
                             0xafffe, 0xaffff, 0xbfffe, 0xbffff, 0xcfffe, 0xcffff, 0xdfffe, 0xdffff, 0xefffe, 0xeffff,
                             0xffffe, 0xfffff, 0x10fffe, 0x10ffff)

  private def isNonCharacter[V](i: Int) =
    (0xfdd0 <= i && i <= 0xfdef) || nonchars.contains(i)

  private def isControl[V](i: Int) =
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
