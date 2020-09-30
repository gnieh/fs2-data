/*
 * Copyright 2019 Lucas Satabin
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package fs2
package data
package xml
package internals

import scala.collection.immutable.VectorBuilder

private[xml] object EventParser {

  // ==== utils ====

  val valueDelimiters = " \t\r\n<&"

  private def fail[F[_], R](prod: String, msg: String, chunkAcc: List[XmlEvent])(
      implicit F: RaiseThrowable[F]): Pull[F, XmlEvent, R] =
    emitChunk(chunkAcc) >> Pull.raiseError[F](new XmlException(XmlSyntax(prod), msg))

  private def peekChar[F[_]](ctx: Context[F]): Pull[F, XmlEvent, Option[Result[F, Char]]] =
    if (ctx.isEndOfChunk) {
      emitChunk(ctx.chunkAcc) >> ctx.rest.pull.uncons.flatMap {
        case Some((hd, tl)) => peekChar(Context(hd, tl))
        case None           => Pull.pure(None)
      }
    } else {
      Pull.pure(Some((ctx, ctx.chunk(ctx.idx))))
    }

  private def nextChar[F[_]](ctx: Context[F])(implicit F: RaiseThrowable[F]): Pull[F, XmlEvent, Result[F, Char]] =
    if (ctx.isEndOfChunk) {
      emitChunk(ctx.chunkAcc) >> ctx.rest.pull.uncons.flatMap {
        case Some((hd, tl)) => nextChar(Context(hd, tl))
        case None           => fail[F, Result[F, Char]]("1", "unexpected end of input", Nil)
      }
    } else {
      Pull.pure((ctx.nextIdx, ctx.chunk(ctx.idx)))
    }

  private def isValid(is11: Boolean, c: Int): Boolean =
    if (is11)
      // [#x1-#xD7FF] | [#xE000-#xFFFD] | [#x10000-#x10FFFF]
      (0x1 <= c && c <= 0xd7ff) || (0xe000 <= c && c <= 0xfffd) || (0x10000 <= c && c <= 0x10ffff)
    else
      // #x9 | #xA | #xD | [#x20-#xD7FF] | [#xE000-#xFFFD] | [#x10000-#x10FFFF]
      c == 0x9 || c == 0xa || c == 0xd || (0x20 <= c && c <= 0xd7ff) || (0xe000 <= c && c <= 0xfffd) || (0x10000 <= c && c <= 0x10ffff)

  private def isNCNameStart(c: Char): Boolean = {
    import java.lang.Character._
    getType(c).toByte match {
      case LOWERCASE_LETTER | UPPERCASE_LETTER | OTHER_LETTER | TITLECASE_LETTER | LETTER_NUMBER => true
      case _                                                                                     => c == '_'
    }
  }

  private def isNCNameChar(c: Char): Boolean = {
    import java.lang.Character._
    // The constants represent groups Mc, Me, Mn, Lm, and Nd.
    isNCNameStart(c) || (getType(c).toByte match {
      case COMBINING_SPACING_MARK | ENCLOSING_MARK | NON_SPACING_MARK | MODIFIER_LETTER | DECIMAL_DIGIT_NUMBER => true
      case _                                                                                                   => ".-·".contains(c)
    })
  }

  private def isXmlWhitespace(c: Char): Boolean =
    c == ' ' || c == '\t' || c == '\r' || c == '\n'

  private def accept[F[_]](ctx: Context[F], c: Char, error: String, msg: String)(
      implicit F: RaiseThrowable[F]): Pull[F, XmlEvent, Context[F]] =
    if (ctx.isEndOfChunk) {
      emitChunk(ctx.chunkAcc) >> ctx.rest.pull.uncons.flatMap {
        case Some((hd, tl)) => accept(Context(hd, tl), c, error, msg)
        case None           => fail[F, Context[F]](error, msg, Nil)
      }
    } else {
      if (ctx.chunk(ctx.idx) == c)
        Pull.pure(ctx.nextIdx)
      else
        fail[F, Context[F]](error, msg, ctx.chunkAcc)
    }

  private def accept[F[_]](ctx: Context[F], s: String)(
      implicit F: RaiseThrowable[F]): Pull[F, XmlEvent, Result[F, Int]] = {
    def loop(ctx: Context[F], sidx: Int): Pull[F, XmlEvent, Result[F, Int]] =
      if (sidx >= s.length) {
        Pull.pure((ctx, s.length))
      } else if (ctx.isEndOfChunk) {
        emitChunk(ctx.chunkAcc) >> ctx.rest.pull.uncons.flatMap {
          case Some((hd, tl)) => accept(Context(hd, tl), s)
          case None           => Pull.pure((Context.eos, sidx))
        }
      } else {
        if (ctx.chunk(ctx.idx) == s.charAt(sidx))
          loop(ctx.nextIdx, sidx + 1)
        else
          Pull.pure((ctx, sidx))
      }
    loop(ctx, 0)
  }

  private def accept[F[_]](ctx: Context[F], s: String, error: String, msg: String)(
      implicit F: RaiseThrowable[F]): Pull[F, XmlEvent, Context[F]] =
    accept(ctx, s).flatMap {
      case (ctx, n) if n == s.length => Pull.pure(ctx)
      case _                         => fail[F, Context[F]](error, msg, Nil)
    }

  private def assert[F[_]](ctx: Context[F], p: Char => Boolean, error: String, msg: String)(
      implicit F: RaiseThrowable[F]): Pull[F, XmlEvent, Result[F, Char]] =
    peekChar(ctx).flatMap {
      case Some((ctx, c)) if p(c) => Pull.pure((ctx.nextIdx, c))
      case _                      => fail[F, Result[F, Char]](error, msg, Nil)
    }

  private def untilChar[F[_]](ctx: Context[F], p: Char => Boolean, sb: StringBuilder): Pull[F, XmlEvent, Context[F]] =
    if (ctx.isEndOfChunk) {
      emitChunk(ctx.chunkAcc) >> ctx.rest.pull.uncons.flatMap {
        case Some((hd, tl)) => untilChar(Context(hd, tl), p, sb)
        case None           => Pull.pure(Context.eos)
      }
    } else {
      val c = ctx.chunk(ctx.idx)
      if (!p(c))
        untilChar(ctx.nextIdx, p, sb.append(c))
      else
        Pull.pure(ctx)
    }

  // ==== low-level internals ====

  private def readNCName[F[_]](ctx: Context[F])(implicit F: RaiseThrowable[F]): Pull[F, XmlEvent, Result[F, String]] =
    if (ctx.isEndOfChunk) {
      emitChunk(ctx.chunkAcc) >> ctx.rest.pull.uncons.flatMap {
        case Some((hd, tl)) => readNCName(Context(hd, tl))
        case None           => fail[F, Result[F, String]]("1", "unexpected end of input", Nil)
      }
    } else {
      val c = ctx.chunk(ctx.idx)
      if (isNCNameStart(c)) {
        val sb = new StringBuilder
        untilChar(ctx.nextIdx, c => !isNCNameChar(c), sb.append(c)).map { ctx =>
          (ctx, sb.result())
        }
      } else {
        fail[F, Result[F, String]]("5", s"character '$c' cannot start a NCName", ctx.chunkAcc)
      }
    }

  private def readQName[F[_]](ctx: Context[F])(implicit F: RaiseThrowable[F]): Pull[F, XmlEvent, Result[F, QName]] =
    readNCName(ctx).flatMap {
      case (ctx, part1) =>
        def readPart2(ctx: Context[F]): Pull[F, XmlEvent, Result[F, QName]] =
          if (ctx.isEndOfChunk) {
            emitChunk(ctx.chunkAcc) >> ctx.rest.pull.uncons.flatMap {
              case Some((hd, tl)) => readPart2(Context(hd, tl))
              case None           => Pull.pure((Context.eos, QName(None, part1)))
            }
          } else {
            ctx.chunk(ctx.idx) match {
              case ':' =>
                readNCName(ctx.nextIdx).map {
                  case (ctx, part2) =>
                    (ctx, QName(Some(part1), part2))
                }
              case _ =>
                Pull.pure((ctx, QName(None, part1)))
            }
          }
        readPart2(ctx)
    }

  private def space[F[_]](ctx: Context[F]): Pull[F, XmlEvent, Context[F]] =
    if (ctx.isEndOfChunk) {
      emitChunk(ctx.chunkAcc) >> ctx.rest.pull.uncons.flatMap {
        case Some((hd, tl)) => space(Context(hd, tl))
        case None           => Pull.pure(Context.eos)
      }
    } else {
      if (isXmlWhitespace(ctx.chunk(ctx.idx)))
        space(ctx.nextIdx)
      else
        Pull.pure(ctx)
    }
  private def readMarkupToken[F[_]](ctx: Context[F])(
      implicit F: RaiseThrowable[F]): Pull[F, XmlEvent, Result[F, MarkupToken]] =
    accept(ctx, '<', "43", "expected token start").flatMap { ctx =>
      def read(ctx: Context[F]): Pull[F, XmlEvent, Result[F, MarkupToken]] =
        if (ctx.isEndOfChunk) {
          emitChunk(ctx.chunkAcc) >> ctx.rest.pull.uncons.flatMap {
            case Some((hd, tl)) => read(Context(hd, tl))
            case None           => fail[F, Result[F, MarkupToken]]("1", "unexpected end of input", Nil)
          }
        } else {
          ctx.chunk(ctx.idx) match {
            case '/' =>
              for {
                (ctx, qname) <- readQName(ctx.nextIdx)
                ctx <- space(ctx)
                ctx <- accept(ctx, '>', "42", "missing '>' at the end of closing tag")
              } yield (ctx, MarkupToken.EndToken(qname))
            case '?' =>
              readNCName(ctx.nextIdx).map {
                case (ctx, name) => (ctx, MarkupToken.PIToken(name))
              }
            case '!' =>
              peekChar(ctx.nextIdx).flatMap {
                case Some((ctx, '-')) =>
                  skipComment(ctx.nextIdx)
                case Some((ctx, '[')) =>
                  readCDATA(ctx.nextIdx)
                case Some((ctx, _)) =>
                  readNCName(ctx).map {
                    case (ctx, name) =>
                      (ctx, MarkupToken.DeclToken(name))
                  }
                case None =>
                  fail[F, Result[F, MarkupToken]]("1", "unexpected end of input", Nil)
              }
            case _ =>
              readQName(ctx).map {
                case (ctx, name) => (ctx, MarkupToken.StartToken(name))
              }
          }
        }
      read(ctx)
    }

  /** We have read '<!-' so far */
  private def skipComment[F[_]](ctx: Context[F])(
      implicit F: RaiseThrowable[F]): Pull[F, XmlEvent, Result[F, MarkupToken]] =
    accept(ctx, '-', "15", "second dash missing to open comment").flatMap { ctx =>
      def loop(ctx: Context[F]): Pull[F, XmlEvent, Context[F]] =
        nextChar(ctx).flatMap {
          case (ctx, '-') =>
            nextChar(ctx).flatMap {
              case (ctx, '-') =>
                accept(ctx, '>', "15", "'--' is not inside comments")
              case (ctx, _) =>
                loop(ctx)
            }
          case (ctx, _) =>
            loop(ctx)
        }
      loop(ctx).map { ctx =>
        (ctx, MarkupToken.CommentToken)
      }
    }

  /** We have read '<![' so far */
  private def readCDATA[F[_]](ctx: Context[F])(
      implicit F: RaiseThrowable[F]): Pull[F, XmlEvent, Result[F, MarkupToken]] =
    accept(ctx, "CDATA[", "19", "'CDATA[' expected").map { ctx =>
      (ctx, MarkupToken.CDataToken)
    }

  /** We have just read the PI target */
  private def readPIBody[F[_]](ctx: Context[F])(implicit F: RaiseThrowable[F]): Pull[F, XmlEvent, Result[F, String]] =
    space(ctx).flatMap { ctx =>
      def loop(ctx: Context[F], sb: StringBuilder): Pull[F, XmlEvent, Result[F, String]] =
        untilChar(ctx, c => c == '?', sb).flatMap { ctx =>
          accept(ctx, '?', "16", "unexpected end of input").flatMap { ctx =>
            peekChar(ctx).flatMap {
              case Some((ctx, '>')) =>
                Pull.pure((ctx.nextIdx, sb.result()))
              case Some((ctx, _)) =>
                loop(ctx, sb.append('?'))
              case None =>
                fail[F, Result[F, String]]("16", "unexpected end of input", Nil)
            }
          }
        }
      loop(ctx, new StringBuilder)
    }

  /** We read the beginning of internal DTD subset, read until final ']>' */
  private def skipInternalDTD[F[_]](ctx: Context[F])(implicit F: RaiseThrowable[F]): Pull[F, XmlEvent, Context[F]] =
    nextChar(ctx).flatMap {
      case (ctx, ']') =>
        nextChar(ctx).flatMap {
          case (ctx, '>') => Pull.pure(ctx)
          case (ctx, _)   => skipInternalDTD(ctx)
        }
      case (ctx, _) => skipInternalDTD(ctx)
    }

  private def readExternalID[F[_]](ctx: Context[F])(
      implicit F: RaiseThrowable[F]): Pull[F, XmlEvent, Result[F, String]] =
    readNCName(ctx).flatMap {
      case (ctx, sysOrPub) =>
        assert(ctx, isXmlWhitespace(_), "75", "space required after SYSTEM or PUBLIC").flatMap {
          case (ctx, _) =>
            sysOrPub match {
              case "SYSTEM" =>
                readQuoted(ctx, false, "11")
              case "PUBLIC" =>
                for {
                  (ctx, _) <- readQuoted(ctx, true, "12")
                  (ctx, _) <- assert(ctx, isXmlWhitespace(_), "12", "space required after PubidLiteral")
                  res <- readQuoted(ctx, false, "12")
                } yield res
              case _ =>
                fail[F, Result[F, String]]("75", "SYSTEM or PUBLIC expected", ctx.chunkAcc)
            }
        }
    }

  private def readQuoted[F[_]](ctx: Context[F], pub: Boolean, error: String)(
      implicit F: RaiseThrowable[F]): Pull[F, XmlEvent, Result[F, String]] =
    space(ctx).flatMap { ctx =>
      assert(ctx, c => c == '"' || c == '\'', error, "single or double quote expected")
        .flatMap {
          case (ctx, delimiter) =>
            val pred: Char => Boolean =
              if (pub)
                if (delimiter == '\'')
                  c =>
                    !(c == 0x20 || c == 0xd || c == 0xa || ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z') || ('0' <= c && c <= '9') || "-'()+,./:=?;!*#@$_%"
                      .contains(c))
                else
                  c =>
                    !(c == 0x20 || c == 0xd || c == 0xa || ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z') || ('0' <= c && c <= '9') || "-()+,./:=?;!*#@$_%"
                      .contains(c))
              else
                c => c == delimiter

            val sb = new StringBuilder
            untilChar(ctx, pred, sb).flatMap { ctx =>
              Pull.pure((ctx.nextIdx, sb.result()))
            }
        }
    }

  private def scanMisc[F[_]](ctx: Context[F])(
      implicit F: RaiseThrowable[F]): Pull[F, XmlEvent, Option[Result[F, MarkupToken]]] =
    space(ctx).flatMap { ctx =>
      peekChar(ctx).flatMap {
        case Some((ctx, '<')) =>
          readMarkupToken(ctx).flatMap {
            case (ctx, MarkupToken.CommentToken)      => scanMisc(ctx)
            case res @ (_, MarkupToken.PIToken(_))    => Pull.pure(Some(res))
            case res @ (_, MarkupToken.DeclToken(_))  => Pull.pure(Some(res))
            case res @ (_, MarkupToken.StartToken(_)) => Pull.pure(Some(res))
            case (ctx, t)                             => fail[F, Option[Result[F, MarkupToken]]]("22", s"unexpected token '$t'", ctx.chunkAcc)
          }
        case Some((ctx, c)) => fail[F, Option[Result[F, MarkupToken]]]("22", s"unexpected character '$c'", ctx.chunkAcc)
        case None           => Pull.pure(None)
      }
    }

  /** We read '&#' so far */
  def readCharRef[F[_]](ctx: Context[F], is11: Boolean)(
      implicit F: RaiseThrowable[F]): Pull[F, XmlEvent, Result[F, Int]] = {
    def postlude(ctx: Context[F], n: Int) =
      nextChar(ctx).flatMap {
        case (ctx, ';') =>
          if (isValid(is11, n))
            Pull.pure((ctx, n))
          else
            fail[F, Result[F, Int]]("2", "invalid character", ctx.chunkAcc)
        case _ =>
          fail[F, Result[F, Int]]("66", "character reference must end with a semicolon", ctx.chunkAcc)
      }
    peekChar(ctx).flatMap {
      case Some((ctx, 'x')) =>
        readNum(ctx.nextIdx, 16).flatMap {
          case (ctx, n) => postlude(ctx, n)
        }
      case Some((ctx, _)) =>
        readNum(ctx, 10).flatMap {
          case (ctx, n) => postlude(ctx, n)
        }
      case None => fail[F, Result[F, Int]]("66", "unexpected end of input", Nil)
    }
  }

  private def readNum[F[_]](ctx: Context[F], base: Int)(
      implicit F: RaiseThrowable[F]): Pull[F, XmlEvent, Result[F, Int]] = {
    object Digit {
      def unapply(c: Char): Option[Int] =
        if ((base == 10 || base == 16) && '0' <= c && c <= '9')
          Some(c - '0')
        else if (base == 16 && 'a' <= c && c <= 'f')
          Some(c - 'a' + 10)
        else if (base == 16 && 'A' <= c && c <= 'F')
          Some(c - 'A' + 10)
        else
          None
    }

    def restNum(ctx: Context[F], acc: Int): Pull[F, XmlEvent, Result[F, Int]] =
      peekChar(ctx).flatMap {
        case Some((ctx, Digit(d))) =>
          restNum(ctx.nextIdx, acc * base + d)
        case Some((ctx, _)) =>
          Pull.pure((ctx, acc))
        case None =>
          Pull.pure((Context.eos, acc))
      }

    nextChar(ctx).flatMap {
      case (ctx, Digit(d)) => restNum(ctx, d)
      case (ctx, _)        => fail[F, Result[F, Int]]("66", "bad first character reference digit", ctx.chunkAcc)
    }
  }

  // ==== middle-level internals ====

  private def readAttributes[F[_]](ctx: Context[F], is11: Boolean, tname: QName)(
      implicit F: RaiseThrowable[F]): Pull[F, XmlEvent, Result[F, List[Attr]]] = {
    def loop(ctx: Context[F], attributes: VectorBuilder[Attr]): Pull[F, XmlEvent, Result[F, List[Attr]]] =
      space(ctx).flatMap { ctx =>
        peekChar(ctx).flatMap {
          case Some((ctx, c)) if isNCNameStart(c) =>
            for {
              (ctx, name) <- readQName(ctx)
              ctx <- space(ctx)
              ctx <- accept(ctx, '=', "25", "'=' character expected")
              ctx <- space(ctx)
              (ctx, delimiter) <- assert(ctx,
                                         c => c == '"' || c == '\'',
                                         "10",
                                         "single or double quote expected around attribute value")
              (ctx, value) <- readAttributeValue(ctx, is11, Some(delimiter), new StringBuilder, new VectorBuilder)
              res <- loop(ctx, attributes += Attr(name, value))
            } yield res
          case Some((ctx, _)) => Pull.pure((ctx, attributes.result().toList))
          case None           => fail[F, Result[F, List[Attr]]]("1", "unexpected end of input", Nil)
        }
      }
    loop(ctx, new VectorBuilder)
  }

  private def readAttributeValue[F[_]](ctx: Context[F],
                                       is11: Boolean,
                                       delim: Option[Char],
                                       current: StringBuilder,
                                       builder: VectorBuilder[XmlEvent.XmlTexty])(
      implicit F: RaiseThrowable[F]): Pull[F, XmlEvent, Result[F, List[XmlEvent.XmlTexty]]] = {
    val delimiters = delim.fold(valueDelimiters)(valueDelimiters + _)
    untilChar(ctx, delimiters.contains(_), current).flatMap { ctx =>
      nextChar(ctx).flatMap {
        case (ctx, c) if Some(c) == delim =>
          if (!current.isEmpty)
            builder += XmlEvent.XmlString(current.toString, false)
          Pull.pure((ctx, builder.result().toList))
        case (ctx, '\r') =>
          nextChar(ctx).flatMap {
            case (ctx, '\n') =>
              readAttributeValue(ctx, is11, delim, current.append('\n'), builder)
            case (ctx, c) =>
              readAttributeValue(ctx, is11, delim, current.append(' '), builder)
          }
        case ((ctx, c)) if isXmlWhitespace(c) =>
          readAttributeValue(ctx, is11, delim, current.append(' '), builder)
        case (ctx, '&') =>
          builder += XmlEvent.XmlString(current.toString, false)
          peekChar(ctx).flatMap {
            case Some((ctx, '#')) =>
              readCharRef(ctx.nextIdx, is11).flatMap {
                case (ctx, n) =>
                  builder += XmlEvent.XmlCharRef(n)
                  readAttributeValue(ctx, is11, delim, new StringBuilder, builder)
              }
            case Some((ctx, _)) =>
              readNamedEntity(ctx).flatMap {
                case (ctx, s) =>
                  builder += XmlEvent.XmlEntityRef(s)
                  readAttributeValue(ctx, is11, delim, new StringBuilder, builder)
              }
            case None =>
              fail[F, Result[F, List[XmlEvent.XmlTexty]]]("1", "unexpected end of input", Nil)
          }
        case (ctx, c) =>
          fail[F, Result[F, List[XmlEvent.XmlTexty]]]("10", s"unexpected character '$c'", ctx.chunkAcc)
      }
    }
  }

  private def readNamedEntity[F[_]](ctx: Context[F])(
      implicit F: RaiseThrowable[F]): Pull[F, XmlEvent, Result[F, String]] =
    readNCName(ctx).flatMap {
      case (ctx, name) =>
        accept(ctx, ';', "68", "named entity must end with a semicolon").map { ctx =>
          (ctx, name)
        }
    }

  private def completeStartTag[F[_]](ctx: Context[F], is11: Boolean, name: QName)(
      implicit F: RaiseThrowable[F]): Pull[F, XmlEvent, Result[F, XmlEvent.StartTag]] =
    readAttributes(ctx, is11, name).flatMap {
      case (ctx, attributes) =>
        space(ctx).flatMap { ctx =>
          for {
            (ctx, isEmpty) <- peekChar(ctx).flatMap {
              case Some((ctx, '/')) => Pull.pure((ctx.nextIdx, true))
              case Some((ctx, _))   => Pull.pure((ctx, false))
              case None             => fail[F, Result[F, Boolean]]("44", "unexpected end of input", Nil)
            }
            ctx <- accept(ctx, '>', "44", "missing closing '>'")

          } yield (ctx, XmlEvent.StartTag(name, attributes, isEmpty))
        }
    }

  /** We read '<[CDATA[' so far */
  private def readCDATABody[F[_]](ctx: Context[F], sb: StringBuilder)(
      implicit F: RaiseThrowable[F]): Pull[F, XmlEvent, Result[F, String]] =
    untilChar(ctx, c => c == '\n' || c == '\r' || c == ']' || c == '&', sb).flatMap { ctx =>
      nextChar(ctx).flatMap {
        case (ctx, '\n') =>
          readCDATABody(ctx, sb.append('\n'))
        case (ctx, ']') =>
          peekChar(ctx).flatMap {
            case Some((ctx, ']')) =>
              checkCDATAEnd(ctx.nextIdx, sb).flatMap {
                case (ctx, true)  => Pull.pure((ctx, sb.result()))
                case (ctx, false) => readCDATABody(ctx, sb)
              }
            case Some((ctx, _)) =>
              readCDATABody(ctx, sb.append(']'))
            case None =>
              fail[F, Result[F, String]]("1", "unexpected end of input", Nil)
          }
        case (ctx, '&') =>
          accept(ctx, "gt;").flatMap {
            case (ctx, n) =>
              if (n == 3) {
                sb.append('>')
              } else {
                sb.append('&')
                for (i <- 0 until n)
                  sb.append("gt;".charAt(i))
              }
              readCDATABody(ctx, sb)
          }
        case (ctx, _) =>
          // must be '\r'
          peekChar(ctx).flatMap {
            case Some((ctx, c)) =>
              if (c == '\n')
                readCDATABody(ctx.nextIdx, sb.append('\n'))
              else
                readCDATABody(ctx, sb.append(' '))
            case None =>
              fail[F, Result[F, String]]("1", "unexpected end of input", Nil)
          }
      }
    }

  def checkCDATAEnd[F[_]](ctx: Context[F], sb: StringBuilder)(
      implicit F: RaiseThrowable[F]): Pull[F, XmlEvent, Result[F, Boolean]] =
    peekChar(ctx).flatMap {
      case Some((ctx, '>')) =>
        // done
        Pull.pure((ctx.nextIdx, true))
      case Some((ctx, ']')) =>
        checkCDATAEnd(ctx.nextIdx, sb.append(']'))
      case Some((ctx, _)) =>
        sb.append("]]")
        Pull.pure((ctx, false))
      case None =>
        fail[F, Result[F, Boolean]]("1", "unexpected end of input", Nil)
    }

  private def readCharData[F[_]](ctx: Context[F], is11: Boolean)(
      implicit F: RaiseThrowable[F]): Pull[F, XmlEvent, Result[F, XmlEvent]] =
    peekChar(ctx).flatMap {
      case Some((ctx, '<')) =>
        readMarkupToken(ctx).flatMap {
          case (ctx, MarkupToken.CommentToken) =>
            readCharData(ctx, is11)
          case (ctx, MarkupToken.DeclToken(n)) =>
            fail[F, Result[F, XmlEvent]]("14", s"unexpected declaration '$n'", ctx.chunkAcc)
          case (ctx, MarkupToken.CDataToken) =>
            readCDATABody(ctx, new StringBuilder).map {
              case (ctx, body) => (ctx, XmlEvent.XmlString(body, true))
            }
          case (ctx, MarkupToken.EndToken(name)) =>
            Pull.pure((ctx, XmlEvent.EndTag(name)))
          case (ctx, MarkupToken.StartToken(name)) =>
            completeStartTag(ctx, is11, name)
          case (ctx, MarkupToken.PIToken(target)) if !target.equalsIgnoreCase("xml") =>
            readPIBody(ctx).flatMap {
              case (ctx, body) =>
                Pull.pure((ctx, XmlEvent.XmlPI(target, body)))
            }
          case (ctx, t) =>
            fail[F, Result[F, XmlEvent]]("43", s"unexpected token ${t.render}", ctx.chunkAcc)
        }
      case Some((ctx, '&')) =>
        peekChar(ctx.nextIdx).flatMap {
          case Some((ctx, '#')) =>
            readCharRef(ctx.nextIdx, is11).map {
              case (ctx, n) => (ctx, XmlEvent.XmlCharRef(n))
            }
          case Some((ctx, _)) =>
            readNamedEntity(ctx).map {
              case (ctx, v) => (ctx, XmlEvent.XmlEntityRef(v))
            }
          case None =>
            fail[F, Result[F, XmlEvent]]("1", "unexpected end of input", Nil)
        }
      case Some((ctx, _)) =>
        slowPath(ctx, new StringBuilder)
      case None =>
        Pull.pure((Context.eos, XmlEvent.EndDocument))
    }

  private def slowPath[F[_]](ctx: Context[F], sb: StringBuilder)(
      implicit F: RaiseThrowable[F]): Pull[F, XmlEvent, Result[F, XmlEvent.XmlString]] =
    untilChar(ctx, c => c == '<' || c == '&' || c == '\r', sb).flatMap { ctx =>
      peekChar(ctx).flatMap {
        case Some((ctx, '<')) => Pull.pure((ctx, XmlEvent.XmlString(sb.toString, false)))
        case None             => Pull.pure((ctx, XmlEvent.XmlString(sb.toString, false)))
        case Some((ctx, '&')) => Pull.pure((ctx, XmlEvent.XmlString(sb.toString, false)))
        case Some((ctx, _)) =>
          peekChar(ctx.nextIdx).flatMap {
            case Some((ctx, '\n')) =>
              sb.append('\n')
              slowPath(ctx.nextIdx, sb)
            case Some((ctx, _)) =>
              sb.append('\n')
              slowPath(ctx, sb)
            case None =>
              fail[F, Result[F, XmlEvent.XmlString]]("14", "unexpected end of input", Nil)
          }
      }

    }

  // ==== high-level internals

  private def scanPrologToken0[F[_]](ctx: Context[F])(
      implicit F: RaiseThrowable[F]): Pull[F, XmlEvent, Option[Context[F]]] =
    peekChar(ctx).flatMap {
      case Some((ctx, '<')) =>
        readMarkupToken(ctx).flatMap {
          case (ctx, MarkupToken.PIToken(name)) if name.equalsIgnoreCase("xml") =>
            handleXmlDecl(ctx).flatMap {
              case (ctx, (is11, decl)) => scanPrologToken1(ctx.accumulate(decl), is11)
            }
          case (ctx, MarkupToken.PIToken(name)) =>
            readPIBody(ctx).flatMap {
              case (ctx, body) => scanPrologToken1(ctx.accumulate(XmlEvent.XmlPI(name, body)), false)
            }
          case (ctx, MarkupToken.DeclToken(name)) =>
            handleDecl(ctx, name).flatMap {
              case ctx => scanPrologToken2(ctx, false)
            }
          case (ctx, MarkupToken.StartToken(name)) =>
            readElement(ctx, false, name).map(Some(_))
          case (ctx, MarkupToken.CommentToken) =>
            scanPrologToken1(ctx, false)
          case (ctx, t) =>
            fail[F, Option[Context[F]]]("22", s"unexpected markup $t", ctx.chunkAcc)
        }
      case Some((ctx, _)) =>
        scanPrologToken1(ctx, false)
      case None =>
        Pull.pure(None)
    }

  private def scanPrologToken1[F[_]](ctx: Context[F], is11: Boolean)(
      implicit F: RaiseThrowable[F]): Pull[F, XmlEvent, Option[Context[F]]] =
    scanMisc(ctx).flatMap {
      case Some((ctx, MarkupToken.PIToken(name))) if !name.equalsIgnoreCase("xml") =>
        readPIBody(ctx).flatMap {
          case (ctx, body) => scanPrologToken1(ctx.accumulate(XmlEvent.XmlPI(name, body)), is11)
        }
      case Some((ctx, MarkupToken.DeclToken(name))) =>
        handleDecl(ctx, name).flatMap {
          case ctx => scanPrologToken2(ctx, is11)
        }
      case Some((ctx, MarkupToken.StartToken(name))) =>
        readElement(ctx, is11, name).map(Some(_))
      case Some((ctx, t)) =>
        fail[F, Option[Context[F]]]("22", s"unexpected markup $t", ctx.chunkAcc)
      case None =>
        Pull.pure(None)
    }

  private def handleXmlDecl[F[_]](ctx: Context[F])(
      implicit F: RaiseThrowable[F]): Pull[F, XmlEvent, Result[F, (Boolean, XmlEvent.XmlDecl)]] =
    for {
      (ctx, _) <- assert(ctx, isXmlWhitespace(_), "24", "space is expected after xml")
      ctx <- space(ctx)
      ctx <- accept(ctx, "version", "24", "expected 'version' attribute")
      ctx <- space(ctx)
      ctx <- accept(ctx, '=', "24", "expected '=' after version")
      ctx <- space(ctx)
      (ctx, delimiter) <- assert(ctx, c => c == '"' || c == '\'', "24", "simple or double quote expected")
      ctx <- accept(ctx, '1', "26", "expected major version 1")
      ctx <- accept(ctx, '.', "26", "expected dot")
      sb = new StringBuilder("1.")
      ctx <- untilChar(ctx, !_.isDigit, sb)
      version = sb.result()
      res <- if (version.length == 2) {
        fail[F, Result[F, (Boolean, XmlEvent.XmlDecl)]]("26", "expected non empty minor version", ctx.chunkAcc)
      } else {
        for {
          ctx <- accept(ctx, delimiter, "24", "expected delimiter to close version attribute value")
          (ctx, (hasSpace, encoding)) <- readEncoding(ctx, false)
          (ctx, standalone) <- readStandalone(ctx, hasSpace)
          ctx <- space(ctx)
          ctx <- accept(ctx, "?>", "23", "expected end of PI")
        } yield (ctx, (version == "1.1", XmlEvent.XmlDecl(version, encoding, standalone)))
      }
    } yield res

  private def readEncoding[F[_]](ctx: Context[F], hasSpace: Boolean)(
      implicit F: RaiseThrowable[F]): Pull[F, XmlEvent, Result[F, (Boolean, Option[String])]] =
    peekChar(ctx).flatMap {
      case Some((ctx, c)) if isXmlWhitespace(c) =>
        space(ctx).flatMap(readEncoding(_, true))
      case Some((ctx, 'e')) =>
        if (hasSpace) {
          for {
            ctx <- accept(ctx, "encoding", "80", "expected 'encoding' attribute")
            ctx <- space(ctx)
            ctx <- accept(ctx, '=', "80", "expected '='")
            ctx <- space(ctx)
            (ctx, delimiter) <- assert(ctx, c => c == '"' || c == '\'', "80", "simple or double quote expected")
            (ctx, fst) <- assert(ctx,
                                 c => (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z'),
                                 "81",
                                 "wrong encoding name character")
            sb = new StringBuilder().append(fst)
            ctx <- untilChar(
              ctx,
              c =>
                !((c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9') || c == '.' || c == '_' || c == '-'),
              sb)
            ctx <- accept(ctx, delimiter, "80", "'encoding' attribute value must end with proper delimiter")
          } yield (ctx, (false, Some(sb.result())))
        } else {
          fail[F, Result[F, (Boolean, Option[String])]]("80",
                                                        "expected space before 'encoding' attribute",
                                                        ctx.chunkAcc)
        }
      case Some((ctx, _)) =>
        Pull.pure((ctx, (hasSpace, None)))
      case None =>
        Pull.pure((Context.eos, (hasSpace, None)))
    }

  private def readStandalone[F[_]](ctx: Context[F], hasSpace: Boolean)(
      implicit F: RaiseThrowable[F]): Pull[F, XmlEvent, Result[F, Option[Boolean]]] =
    peekChar(ctx).flatMap {
      case Some((ctx, c)) if isXmlWhitespace(c) =>
        space(ctx).flatMap(readStandalone(_, true))
      case Some((ctx, 's')) =>
        if (hasSpace) {
          for {
            ctx <- accept(ctx, "standalone", "32", "expected 'standalone' attribute")
            ctx <- space(ctx)
            ctx <- accept(ctx, '=', "32", "expected '='")
            ctx <- space(ctx)
            (ctx, delimiter) <- assert(ctx, c => c == '"' || c == '\'', "32", "simple or double quote expected")
            (ctx, sa) <- nextChar(ctx).flatMap {
              case (ctx, 'y') =>
                accept(ctx, "es", "32", "expected 'yes' or 'no'").map { ctx =>
                  (ctx, true)
                }
              case (ctx, 'n') =>
                accept(ctx, 'o', "32", "expected 'yes' or 'no'").map { ctx =>
                  (ctx, false)
                }
              case (ctx, _) => fail[F, Result[F, Boolean]]("32", "expected 'yes' or 'no'", ctx.chunkAcc)
            }
            ctx <- accept(ctx, delimiter, "32", "'standalone' attribute value must end with proper delimiter")
          } yield (ctx, Some(sa))
        } else {
          fail[F, Result[F, Option[Boolean]]]("32", "expected space before 'standalone' attribute", ctx.chunkAcc)
        }
      case Some((ctx, _)) => Pull.pure((ctx, None))
      case None           => Pull.pure((Context.eos, None))
    }

  private def handleDecl[F[_]](ctx: Context[F], name: String)(
      implicit F: RaiseThrowable[F]): Pull[F, XmlEvent, Context[F]] =
    name match {
      case "DOCTYPE" =>
        for {
          (ctx, _) <- assert(ctx, isXmlWhitespace(_), "28", "space is expected after DOCTYPE")
          ctx <- space(ctx)
          (ctx, docname) <- readNCName(ctx)
          ctx <- space(ctx)
          (ctx, systemid) <- peekChar(ctx).flatMap {
            case Some((ctx, c)) if isNCNameStart(c) => readExternalID(ctx).map { case (ctx, name) => (ctx, Some(name)) }
            case Some((ctx, c))                     => Pull.pure((ctx, None))
            case None                               => Pull.pure((Context.eos[F], None))
          }
          ctx <- space(ctx)
          res <- nextChar(ctx).flatMap {
            case (ctx, '>') =>
              // done
              Pull.pure(ctx.accumulate(XmlEvent.XmlDoctype(name, docname, systemid)))
            case (ctx, '[') =>
              skipInternalDTD(ctx).map { ctx =>
                ctx.accumulate(XmlEvent.XmlDoctype(name, docname, systemid))
              }
            case (ctx, c) =>
              fail[F, Context[F]]("28", s"end of doctype or internal DTD expected but got $c", ctx.chunkAcc)
          }
        } yield res
      case _ =>
        fail[F, Context[F]]("22", "expected DOCTYPE declaration", ctx.chunkAcc)
    }

  private def scanPrologToken2[F[_]](ctx: Context[F], is11: Boolean)(
      implicit F: RaiseThrowable[F]): Pull[F, XmlEvent, Option[Context[F]]] =
    scanMisc(ctx).flatMap {
      case Some((ctx, MarkupToken.PIToken(name))) =>
        readPIBody(ctx).flatMap {
          case (ctx, body) => scanPrologToken2(ctx.accumulate(XmlEvent.XmlPI(name, body)), is11)
        }
      case Some((ctx, MarkupToken.StartToken(name))) =>
        readElement(ctx, is11, name).map(Some(_))
      case Some((ctx, t)) =>
        fail[F, Option[Context[F]]]("22", s"unexpected markup $t", ctx.chunkAcc)
      case None =>
        Pull.pure(None)
    }

  private def readElement[F[_]](ctx: Context[F], is11: Boolean, name: QName)(
      implicit F: RaiseThrowable[F]): Pull[F, XmlEvent, Context[F]] =
    for {
      (ctx, startTag) <- completeStartTag(ctx, is11, name)
      ctx <- if (startTag.isEmpty)
        Pull.pure(ctx.accumulate(startTag, XmlEvent.EndTag(name)))
      else
        readContent(ctx.accumulate(startTag), is11, name)
    } yield ctx

  private def readContent[F[_]](ctx: Context[F], is11: Boolean, name: QName)(
      implicit F: RaiseThrowable[F]): Pull[F, XmlEvent, Context[F]] =
    for {
      (ctx, last) <- readCharData(ctx, is11)
      ctx <- last match {
        case XmlEvent.EndTag(n) if n == name =>
          // we are done reading that content
          Pull.pure(ctx.accumulate(last))
        case XmlEvent.EndTag(n) =>
          fail[F, Context[F]]("GIMatch",
                              s"unexpected closing tag '</${n.render}>' (expected '</${name.render}>')",
                              ctx.chunkAcc)
        case XmlEvent.StartTag(name1, _, false) =>
          // parse child element, and continue
          readContent(ctx.accumulate(last), is11, name1).flatMap(ctx => readContent(ctx, is11, name))
        case XmlEvent.StartTag(name1, _, true) =>
          // parse child element, and continue
          readContent(ctx.accumulate(last, XmlEvent.EndTag(name1)), is11, name)
        case _ =>
          // just emit and continue
          readContent(ctx.accumulate(last), is11, name)
      }
    } yield ctx

  def pipe[F[_]](implicit F: RaiseThrowable[F]): Pipe[F, Char, XmlEvent] = {
    def go(ctx: Context[F]): Pull[F, XmlEvent, Unit] =
      scanPrologToken0(ctx).flatMap {
        case Some(ctx) => go(ctx)
        case None      => Pull.done
      }
    s => go(Context(Chunk.empty, 0, s, Nil)).stream
  }

}
