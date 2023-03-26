/*
 * Copyright 2023 Lucas Satabin
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

import text._

import cats.syntax.all._

import scala.collection.immutable.VectorBuilder

private[xml] object EventParser {

  // ==== utils ====

  val valueDelimiters = " \t\r\n<&"

  def pipe[F[_], T](
      includeComments: Boolean)(implicit F: RaiseThrowable[F], T: CharLikeChunks[F, T]): Pipe[F, T, XmlEvent] = {

    val eos = T.create(Stream.empty)

    def fail[R](prod: String, msg: String, chunkAcc: Option[VectorBuilder[XmlEvent]]): Pull[F, XmlEvent, R] =
      emitChunk(chunkAcc) >> Pull.raiseError[F](new XmlException(XmlSyntax(prod), msg))

    def peekChar(
        ctx: T.Context,
        chunkAcc: VectorBuilder[XmlEvent]): Pull[F, XmlEvent, Option[(T.Context, VectorBuilder[XmlEvent], Char)]] =
      if (T.needsPull(ctx)) {
        emitChunk(Some(chunkAcc)) >> T.pullNext(ctx).flatMap {
          case Some(ctx) =>
            chunkAcc.clear()
            peekChar(ctx, chunkAcc)
          case None => Pull.pure(None)
        }
      } else {
        Pull.pure(Some((ctx, chunkAcc, T.current(ctx))))
      }

    def nextChar(ctx: T.Context,
                 chunkAcc: VectorBuilder[XmlEvent]): Pull[F, XmlEvent, (T.Context, VectorBuilder[XmlEvent], Char)] =
      if (T.needsPull(ctx)) {
        emitChunk(Some(chunkAcc)) >> T.pullNext(ctx).flatMap {
          case Some(ctx) =>
            chunkAcc.clear()
            nextChar(ctx, chunkAcc)
          case None => fail("1", "unexpected end of input", None)
        }
      } else {
        val c = T.current(ctx)
        Pull.pure((T.advance(ctx), chunkAcc, c))
      }

    def isValid(is11: Boolean, c: Int): Boolean =
      if (is11)
        // [#x1-#xD7FF] | [#xE000-#xFFFD] | [#x10000-#x10FFFF]
        (0x1 <= c && c <= 0xd7ff) || (0xe000 <= c && c <= 0xfffd) || (0x10000 <= c && c <= 0x10ffff)
      else
        // #x9 | #xA | #xD | [#x20-#xD7FF] | [#xE000-#xFFFD] | [#x10000-#x10FFFF]
        c == 0x9 || c == 0xa || c == 0xd || (0x20 <= c && c <= 0xd7ff) || (0xe000 <= c && c <= 0xfffd) || (0x10000 <= c && c <= 0x10ffff)

    def acceptChar(ctx: T.Context,
                   c: Char,
                   error: String,
                   msg: String,
                   chunkAcc: VectorBuilder[XmlEvent]): Pull[F, XmlEvent, (T.Context, VectorBuilder[XmlEvent])] =
      if (T.needsPull(ctx)) {
        emitChunk(Some(chunkAcc)) >> T.pullNext(ctx).flatMap {
          case Some(ctx) =>
            chunkAcc.clear()
            acceptChar(ctx, c, error, msg, chunkAcc)
          case None => fail(error, msg, None)
        }
      } else {
        if (T.current(ctx) == c)
          Pull.pure((T.advance(ctx), chunkAcc))
        else
          fail(error, msg, Some(chunkAcc))
      }

    def accept(ctx: T.Context,
               s: String,
               chunkAcc: VectorBuilder[XmlEvent]): Pull[F, XmlEvent, (T.Context, VectorBuilder[XmlEvent], Int)] = {
      def loop(ctx: T.Context,
               sidx: Int,
               chunkAcc: VectorBuilder[XmlEvent]): Pull[F, XmlEvent, (T.Context, VectorBuilder[XmlEvent], Int)] =
        if (sidx >= s.length) {
          Pull.pure((ctx, chunkAcc, s.length))
        } else if (T.needsPull(ctx)) {
          emitChunk(Some(chunkAcc)) >> T.pullNext(ctx).flatMap {
            case Some(ctx) =>
              chunkAcc.clear()
              loop(ctx, sidx, chunkAcc)
            case None =>
              Pull.pure((eos, new VectorBuilder[XmlEvent], sidx))
          }
        } else {
          if (T.current(ctx) == s.charAt(sidx))
            loop(T.advance(ctx), sidx + 1, chunkAcc)
          else
            Pull.pure((ctx, chunkAcc, sidx))
        }
      loop(ctx, 0, chunkAcc)
    }

    def acceptString(ctx: T.Context,
                     s: String,
                     error: String,
                     msg: String,
                     chunkAcc: VectorBuilder[XmlEvent]): Pull[F, XmlEvent, (T.Context, VectorBuilder[XmlEvent])] =
      accept(ctx, s, chunkAcc).flatMap {
        case (ctx, chunkAcc, n) if n == s.length => Pull.pure((ctx, chunkAcc))
        case (_, chunkAcc, _)                    => fail(error, msg, Some(chunkAcc))
      }

    def assert(ctx: T.Context,
               p: Char => Boolean,
               error: String,
               msg: String,
               chunkAcc: VectorBuilder[XmlEvent]): Pull[F, XmlEvent, (T.Context, VectorBuilder[XmlEvent], Char)] =
      peekChar(ctx, chunkAcc).flatMap {
        case Some((ctx, chunkAcc, c)) if p(c) => Pull.pure((T.advance(ctx), chunkAcc, c))
        case Some((_, chunkAcc, _))           => fail(error, msg, Some(chunkAcc))
        case None                             => fail(error, msg, None)
      }

    def untilChar(ctx: T.Context,
                  p: Char => Boolean,
                  sb: StringBuilder,
                  chunkAcc: VectorBuilder[XmlEvent]): Pull[F, XmlEvent, (T.Context, VectorBuilder[XmlEvent])] =
      if (T.needsPull(ctx)) {
        emitChunk(Some(chunkAcc)) >> T.pullNext(ctx).flatMap {
          case Some(ctx) =>
            chunkAcc.clear()
            untilChar(ctx, p, sb, chunkAcc)
          case None =>
            Pull.pure((eos, new VectorBuilder[XmlEvent]))
        }
      } else {
        val c = T.current(ctx)
        if (!p(c))
          untilChar(T.advance(ctx), p, sb.append(c), chunkAcc)
        else
          Pull.pure((ctx, chunkAcc))
      }

    // ==== low-level internals ====

    def readNCName(ctx: T.Context,
                   chunkAcc: VectorBuilder[XmlEvent]): Pull[F, XmlEvent, (T.Context, VectorBuilder[XmlEvent], String)] =
      if (T.needsPull(ctx)) {
        emitChunk(Some(chunkAcc)) >> T.pullNext(ctx).flatMap {
          case Some(ctx) =>
            chunkAcc.clear()
            readNCName(ctx, chunkAcc)
          case None => fail("1", "unexpected end of input", None)
        }
      } else {
        val c = T.current(ctx)
        if (isNCNameStart(c)) {
          val sb = new StringBuilder
          untilChar(T.advance(ctx), c => !isNCNameChar(c), sb.append(c), chunkAcc).map { case (ctx, chunkAcc) =>
            (ctx, chunkAcc, sb.result())
          }
        } else {
          fail("5", s"character '$c' cannot start a NCName", Some(chunkAcc))
        }
      }

    def readQName(ctx: T.Context,
                  chunkAcc: VectorBuilder[XmlEvent]): Pull[F, XmlEvent, (T.Context, VectorBuilder[XmlEvent], QName)] =
      readNCName(ctx, chunkAcc).flatMap { case (ctx, chunkAcc, part1) =>
        def readPart2(
            ctx: T.Context,
            chunkAcc: VectorBuilder[XmlEvent]): Pull[F, XmlEvent, (T.Context, VectorBuilder[XmlEvent], QName)] =
          if (T.needsPull(ctx)) {
            emitChunk(Some(chunkAcc)) >> T.pullNext(ctx).flatMap {
              case Some(ctx) =>
                chunkAcc.clear()
                readPart2(ctx, chunkAcc)
              case None =>
                Pull.pure((eos, new VectorBuilder[XmlEvent], QName(None, part1)))
            }
          } else {
            T.current(ctx) match {
              case ':' =>
                readNCName(T.advance(ctx), chunkAcc).map { case (ctx, chunkAcc, part2) =>
                  (ctx, chunkAcc, QName(Some(part1), part2))
                }
              case _ =>
                Pull.pure((ctx, chunkAcc, QName(None, part1)))
            }
          }
        readPart2(ctx, chunkAcc)
      }

    def space(ctx: T.Context,
              chunkAcc: VectorBuilder[XmlEvent]): Pull[F, XmlEvent, (T.Context, VectorBuilder[XmlEvent])] =
      if (T.needsPull(ctx)) {
        emitChunk(Some(chunkAcc)) >> T.pullNext(ctx).flatMap {
          case Some(ctx) =>
            chunkAcc.clear()
            space(ctx, chunkAcc)
          case None =>
            Pull.pure((eos, new VectorBuilder[XmlEvent]))
        }
      } else {
        if (isXmlWhitespace(T.current(ctx)))
          space(T.advance(ctx), chunkAcc)
        else
          Pull.pure((ctx, chunkAcc))
      }

    def readMarkupToken(
        ctx: T.Context,
        chunkAcc: VectorBuilder[XmlEvent]): Pull[F, XmlEvent, (T.Context, VectorBuilder[XmlEvent], MarkupToken)] =
      acceptChar(ctx, '<', "43", "expected token start", chunkAcc).flatMap { case (ctx, chunkAcc) =>
        def read(
            ctx: T.Context,
            chunkAcc: VectorBuilder[XmlEvent]): Pull[F, XmlEvent, (T.Context, VectorBuilder[XmlEvent], MarkupToken)] =
          if (T.needsPull(ctx)) {
            emitChunk(Some(chunkAcc)) >> T.pullNext(ctx).flatMap {
              case Some(ctx) =>
                chunkAcc.clear()
                read(ctx, chunkAcc)
              case None => fail("1", "unexpected end of input", None)
            }
          } else {
            T.current(ctx) match {
              case '/' =>
                readQName(T.advance(ctx), chunkAcc).flatMap { case (ctx, chunkAcc, qname) =>
                  space(ctx, chunkAcc).flatMap { case (ctx, chunkAcc) =>
                    acceptChar(ctx, '>', "42", "missing '>' at the end of closing tag", chunkAcc)
                      .map { case (ctx, chunkAcc) => (ctx, chunkAcc, MarkupToken.EndToken(qname)) }
                  }
                }
              case '?' =>
                readNCName(T.advance(ctx), chunkAcc).map { case (ctx, chunkAcc, name) =>
                  (ctx, chunkAcc, MarkupToken.PIToken(name))
                }
              case '!' =>
                peekChar(T.advance(ctx), chunkAcc).flatMap {
                  case Some((ctx, chunkAcc, '-')) =>
                    readComment(T.advance(ctx), chunkAcc)
                  case Some((ctx, chunkAcc, '[')) =>
                    readCDATA(T.advance(ctx), chunkAcc)
                  case Some((ctx, chunkAcc, _)) =>
                    readNCName(ctx, chunkAcc).map { case (ctx, chunkAcc, name) =>
                      (ctx, chunkAcc, MarkupToken.DeclToken(name))
                    }
                  case None =>
                    fail("1", "unexpected end of input", None)
                }
              case _ =>
                readQName(ctx, chunkAcc).map { case (ctx, chunkAcc, name) =>
                  (ctx, chunkAcc, MarkupToken.StartToken(name))
                }
            }
          }
        read(ctx, chunkAcc)
      }

    /* We have read '<!-' so far */
    def readComment(
        ctx: T.Context,
        chunkAcc: VectorBuilder[XmlEvent]): Pull[F, XmlEvent, (T.Context, VectorBuilder[XmlEvent], MarkupToken)] =
      acceptChar(ctx, '-', "15", "second dash missing to open comment", chunkAcc).flatMap { case (ctx, chunkAcc) =>
        def loop(ctx: T.Context,
                 builder: StringBuilder,
                 chunkAcc: VectorBuilder[XmlEvent]): Pull[F, XmlEvent, (T.Context, VectorBuilder[XmlEvent])] =
          nextChar(ctx, chunkAcc).flatMap {
            case (ctx, chunkAcc, '-') =>
              nextChar(ctx, chunkAcc).flatMap {
                case (ctx, chunkAcc, '-') =>
                  acceptChar(ctx, '>', "15", "'--' is not inside comments", chunkAcc)
                case (ctx, chunkAcc, c) =>
                  if (includeComments) builder.append('-').append(c)
                  loop(ctx, builder, chunkAcc)
              }
            case (ctx, chunkAcc, c) =>
              if (includeComments) builder.append(c)
              loop(ctx, builder, chunkAcc)
          }
        val builder = new StringBuilder
        loop(ctx, builder, chunkAcc).map { case (ctx, chunkAcc) =>
          (ctx, chunkAcc, MarkupToken.CommentToken(includeComments.guard[Option].as(builder.result())))
        }
      }

    /* We have read '<![' so far */
    def readCDATA(
        ctx: T.Context,
        chunkAcc: VectorBuilder[XmlEvent]): Pull[F, XmlEvent, (T.Context, VectorBuilder[XmlEvent], MarkupToken)] =
      acceptString(ctx, "CDATA[", "19", "'CDATA[' expected", chunkAcc).map { case (ctx, chunkAcc) =>
        (ctx, chunkAcc, MarkupToken.CDataToken)
      }

    /* We have just read the PI target */
    def readPIBody(ctx: T.Context,
                   chunkAcc: VectorBuilder[XmlEvent]): Pull[F, XmlEvent, (T.Context, VectorBuilder[XmlEvent], String)] =
      space(ctx, chunkAcc).flatMap { case (ctx, chunkAcc) =>
        def loop(ctx: T.Context,
                 sb: StringBuilder,
                 chunkAcc: VectorBuilder[XmlEvent]): Pull[F, XmlEvent, (T.Context, VectorBuilder[XmlEvent], String)] =
          untilChar(ctx, c => c == '?', sb, chunkAcc).flatMap { case (ctx, chunkAcc) =>
            acceptChar(ctx, '?', "16", "unexpected end of input", chunkAcc).flatMap { case (ctx, chunkAcc) =>
              peekChar(ctx, chunkAcc).flatMap {
                case Some((ctx, chunkAcc, '>')) =>
                  Pull.pure((T.advance(ctx), chunkAcc, sb.result()))
                case Some((ctx, chunkAcc, _)) =>
                  loop(ctx, sb.append('?'), chunkAcc)
                case None =>
                  fail("16", "unexpected end of input", None)
              }
            }
          }
        loop(ctx, new StringBuilder, chunkAcc)
      }

    /* We read the beginning of internal DTD subset, read until final ']>' */
    def skipInternalDTD(ctx: T.Context,
                        chunkAcc: VectorBuilder[XmlEvent]): Pull[F, XmlEvent, (T.Context, VectorBuilder[XmlEvent])] =
      nextChar(ctx, chunkAcc).flatMap {
        case (ctx, chunkAcc, ']') =>
          nextChar(ctx, chunkAcc).flatMap {
            case (ctx, chunkAcc, '>') => Pull.pure((ctx, chunkAcc))
            case (ctx, chunkAcc, _)   => skipInternalDTD(ctx, chunkAcc)
          }
        case (ctx, chunkAcc, _) => skipInternalDTD(ctx, chunkAcc)
      }

    def readExternalID(
        ctx: T.Context,
        chunkAcc: VectorBuilder[XmlEvent]): Pull[F, XmlEvent, (T.Context, VectorBuilder[XmlEvent], String)] =
      readNCName(ctx, chunkAcc).flatMap { case (ctx, chunkAcc, sysOrPub) =>
        assert(ctx, isXmlWhitespace(_), "75", "space required after SYSTEM or PUBLIC", chunkAcc).flatMap {
          case (ctx, chunkAcc, _) =>
            sysOrPub match {
              case "SYSTEM" =>
                readQuoted(ctx, false, "11", chunkAcc)
              case "PUBLIC" =>
                readQuoted(ctx, true, "12", chunkAcc).flatMap { case (ctx, chunkAcc, _) =>
                  assert(ctx, isXmlWhitespace(_), "12", "space required after PubidLiteral", chunkAcc).flatMap {
                    case (ctx, chunkAcc, _) => readQuoted(ctx, false, "12", chunkAcc)
                  }
                }
              case _ =>
                fail("75", "SYSTEM or PUBLIC expected", Some(chunkAcc))
            }
        }
      }

    def readQuoted(ctx: T.Context,
                   pub: Boolean,
                   error: String,
                   chunkAcc: VectorBuilder[XmlEvent]): Pull[F, XmlEvent, (T.Context, VectorBuilder[XmlEvent], String)] =
      space(ctx, chunkAcc).flatMap { case (ctx, chunkAcc) =>
        assert(ctx, c => c == '"' || c == '\'', error, "single or double quote expected", chunkAcc)
          .flatMap { case (ctx, chunkAcc, delimiter) =>
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
            untilChar(ctx, pred, sb, chunkAcc).flatMap { case (ctx, chunkAcc) =>
              Pull.pure((T.advance(ctx), chunkAcc, sb.result()))
            }
          }
      }

    def scanMisc(ctx: T.Context, chunkAcc: VectorBuilder[XmlEvent])
        : Pull[F, XmlEvent, Option[(T.Context, VectorBuilder[XmlEvent], MarkupToken)]] =
      space(ctx, chunkAcc).flatMap { case (ctx, chunkAcc) =>
        peekChar(ctx, chunkAcc).flatMap {
          case Some((ctx, chunkAcc, '<')) =>
            readMarkupToken(ctx, chunkAcc).flatMap {
              case (ctx, chunkAcc, MarkupToken.CommentToken(None)) => scanMisc(ctx, chunkAcc)
              case (ctx, chunkAcc, MarkupToken.CommentToken(Some(comment))) =>
                scanMisc(ctx, chunkAcc += XmlEvent.Comment(comment))
              case res @ (_, _, MarkupToken.PIToken(_))    => Pull.pure(Some(res))
              case res @ (_, _, MarkupToken.DeclToken(_))  => Pull.pure(Some(res))
              case res @ (_, _, MarkupToken.StartToken(_)) => Pull.pure(Some(res))
              case (_, chunkAcc, t)                        => fail("22", s"unexpected token '$t'", Some(chunkAcc))
            }
          case Some((_, chunkAcc, c)) =>
            fail("22", s"unexpected character '$c'", Some(chunkAcc))
          case None => Pull.pure(None)
        }
      }

    /* We read '&#' so far */
    def readCharRef(ctx: T.Context,
                    is11: Boolean,
                    chunkAcc: VectorBuilder[XmlEvent]): Pull[F, XmlEvent, (T.Context, VectorBuilder[XmlEvent], Int)] = {
      def postlude(ctx: T.Context, n: Int, chunkAcc: VectorBuilder[XmlEvent]) =
        nextChar(ctx, chunkAcc).flatMap {
          case (ctx, chunkAcc, ';') =>
            if (isValid(is11, n))
              Pull.pure((ctx, chunkAcc, n))
            else
              fail("2", "invalid character", Some(chunkAcc))
          case (_, chunkAcc, _) =>
            fail("66", "character reference must end with a semicolon", Some(chunkAcc))
        }
      peekChar(ctx, chunkAcc).flatMap {
        case Some((ctx, chunkAcc, 'x')) =>
          readNum(T.advance(ctx), 16, chunkAcc).flatMap { case (ctx, chunkAcc, n) =>
            postlude(ctx, n, chunkAcc)
          }
        case Some((ctx, chunkAcc, _)) =>
          readNum(ctx, 10, chunkAcc).flatMap { case (ctx, chunkAcc, n) =>
            postlude(ctx, n, chunkAcc)
          }
        case None => fail("66", "unexpected end of input", None)
      }
    }

    def readNum(ctx: T.Context,
                base: Int,
                chunkAcc: VectorBuilder[XmlEvent]): Pull[F, XmlEvent, (T.Context, VectorBuilder[XmlEvent], Int)] = {
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

      def restNum(ctx: T.Context,
                  acc: Int,
                  chunkAcc: VectorBuilder[XmlEvent]): Pull[F, XmlEvent, (T.Context, VectorBuilder[XmlEvent], Int)] =
        peekChar(ctx, chunkAcc).flatMap {
          case Some((ctx, chunkAcc, Digit(d))) =>
            restNum(T.advance(ctx), acc * base + d, chunkAcc)
          case Some((ctx, chunkAcc, _)) =>
            Pull.pure((ctx, chunkAcc, acc))
          case None =>
            Pull.pure((eos, new VectorBuilder[XmlEvent], acc))
        }

      nextChar(ctx, chunkAcc).flatMap {
        case (ctx, chunkAcc, Digit(d)) => restNum(ctx, d, chunkAcc)
        case (_, chunkAcc, _)          => fail("66", "bad first character reference digit", Some(chunkAcc))
      }
    }

    // ==== middle-level internals ====

    def readAttributes(
        ctx: T.Context,
        is11: Boolean,
        chunkAcc: VectorBuilder[XmlEvent]): Pull[F, XmlEvent, (T.Context, VectorBuilder[XmlEvent], List[Attr])] = {
      def loop(ctx: T.Context,
               attributes: VectorBuilder[Attr],
               chunkAcc: VectorBuilder[XmlEvent]): Pull[F, XmlEvent, (T.Context, VectorBuilder[XmlEvent], List[Attr])] =
        space(ctx, chunkAcc).flatMap { case (ctx, chunkAcc) =>
          peekChar(ctx, chunkAcc).flatMap {
            case Some((ctx, chunkAcc, c)) if isNCNameStart(c) =>
              readQName(ctx, chunkAcc).flatMap { case (ctx, chunkAcc, name) =>
                space(ctx, chunkAcc).flatMap { case (ctx, chunkAcc) =>
                  acceptChar(ctx, '=', "25", "'=' character expected", chunkAcc).flatMap { case (ctx, chunkAcc) =>
                    space(ctx, chunkAcc).flatMap { case (ctx, chunkAcc) =>
                      assert(ctx,
                             c => c == '"' || c == '\'',
                             "10",
                             "single or double quote expected around attribute value",
                             chunkAcc)
                        .flatMap { case (ctx, chunkAcc, delimiter) =>
                          readAttributeValue(ctx, is11, Some(delimiter), new StringBuilder, new VectorBuilder, chunkAcc)
                            .flatMap { case (ctx, chunkAcc, value) =>
                              loop(ctx, attributes += Attr(name, value), chunkAcc)
                            }
                        }
                    }
                  }
                }
              }
            case Some((ctx, chunkAcc, _)) => Pull.pure((ctx, chunkAcc, attributes.result().toList))
            case None                     => fail("1", "unexpected end of input", None)
          }
        }
      loop(ctx, new VectorBuilder, chunkAcc)
    }

    def readAttributeValue(ctx: T.Context,
                           is11: Boolean,
                           delim: Option[Char],
                           current: StringBuilder,
                           builder: VectorBuilder[XmlEvent.XmlTexty],
                           chunkAcc: VectorBuilder[XmlEvent])
        : Pull[F, XmlEvent, (T.Context, VectorBuilder[XmlEvent], List[XmlEvent.XmlTexty])] = {
      val delimiters = delim.fold(valueDelimiters)(valueDelimiters + _)
      untilChar(ctx, delimiters.contains(_), current, chunkAcc).flatMap { case (ctx, chunkAcc) =>
        nextChar(ctx, chunkAcc).flatMap {
          case (ctx, chunkAcc, c) if Some(c) == delim =>
            if (!current.isEmpty)
              builder += XmlEvent.XmlString(current.toString, false)
            Pull.pure((ctx, chunkAcc, builder.result().toList))
          case (ctx, chunkAcc, '\r') =>
            nextChar(ctx, chunkAcc).flatMap {
              case (ctx, chunkAcc, '\n') =>
                readAttributeValue(ctx, is11, delim, current.append('\n'), builder, chunkAcc)
              case (ctx, chunkAcc, _) =>
                readAttributeValue(ctx, is11, delim, current.append(' '), builder, chunkAcc)
            }
          case (ctx, chunkAcc, c) if isXmlWhitespace(c) =>
            readAttributeValue(ctx, is11, delim, current.append(' '), builder, chunkAcc)
          case (ctx, chunkAcc, '&') =>
            builder += XmlEvent.XmlString(current.toString, false)
            peekChar(ctx, chunkAcc).flatMap {
              case Some((ctx, chunkAcc, '#')) =>
                readCharRef(T.advance(ctx), is11, chunkAcc).flatMap { case (ctx, chunkAcc, n) =>
                  builder += XmlEvent.XmlCharRef(n)
                  readAttributeValue(ctx, is11, delim, new StringBuilder, builder, chunkAcc)
                }
              case Some((ctx, chunkAcc, _)) =>
                readNamedEntity(ctx, chunkAcc).flatMap { case (ctx, chunkAcc, s) =>
                  builder += XmlEvent.XmlEntityRef(s)
                  readAttributeValue(ctx, is11, delim, new StringBuilder, builder, chunkAcc)
                }
              case None =>
                fail("1", "unexpected end of input", None)
            }
          case (_, chunkAcc, c) =>
            fail("10", s"unexpected character '$c'", Some(chunkAcc))
        }
      }
    }

    def readNamedEntity(
        ctx: T.Context,
        chunkAcc: VectorBuilder[XmlEvent]): Pull[F, XmlEvent, (T.Context, VectorBuilder[XmlEvent], String)] =
      readNCName(ctx, chunkAcc).flatMap { case (ctx, chunkAcc, name) =>
        acceptChar(ctx, ';', "68", "named entity must end with a semicolon", chunkAcc).map { case (ctx, chunkAcc) =>
          (ctx, chunkAcc, name)
        }
      }

    def completeStartTag(
        ctx: T.Context,
        is11: Boolean,
        name: QName,
        chunkAcc: VectorBuilder[XmlEvent]): Pull[F, XmlEvent, (T.Context, VectorBuilder[XmlEvent], XmlEvent.StartTag)] =
      readAttributes(ctx, is11, chunkAcc).flatMap { case (ctx, chunkAcc, attributes) =>
        space(ctx, chunkAcc).flatMap { case (ctx, chunkAcc) =>
          peekChar(ctx, chunkAcc)
            .flatMap {
              case Some((ctx, chunkAcc, '/')) => Pull.pure((T.advance(ctx), chunkAcc, true))
              case Some((ctx, chunkAcc, _))   => Pull.pure((ctx, chunkAcc, false))
              case None                       => fail("44", "unexpected end of input", None)
            }
            .flatMap { case (ctx, chunkAcc, isEmpty) =>
              acceptChar(ctx, '>', "44", "missing closing '>'", chunkAcc).map { case (ctx, chunkAcc) =>
                (ctx, chunkAcc, XmlEvent.StartTag(name, attributes, isEmpty))
              }
            }
        }
      }

    /* We read '<[CDATA[' so far */
    def readCDATABody(
        ctx: T.Context,
        sb: StringBuilder,
        chunkAcc: VectorBuilder[XmlEvent]): Pull[F, XmlEvent, (T.Context, VectorBuilder[XmlEvent], String)] =
      untilChar(ctx, c => c == '\n' || c == '\r' || c == ']' || c == '&', sb, chunkAcc).flatMap {
        case (ctx, chunkAcc) =>
          nextChar(ctx, chunkAcc).flatMap {
            case (ctx, chunkAcc, '\n') =>
              readCDATABody(ctx, sb.append('\n'), chunkAcc)
            case (ctx, chunkAcc, ']') =>
              peekChar(ctx, chunkAcc).flatMap {
                case Some((ctx, chunkAcc, ']')) =>
                  checkCDATAEnd(T.advance(ctx), sb, chunkAcc).flatMap {
                    case (ctx, chunkAcc, true)  => Pull.pure((ctx, chunkAcc, sb.result()))
                    case (ctx, chunkAcc, false) => readCDATABody(ctx, sb, chunkAcc)
                  }
                case Some((ctx, chunkAcc, _)) =>
                  readCDATABody(ctx, sb.append(']'), chunkAcc)
                case None =>
                  fail("1", "unexpected end of input", None)
              }
            case (ctx, chunkAcc, '&') =>
              accept(ctx, "gt;", chunkAcc).flatMap { case (ctx, chunkAcc, n) =>
                if (n == 3) {
                  sb.append('>')
                } else {
                  sb.append('&')
                  sb.append("gt;".substring(0, n))
                }
                readCDATABody(ctx, sb, chunkAcc)
              }
            case (ctx, chunkAcc, _) =>
              // must be '\r'
              peekChar(ctx, chunkAcc).flatMap {
                case Some((ctx, chunkAcc, c)) =>
                  if (c == '\n')
                    readCDATABody(T.advance(ctx), sb.append('\n'), chunkAcc)
                  else
                    readCDATABody(ctx, sb.append(' '), chunkAcc)
                case None =>
                  fail("1", "unexpected end of input", None)
              }
          }
      }

    def checkCDATAEnd(
        ctx: T.Context,
        sb: StringBuilder,
        chunkAcc: VectorBuilder[XmlEvent]): Pull[F, XmlEvent, (T.Context, VectorBuilder[XmlEvent], Boolean)] =
      peekChar(ctx, chunkAcc).flatMap {
        case Some((ctx, chunkAcc, '>')) =>
          // done
          Pull.pure((T.advance(ctx), chunkAcc, true))
        case Some((ctx, chunkAcc, ']')) =>
          checkCDATAEnd(T.advance(ctx), sb.append(']'), chunkAcc)
        case Some((ctx, chunkAcc, _)) =>
          sb.append("]]")
          Pull.pure((ctx, chunkAcc, false))
        case None =>
          fail("1", "unexpected end of input", None)
      }

    def readCharData(
        ctx: T.Context,
        is11: Boolean,
        chunkAcc: VectorBuilder[XmlEvent]): Pull[F, XmlEvent, (T.Context, VectorBuilder[XmlEvent], XmlEvent)] =
      peekChar(ctx, chunkAcc).flatMap {
        case Some((ctx, chunkAcc, '<')) =>
          readMarkupToken(ctx, chunkAcc).flatMap {
            case (ctx, chunkAcc, MarkupToken.CommentToken(None)) =>
              readCharData(ctx, is11, chunkAcc)
            case (ctx, chunkAcc, MarkupToken.CommentToken(Some(comment))) =>
              readCharData(ctx, is11, chunkAcc += XmlEvent.Comment(comment))
            case (_, chunkAcc, MarkupToken.DeclToken(n)) =>
              fail("14", s"unexpected declaration '$n'", Some(chunkAcc))
            case (ctx, chunkAcc, MarkupToken.CDataToken) =>
              readCDATABody(ctx, new StringBuilder, chunkAcc).map { case (ctx, chunkAcc, body) =>
                (ctx, chunkAcc, XmlEvent.XmlString(body, true))
              }
            case (ctx, chunkAcc, MarkupToken.EndToken(name)) =>
              Pull.pure((ctx, chunkAcc, XmlEvent.EndTag(name)))
            case (ctx, chunkAcc, MarkupToken.StartToken(name)) =>
              completeStartTag(ctx, is11, name, chunkAcc)
            case (ctx, chunkAcc, MarkupToken.PIToken(target)) if !target.equalsIgnoreCase("xml") =>
              readPIBody(ctx, chunkAcc).flatMap { case (ctx, chunkAcc, body) =>
                Pull.pure((ctx, chunkAcc, XmlEvent.XmlPI(target, body)))
              }
            case (_, chunkAcc, t) =>
              fail("43", s"unexpected token ${t.render}", Some(chunkAcc))
          }
        case Some((ctx, chunkAcc, '&')) =>
          peekChar(T.advance(ctx), chunkAcc).flatMap {
            case Some((ctx, chunkAcc, '#')) =>
              readCharRef(T.advance(ctx), is11, chunkAcc).map { case (ctx, chunkAcc, n) =>
                (ctx, chunkAcc, XmlEvent.XmlCharRef(n))
              }
            case Some((ctx, chunkAcc, _)) =>
              readNamedEntity(ctx, chunkAcc).map { case (ctx, chunkAcc, v) =>
                (ctx, chunkAcc, XmlEvent.XmlEntityRef(v))
              }
            case None =>
              fail("1", "unexpected end of input", None)
          }
        case Some((ctx, chunkAcc, _)) =>
          slowPath(ctx, new StringBuilder, chunkAcc)
        case None =>
          fail("1", "unexpected end of input", None)
      }

    def slowPath(ctx: T.Context, sb: StringBuilder, chunkAcc: VectorBuilder[XmlEvent])
        : Pull[F, XmlEvent, (T.Context, VectorBuilder[XmlEvent], XmlEvent.XmlString)] =
      untilChar(ctx, c => c == '<' || c == '&' || c == '\r', sb, chunkAcc).flatMap { case (ctx, chunkAcc) =>
        peekChar(ctx, chunkAcc).flatMap {
          case Some((ctx, chunkAcc, '<')) => Pull.pure((ctx, chunkAcc, XmlEvent.XmlString(sb.toString, false)))
          case None                       => Pull.pure((ctx, chunkAcc, XmlEvent.XmlString(sb.toString, false)))
          case Some((ctx, chunkAcc, '&')) => Pull.pure((ctx, chunkAcc, XmlEvent.XmlString(sb.toString, false)))
          case Some((ctx, chunkAcc, _)) =>
            peekChar(T.advance(ctx), chunkAcc).flatMap {
              case Some((ctx, chunkAcc, '\n')) =>
                sb.append('\n')
                slowPath(T.advance(ctx), sb, chunkAcc)
              case Some((ctx, chunkAcc, _)) =>
                sb.append('\n')
                slowPath(ctx, sb, chunkAcc)
              case None =>
                fail("14", "unexpected end of input", None)
            }
        }

      }

    // ==== high-level internals

    def scanPrologToken0(
        ctx: T.Context,
        chunkAcc: VectorBuilder[XmlEvent]): Pull[F, XmlEvent, Option[(T.Context, VectorBuilder[XmlEvent])]] =
      peekChar(ctx, chunkAcc).flatMap {
        case Some((ctx, chunkAcc, '<')) =>
          readMarkupToken(ctx, chunkAcc)
            .map { case (ctx, chunkAcc, t) => (ctx, chunkAcc += XmlEvent.StartDocument, t) }
            .flatMap {
              case (ctx, chunkAcc, MarkupToken.PIToken(name)) if name.equalsIgnoreCase("xml") =>
                handleXmlDecl(ctx, chunkAcc).flatMap { case (ctx, chunkAcc, (is11, decl)) =>
                  scanPrologToken1(ctx, is11, chunkAcc += decl)
                }
              case (ctx, chunkAcc, MarkupToken.PIToken(name)) =>
                readPIBody(ctx, chunkAcc).flatMap { case (ctx, chunkAcc, body) =>
                  scanPrologToken1(ctx, false, chunkAcc += XmlEvent.XmlPI(name, body))
                }
              case (ctx, chunkAcc, MarkupToken.DeclToken(name)) =>
                handleDecl(ctx, name, chunkAcc).flatMap { case (ctx, chunkAcc) =>
                  scanPrologToken2(ctx, false, chunkAcc)
                }
              case (ctx, chunkAcc, MarkupToken.StartToken(name)) =>
                readElement(ctx, false, name, chunkAcc)
              case (ctx, chunkAcc, MarkupToken.CommentToken(None)) =>
                scanPrologToken1(ctx, false, chunkAcc)
              case (ctx, chunkAcc, MarkupToken.CommentToken(Some(comment))) =>
                scanPrologToken1(ctx, false, chunkAcc += XmlEvent.Comment(comment))
              case (_, chunkAcc, t) =>
                fail("22", s"unexpected markup $t", Some(chunkAcc))
            }
        case Some((ctx, chunkAcc, _)) =>
          scanPrologToken1(ctx, false, chunkAcc += XmlEvent.StartDocument)
        case None =>
          Pull.pure(None)
      }

    def scanPrologToken1(
        ctx: T.Context,
        is11: Boolean,
        chunkAcc: VectorBuilder[XmlEvent]): Pull[F, XmlEvent, Option[(T.Context, VectorBuilder[XmlEvent])]] =
      scanMisc(ctx, chunkAcc).flatMap {
        case Some((ctx, chunkAcc, MarkupToken.PIToken(name))) if !name.equalsIgnoreCase("xml") =>
          readPIBody(ctx, chunkAcc).flatMap { case (ctx, chunkAcc, body) =>
            scanPrologToken1(ctx, is11, chunkAcc += XmlEvent.XmlPI(name, body))
          }
        case Some((ctx, chunkAcc, MarkupToken.DeclToken(name))) =>
          handleDecl(ctx, name, chunkAcc).flatMap { case (ctx, chunkAcc) =>
            scanPrologToken2(ctx, is11, chunkAcc)
          }
        case Some((ctx, chunkAcc, MarkupToken.StartToken(name))) =>
          readElement(ctx, is11, name, chunkAcc)
        case Some((_, chunkAcc, t)) =>
          fail("22", s"unexpected markup $t", Some(chunkAcc))
        case None =>
          Pull.output1(XmlEvent.EndDocument).as(None)
      }

    def handleVersion(
        ctx: T.Context,
        chunkAcc: VectorBuilder[XmlEvent],
        version: String,
        delimiter: Char): Pull[F, XmlEvent, (T.Context, VectorBuilder[XmlEvent], (Boolean, XmlEvent.XmlDecl))] =
      if (version.length == 2) {
        fail("26", "expected non empty minor version", Some(chunkAcc))
      } else {
        acceptChar(ctx, delimiter, "24", "expected delimiter to close version attribute value", chunkAcc)
          .flatMap { case (ctx, chunkAcc) =>
            readEncoding(ctx, false, chunkAcc).flatMap { case (ctx, chunkAcc, (hasSpace, encoding)) =>
              readStandalone(ctx, hasSpace, chunkAcc).flatMap { case (ctx, chunkAcc, standalone) =>
                space(ctx, chunkAcc).flatMap { case (ctx, chunkAcc) =>
                  acceptString(ctx, "?>", "23", "expected end of PI", chunkAcc)
                    .map { case (ctx, chunkAcc) =>
                      (ctx, chunkAcc, (version == "1.1", XmlEvent.XmlDecl(version, encoding, standalone)))
                    }
                }
              }
            }
          }
      }

    def handleXmlDecl(ctx: T.Context, chunkAcc: VectorBuilder[XmlEvent])
        : Pull[F, XmlEvent, (T.Context, VectorBuilder[XmlEvent], (Boolean, XmlEvent.XmlDecl))] =
      assert(ctx, isXmlWhitespace(_), "24", "space is expected after xml", chunkAcc).flatMap {
        case (ctx, chunkAcc, _) =>
          space(ctx, chunkAcc).flatMap { case (ctx, chunkAcc) =>
            acceptString(ctx, "version", "24", "expected 'version' attribute", chunkAcc)
              .flatMap { case (ctx, chunkAcc) =>
                space(ctx, chunkAcc).flatMap { case (ctx, chunkAcc) =>
                  acceptChar(ctx, '=', "24", "expected '=' after version", chunkAcc).flatMap { case (ctx, chunkAcc) =>
                    space(ctx, chunkAcc).flatMap { case (ctx, chunkAcc) =>
                      assert(ctx, c => c == '"' || c == '\'', "24", "simple or double quote expected", chunkAcc)
                        .flatMap { case (ctx, chunkAcc, delimiter) =>
                          acceptChar(ctx, '1', "26", "expected major version 1", chunkAcc)
                            .flatMap { case (ctx, chunkAcc) =>
                              acceptChar(ctx, '.', "26", "expected dot", chunkAcc)
                                .flatMap { case (ctx, chunkAcc) =>
                                  val sb = new StringBuilder("1.")
                                  untilChar(ctx, !_.isDigit, sb, chunkAcc).flatMap { case (ctx, chunkAcc) =>
                                    handleVersion(ctx, chunkAcc, sb.result(), delimiter)
                                  }
                                }
                            }
                        }
                    }
                  }
                }
              }
          }
      }

    def readEncoding(ctx: T.Context, hasSpace: Boolean, chunkAcc: VectorBuilder[XmlEvent])
        : Pull[F, XmlEvent, (T.Context, VectorBuilder[XmlEvent], (Boolean, Option[String]))] =
      peekChar(ctx, chunkAcc).flatMap {
        case Some((ctx, chunkAcc, c)) if isXmlWhitespace(c) =>
          space(ctx, chunkAcc).flatMap { case (ctx, chunkAcc) => readEncoding(ctx, true, chunkAcc) }
        case Some((ctx, chunkAcc, 'e')) =>
          if (hasSpace) {
            acceptString(ctx, "encoding", "80", "expected 'encoding' attribute", chunkAcc).flatMap {
              case (ctx, chunkAcc) =>
                space(ctx, chunkAcc).flatMap { case (ctx, chunkAcc) =>
                  acceptChar(ctx, '=', "80", "expected '='", chunkAcc).flatMap { case (ctx, chunkAcc) =>
                    space(ctx, chunkAcc).flatMap { case (ctx, chunkAcc) =>
                      assert(ctx, c => c == '"' || c == '\'', "80", "simple or double quote expected", chunkAcc)
                        .flatMap { case (ctx, chunkAcc, delimiter) =>
                          assert(ctx,
                                 c => (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z'),
                                 "81",
                                 "wrong encoding name character",
                                 chunkAcc)
                            .flatMap { case (ctx, chunkAcc, fst) =>
                              val sb = new StringBuilder().append(fst)
                              untilChar(
                                ctx,
                                c =>
                                  !((c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9') || c == '.' || c == '_' || c == '-'),
                                sb,
                                chunkAcc)
                                .flatMap { case (ctx, chunkAcc) =>
                                  acceptChar(ctx,
                                             delimiter,
                                             "80",
                                             "'encoding' attribute value must end with proper delimiter",
                                             chunkAcc)
                                    .map { case (ctx, chunkAcc) => (ctx, chunkAcc, (false, Some(sb.result()))) }
                                }
                            }
                        }
                    }
                  }
                }
            }
          } else {
            fail("80", "expected space before 'encoding' attribute", Some(chunkAcc))
          }
        case Some((ctx, chunkAcc, _)) =>
          Pull.pure((ctx, chunkAcc, (hasSpace, None)))
        case None =>
          Pull.pure((eos, new VectorBuilder[XmlEvent], (hasSpace, None)))
      }

    def readStandalone(
        ctx: T.Context,
        hasSpace: Boolean,
        chunkAcc: VectorBuilder[XmlEvent]): Pull[F, XmlEvent, (T.Context, VectorBuilder[XmlEvent], Option[Boolean])] =
      peekChar(ctx, chunkAcc).flatMap {
        case Some((ctx, chunkAcc, c)) if isXmlWhitespace(c) =>
          space(ctx, chunkAcc).flatMap { case (ctx, chunkAcc) => readStandalone(ctx, true, chunkAcc) }
        case Some((ctx, chunkAcc, 's')) =>
          if (hasSpace) {
            acceptString(ctx, "standalone", "32", "expected 'standalone' attribute", chunkAcc).flatMap {
              case (ctx, chunkAcc) =>
                space(ctx, chunkAcc).flatMap { case (ctx, chunkAcc) =>
                  acceptChar(ctx, '=', "32", "expected '='", chunkAcc).flatMap { case (ctx, chunkAcc) =>
                    space(ctx, chunkAcc).flatMap { case (ctx, chunkAcc) =>
                      assert(ctx, c => c == '"' || c == '\'', "32", "simple or double quote expected", chunkAcc)
                        .flatMap { case (ctx, chunkAcc, delimiter) =>
                          nextChar(ctx, chunkAcc)
                            .flatMap {
                              case (ctx, chunkAcc, 'y') =>
                                acceptString(ctx, "es", "32", "expected 'yes' or 'no'", chunkAcc).map {
                                  case (ctx, chunkAcc) => (ctx, chunkAcc, true)
                                }
                              case (ctx, chunkAcc, 'n') =>
                                acceptChar(ctx, 'o', "32", "expected 'yes' or 'no'", chunkAcc).map {
                                  case (ctx, chunkAcc) => (ctx, chunkAcc, false)
                                }
                              case (_, chunkAcc, _) => fail("32", "expected 'yes' or 'no'", Some(chunkAcc))
                            }
                            .flatMap { case (ctx, chunkAcc, sa) =>
                              acceptChar(ctx,
                                         delimiter,
                                         "32",
                                         "'standalone' attribute value must end with proper delimiter",
                                         chunkAcc)
                                .map { case (ctx, chunkAcc) => (ctx, chunkAcc, Some(sa)) }
                            }
                        }
                    }
                  }
                }
            }
          } else {
            fail("32", "expected space before 'standalone' attribute", Some(chunkAcc))
          }
        case Some((ctx, chunkAcc, _)) => Pull.pure((ctx, chunkAcc, None))
        case None                     => Pull.pure((eos, new VectorBuilder[XmlEvent], None))
      }

    def handleDecl(ctx: T.Context,
                   name: String,
                   chunkAcc: VectorBuilder[XmlEvent]): Pull[F, XmlEvent, (T.Context, VectorBuilder[XmlEvent])] =
      name match {
        case "DOCTYPE" =>
          assert(ctx, isXmlWhitespace(_), "28", "space is expected after DOCTYPE", chunkAcc).flatMap {
            case (ctx, chunkAcc, _) =>
              space(ctx, chunkAcc).flatMap { case (ctx, chunkAcc) =>
                readNCName(ctx, chunkAcc).flatMap { case (ctx, chunkAcc, docname) =>
                  space(ctx, chunkAcc).flatMap { case (ctx, chunkAcc) =>
                    peekChar(ctx, chunkAcc)
                      .flatMap {
                        case Some((ctx, chunkAcc, c)) if isNCNameStart(c) =>
                          readExternalID(ctx, chunkAcc).map { case (ctx, chunkAcc, name) =>
                            (ctx, chunkAcc, Some(name))
                          }
                        case Some((ctx, chunkAcc, _)) => Pull.pure((ctx, chunkAcc, None))
                        case None                     => Pull.pure((eos, new VectorBuilder[XmlEvent], None))
                      }
                      .flatMap { case (ctx, chunkAcc, systemid) =>
                        space(ctx, chunkAcc).flatMap { case (ctx, chunkAcc) =>
                          nextChar(ctx, chunkAcc)
                            .flatMap {
                              case (ctx, chunkAcc, '>') =>
                                // done
                                Pull.pure((ctx, chunkAcc += XmlEvent.XmlDoctype(name, docname, systemid)))
                              case (ctx, chunkAcc, '[') =>
                                skipInternalDTD(ctx, chunkAcc).map { case (ctx, chunkAcc) =>
                                  (ctx, chunkAcc += XmlEvent.XmlDoctype(name, docname, systemid))
                                }
                              case (_, chunkAcc, c) =>
                                fail("28", s"end of doctype or internal DTD expected but got $c", Some(chunkAcc))
                            }
                        }
                      }
                  }
                }
              }
          }
        case _ =>
          fail("22", "expected DOCTYPE declaration", Some(chunkAcc))
      }

    def scanPrologToken2(
        ctx: T.Context,
        is11: Boolean,
        chunkAcc: VectorBuilder[XmlEvent]): Pull[F, XmlEvent, Option[(T.Context, VectorBuilder[XmlEvent])]] =
      scanMisc(ctx, chunkAcc).flatMap {
        case Some((ctx, chunkAcc, MarkupToken.PIToken(name))) =>
          readPIBody(ctx, chunkAcc).flatMap { case (ctx, chunkAcc, body) =>
            scanPrologToken2(ctx, is11, chunkAcc += XmlEvent.XmlPI(name, body))
          }
        case Some((ctx, chunkAcc, MarkupToken.StartToken(name))) =>
          readElement(ctx, is11, name, chunkAcc)
        case Some((_, chunkAcc, t)) =>
          fail("22", s"unexpected markup $t", Some(chunkAcc))
        case None =>
          Pull.output1(XmlEvent.EndDocument).as(None)
      }

    def readElement(
        ctx: T.Context,
        is11: Boolean,
        name: QName,
        chunkAcc: VectorBuilder[XmlEvent]): Pull[F, XmlEvent, Option[(T.Context, VectorBuilder[XmlEvent])]] =
      completeStartTag(ctx, is11, name, chunkAcc).flatMap {
        case (ctx, chunkAcc, startTag) if startTag.isEmpty =>
          scanPostlog(ctx, chunkAcc += startTag += XmlEvent.EndTag(name))
        case (ctx, chunkAcc, startTag) =>
          readContent(ctx, is11, name, chunkAcc += startTag).flatMap { case (ctx, chunkAcc) =>
            scanPostlog(ctx, chunkAcc)
          }
      }

    def scanPostlog(
        ctx: T.Context,
        chunkAcc: VectorBuilder[XmlEvent]): Pull[F, XmlEvent, Option[(T.Context, VectorBuilder[XmlEvent])]] =
      space(ctx, chunkAcc).flatMap { case (ctx, chunkAcc) =>
        peekChar(ctx, chunkAcc).flatMap {
          case Some((ctx, chunkAcc, '<')) =>
            readMarkupToken(ctx, chunkAcc)
              .flatMap {
                case (ctx, chunkAcc, MarkupToken.PIToken(name)) if name.equalsIgnoreCase("xml") =>
                  handleXmlDecl(ctx, chunkAcc).flatMap { case (ctx, chunkAcc, (is11, decl)) =>
                    scanPrologToken1(ctx, is11, chunkAcc += XmlEvent.EndDocument += XmlEvent.StartDocument += decl)
                  }
                case (ctx, chunkAcc, MarkupToken.PIToken(name)) =>
                  readPIBody(ctx, chunkAcc).flatMap { case (ctx, chunkAcc, body) =>
                    scanPostlog(ctx, chunkAcc += XmlEvent.XmlPI(name, body))
                  }
                case (ctx, chunkAcc, MarkupToken.DeclToken(name)) =>
                  handleDecl(ctx, name, chunkAcc).flatMap { case (ctx, chunkAcc) =>
                    scanPrologToken2(ctx, false, chunkAcc += XmlEvent.EndDocument += XmlEvent.StartDocument)
                  }
                case (ctx, chunkAcc, MarkupToken.StartToken(name)) =>
                  readElement(ctx, false, name, chunkAcc += XmlEvent.EndDocument += XmlEvent.StartDocument)
                case (ctx, chunkAcc, MarkupToken.CommentToken(None)) =>
                  scanPostlog(ctx, chunkAcc)
                case (ctx, chunkAcc, MarkupToken.CommentToken(Some(comment))) =>
                  scanPostlog(ctx, chunkAcc += XmlEvent.Comment(comment))
                case (_, chunkAcc, t) =>
                  fail("22", s"unexpected markup $t", Some(chunkAcc))
              }
          case Some((ctx, chunkAcc, _)) =>
            scanPrologToken1(ctx, false, chunkAcc += XmlEvent.EndDocument += XmlEvent.StartDocument)
          case None =>
            Pull.output1(XmlEvent.EndDocument).as(None)
        }
      }

    def readContent(ctx: T.Context,
                    is11: Boolean,
                    name: QName,
                    chunkAcc: VectorBuilder[XmlEvent]): Pull[F, XmlEvent, (T.Context, VectorBuilder[XmlEvent])] =
      readCharData(ctx, is11, chunkAcc).flatMap { case (ctx, chunkAcc, last) =>
        last match {
          case XmlEvent.EndTag(n) if n == name =>
            // we are done reading that content
            Pull.pure((ctx, chunkAcc += last))
          case XmlEvent.EndTag(n) =>
            fail("GIMatch", s"unexpected closing tag '</${n.render}>' (expected '</${name.render}>')", Some(chunkAcc))
          case XmlEvent.StartTag(name1, _, false) =>
            // parse child element, and continue
            readContent(ctx, is11, name1, chunkAcc += last).flatMap { case (ctx, chunkAcc) =>
              readContent(ctx, is11, name, chunkAcc)
            }
          case XmlEvent.StartTag(name1, _, true) =>
            // parse child element, and continue
            readContent(ctx, is11, name, chunkAcc += last += XmlEvent.EndTag(name1))
          case _ =>
            // just emit and continue
            readContent(ctx, is11, name, chunkAcc += last)
        }
      }

    def go(ctx: T.Context, chunkAcc: VectorBuilder[XmlEvent]): Pull[F, XmlEvent, Unit] =
      scanPrologToken0(ctx, chunkAcc).flatMap {
        case Some((ctx, chunkAcc)) => go(ctx, chunkAcc += XmlEvent.EndDocument)
        case None                  => Pull.done
      }
    s => Stream.suspend(Stream.emit(T.create(s))).flatMap(go(_, new VectorBuilder).stream)
  }

}
