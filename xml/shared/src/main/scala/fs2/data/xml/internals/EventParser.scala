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

import text._

import cats.data._
import cats.syntax.all._

import scala.collection.immutable.VectorBuilder

private[xml] object EventParser {

  // ==== utils ====

  val valueDelimiters = " \t\r\n<&"

  def pipe[F[_], T](implicit F: RaiseThrowable[F], T: CharLikeChunks[F, T]): Pipe[F, T, XmlEvent] = {

    type XmlPull[V] = Pull[F, XmlEvent, V]
    type State = (T.Context, VectorBuilder[XmlEvent])
    type StatePull[R] = StateT[XmlPull, State, R]

    val eos = T.create(Stream.empty)

    def fail[R](prod: String, msg: String, chunkAcc: Option[VectorBuilder[XmlEvent]]): Pull[F, XmlEvent, R] =
      emitChunk(chunkAcc) >> Pull.raiseError[F](new XmlException(XmlSyntax(prod), msg))

    def failState[R](prod: String, msg: String, chunkAcc: Option[VectorBuilder[XmlEvent]]): StatePull[R] =
      StateT.liftF(emitChunk(chunkAcc) >> Pull.raiseError[F](new XmlException(XmlSyntax(prod), msg)))

    def failAskState[R](prod: String, msg: String): StatePull[R] =
      StateT { case (ctx, chunkAcc) => failState(prod, msg, Some(chunkAcc)).run(ctx -> chunkAcc) }

    def advanceState: StatePull[Unit] = StateT.modify { case (ctx, chunkAcc) => T.advance(ctx) -> chunkAcc }

    def pureState[R](r: R): StatePull[R] = StateT.pure(r)

    def unitState: StatePull[Unit] = StateT.pure(())

    def appendState(event: XmlEvent): StatePull[Unit] = StateT.modify(_.map(_ += event))

    def eosState[R](r: R): StatePull[R] = StateT.set[XmlPull, State](eos -> new VectorBuilder[XmlEvent]).as(r)

    def peekChar: StatePull[Option[Char]] = StateT { case (ctx, chunkAcc) =>
      if (T.needsPull(ctx)) {
        emitChunk(Some(chunkAcc)) >> T.pullNext(ctx).flatMap {
          case Some(ctx) =>
            chunkAcc.clear()
            peekChar.run(ctx -> chunkAcc)
          case None => Pull.pure((ctx, chunkAcc) -> None).covaryAll
        }
      } else {
        Pull.pure((ctx -> chunkAcc, Some(T.current(ctx)))).covaryAll
      }
    }

    def nextChar: StatePull[Char] = StateT { case (ctx, chunkAcc) =>
      if (T.needsPull(ctx)) {
        emitChunk(Some(chunkAcc)) >> T.pullNext(ctx).flatMap {
          case Some(ctx) =>
            chunkAcc.clear()
            nextChar.run(ctx, chunkAcc)
          case None => fail("1", "unexpected end of input", None)
        }
      } else {
        val c = T.current(ctx)
        Pull.pure((T.advance(ctx) -> chunkAcc, c))
      }
    }

    def isValid(is11: Boolean, c: Int): Boolean =
      if (is11)
        // [#x1-#xD7FF] | [#xE000-#xFFFD] | [#x10000-#x10FFFF]
        (0x1 <= c && c <= 0xd7ff) || (0xe000 <= c && c <= 0xfffd) || (0x10000 <= c && c <= 0x10ffff)
      else
        // #x9 | #xA | #xD | [#x20-#xD7FF] | [#xE000-#xFFFD] | [#x10000-#x10FFFF]
        c == 0x9 || c == 0xa || c == 0xd || (0x20 <= c && c <= 0xd7ff) || (0xe000 <= c && c <= 0xfffd) || (0x10000 <= c && c <= 0x10ffff)

    def isNCNameStart(c: Char): Boolean = {
      import java.lang.Character._
      getType(c).toByte match {
        case LOWERCASE_LETTER | UPPERCASE_LETTER | OTHER_LETTER | TITLECASE_LETTER | LETTER_NUMBER => true
        case _                                                                                     => c == '_'
      }
    }

    def isNCNameChar(c: Char): Boolean = {
      import java.lang.Character._
      // The constants represent groups Mc, Me, Mn, Lm, and Nd.
      isNCNameStart(c) || (getType(c).toByte match {
        case COMBINING_SPACING_MARK | ENCLOSING_MARK | NON_SPACING_MARK | MODIFIER_LETTER | DECIMAL_DIGIT_NUMBER => true
        case _                                                                                                   => ".-·".contains(c)
      })
    }

    def isXmlWhitespace(c: Char): Boolean =
      c == ' ' || c == '\t' || c == '\r' || c == '\n'

    def acceptChar(c: Char, error: String, msg: String): StatePull[Unit] = StateT { case (ctx, chunkAcc) =>
      if (T.needsPull(ctx)) {
        emitChunk(Some(chunkAcc)) >> T.pullNext(ctx).flatMap {
          case Some(ctx) =>
            chunkAcc.clear()
            acceptChar(c, error, msg).run(ctx, chunkAcc)
          case None => fail(error, msg, None)
        }
      } else {
        if (T.current(ctx) == c)
          Pull.pure((T.advance(ctx) -> chunkAcc, ()))
        else
          fail(error, msg, Some(chunkAcc))
      }
    }

    def accept(s: String): StatePull[Int] = {
      def loop(sidx: Int): StatePull[Int] = StateT { case (ctx, chunkAcc) =>
        if (sidx >= s.length) {
          Pull.pure((ctx -> chunkAcc, s.length))
        } else if (T.needsPull(ctx)) {
          emitChunk(Some(chunkAcc)) >> T.pullNext(ctx).flatMap {
            case Some(ctx) =>
              chunkAcc.clear()
              accept(s).run(ctx -> chunkAcc)
            case None =>
              Pull.pure((eos -> new VectorBuilder[XmlEvent], sidx))
          }
        } else {
          if (T.current(ctx) == s.charAt(sidx))
            loop(sidx + 1).run(T.advance(ctx), chunkAcc)
          else
            Pull.pure((ctx -> chunkAcc, sidx)).covary
        }
      }
      loop(0)
    }

    def acceptString(s: String, error: String, msg: String): StatePull[Unit] =
      accept(s).flatMap(n =>
        if (n == s.length) unitState
        else failAskState(error, msg))

    def assert(p: Char => Boolean, error: String, msg: String): StatePull[Char] =
      peekChar.flatMap(c =>
        StateT { case (ctx, chunkAcc) =>
          if (c.exists(p)) Pull.pure((T.advance(ctx) -> chunkAcc, c.get))
          else if (c.isDefined) fail(error, msg, Some(chunkAcc))
          else fail(error, msg, None)
        })

    def untilChar(p: Char => Boolean, sb: StringBuilder): StatePull[Unit] = StateT { case (ctx, chunkAcc) =>
      if (T.needsPull(ctx)) {
        emitChunk(Some(chunkAcc)) >> T.pullNext(ctx).flatMap {
          case Some(ctx) =>
            chunkAcc.clear()
            untilChar(p, sb).run(ctx, chunkAcc)
          case None =>
            Pull.pure((eos, new VectorBuilder[XmlEvent]) -> ())
        }
      } else {
        val c = T.current(ctx)
        if (p(c)) Pull.pure((ctx, chunkAcc) -> ())
        else untilChar(p, sb.append(c)).run(T.advance(ctx), chunkAcc)
      }
    }

    // ==== low-level internals ====

    def readNCName: StatePull[String] = StateT { case (ctx, chunkAcc) =>
      if (T.needsPull(ctx)) {
        emitChunk(Some(chunkAcc)) >> T.pullNext(ctx).flatMap {
          case Some(ctx) =>
            chunkAcc.clear()
            readNCName.run(ctx, chunkAcc)
          case None => fail("1", "unexpected end of input", None)
        }
      } else {
        val c = T.current(ctx)
        if (isNCNameStart(c)) {
          val sb = new StringBuilder
          untilChar(c => !isNCNameChar(c), sb.append(c)).map(_ => sb.result()).run(T.advance(ctx), chunkAcc)
        } else {
          fail("5", s"character '$c' cannot start a NCName", Some(chunkAcc))
        }
      }
    }

    def readQName: StatePull[QName] =
      readNCName.flatMap { part1 =>
        def readPart2: StatePull[QName] = StateT { case (ctx, chunkAcc) =>
          if (T.needsPull(ctx)) {
            emitChunk(Some(chunkAcc)) >> T.pullNext(ctx).flatMap {
              case Some(ctx) =>
                chunkAcc.clear()
                readPart2.run(ctx, chunkAcc)
              case None =>
                Pull.pure((eos -> new VectorBuilder[XmlEvent], QName(None, part1)))
            }
          } else {
            T.current(ctx) match {
              case ':' =>
                readNCName.map(part2 => QName(Some(part1), part2)).run(T.advance(ctx), chunkAcc)
              case _ =>
                Pull.pure((ctx -> chunkAcc, QName(None, part1)))
            }
          }
        }
        readPart2
      }

    def space: StatePull[Unit] = StateT { case (ctx, chunkAcc) =>
      if (T.needsPull(ctx)) {
        emitChunk(Some(chunkAcc)) >> T.pullNext(ctx).flatMap {
          case Some(ctx) =>
            chunkAcc.clear()
            space.run(ctx, chunkAcc)
          case None =>
            Pull.pure((eos, new VectorBuilder[XmlEvent]) -> ())
        }
      } else {
        if (isXmlWhitespace(T.current(ctx)))
          space.run(T.advance(ctx), chunkAcc)
        else
          Pull.pure((ctx, chunkAcc) -> ())
      }
    }

    def readMarkupToken: StatePull[MarkupToken] =
      acceptChar('<', "43", "expected token start") >> {
        def read: StatePull[MarkupToken] = StateT { case (ctx, chunkAcc) =>
          if (T.needsPull(ctx)) {
            emitChunk(Some(chunkAcc)) >> T.pullNext(ctx).flatMap {
              case Some(ctx) =>
                chunkAcc.clear()
                read.run(ctx, chunkAcc)
              case None => fail("1", "unexpected end of input", None)
            }
          } else {
            T.current(ctx) match {
              case '/' =>
                (for {
                  qname <- readQName
                  _ <- space
                  _ <- acceptChar('>', "42", "missing '>' at the end of closing tag")
                } yield MarkupToken.EndToken(qname)).run(T.advance(ctx) -> chunkAcc)
              case '?' =>
                readNCName.map(MarkupToken.PIToken).run(T.advance(ctx) -> chunkAcc)
              case '!' =>
                peekChar
                  .flatMap {
                    case Some('-') => advanceState >> skipComment
                    case Some('[') => advanceState >> readCDATA
                    case Some(_)   => readNCName.map[MarkupToken](MarkupToken.DeclToken)
                    case None      => failState[MarkupToken]("1", "unexpected end of input", None)
                  }
                  .run(T.advance(ctx) -> chunkAcc)
              case _ =>
                readQName.map(MarkupToken.StartToken).run(ctx -> chunkAcc)
            }
          }
        }
        read
      }

    /** We have read '<!-' so far */
    def skipComment: StatePull[MarkupToken] =
      acceptChar('-', "15", "second dash missing to open comment") >> {
        def loop: StatePull[Unit] =
          nextChar.flatMap {
            case '-' =>
              nextChar.flatMap {
                case '-' => acceptChar('>', "15", "'--' is not inside comments")
                case _   => loop
              }
            case _ => loop
          }
        loop.as[MarkupToken](MarkupToken.CommentToken)
      }

    /** We have read '<![' so far */
    def readCDATA: StatePull[MarkupToken] = acceptString("CDATA[", "19", "'CDATA[' expected").as(MarkupToken.CDataToken)

    /** We have just read the PI target */
    def readPIBody: StatePull[String] =
      space >> {
        def loop(sb: StringBuilder): StatePull[String] =
          untilChar(_ == '?', sb) >> {
            acceptChar('?', "16", "unexpected end of input") >> {
              peekChar.flatMap {
                case Some('>') =>
                  StateT { case (ctx, chunkAcc) => Pull.pure((T.advance(ctx) -> chunkAcc, sb.result())) }
                case Some(_) =>
                  loop(sb.append('?'))
                case None =>
                  failState[String]("16", "unexpected end of input", None)
              }
            }
          }
        loop(new StringBuilder)
      }

    /** We read the beginning of internal DTD subset, read until final ']>' */
    def skipInternalDTD: StatePull[Unit] =
      nextChar.flatMap {
        case ']' =>
          nextChar.flatMap {
            case '>' => StateT.pure(())
            case _   => skipInternalDTD
          }
        case _ => skipInternalDTD
      }

    def readExternalID: StatePull[String] =
      readNCName.flatMap { sysOrPub =>
        assert(isXmlWhitespace(_), "75", "space required after SYSTEM or PUBLIC").flatMap { case _ =>
          sysOrPub match {
            case "SYSTEM" =>
              readQuoted(false, "11")
            case "PUBLIC" =>
              for {
                _ <- readQuoted(true, "12")
                _ <- assert(isXmlWhitespace(_), "12", "space required after PubidLiteral")
                res <- readQuoted(false, "12")
              } yield res
            case _ =>
              StateT { case (ctx, chunkAcc) => fail("75", "SYSTEM or PUBLIC expected", Some(chunkAcc)) }
          }
        }
      }

    def readQuoted(pub: Boolean, error: String): StatePull[String] =
      space >> {
        assert(c => c == '"' || c == '\'', error, "single or double quote expected").flatMap { delimiter =>
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
          untilChar(pred, sb) >> advanceState.map(_ => sb.result())
        }
      }

    def scanMisc: StatePull[Option[MarkupToken]] =
      space >> {
        peekChar.flatMap {
          case Some('<') =>
            readMarkupToken.flatMap {
              case MarkupToken.CommentToken        => scanMisc
              case res @ MarkupToken.PIToken(_)    => pureState(res.some)
              case res @ MarkupToken.DeclToken(_)  => pureState(res.some)
              case res @ MarkupToken.StartToken(_) => pureState(res.some)
              case t                               => failAskState("22", s"unexpected token '$t'")
            }
          case Some(c) =>
            failAskState("22", s"unexpected character '$c'")
          case None => pureState(None)
        }
      }

    /** We read '&#' so far */
    def readCharRef(is11: Boolean): StatePull[Int] = {
      def postlude(n: Int): StatePull[Int] =
        nextChar.flatMap {
          case ';' =>
            if (isValid(is11, n)) pureState(n)
            else failAskState[Int]("2", "invalid character")
          case _ =>
            failAskState[Int]("66", "character reference must end with a semicolon")
        }
      peekChar.flatMap {
        case Some('x') =>
          advanceState >> readNum(16).flatMap(postlude(_))
        case Some(_) =>
          readNum(10).flatMap(postlude(_))
        case None => failState("66", "unexpected end of input", None)
      }
    }

    def readNum(base: Int): StatePull[Int] = {
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

      def restNum(acc: Int): StatePull[Int] =
        peekChar.flatMap {
          case Some(Digit(d)) => advanceState >> restNum(acc * base + d)
          case Some(_)        => pureState(acc)
          case None           => eosState(acc)
        }

      nextChar.flatMap {
        case Digit(d) => restNum(d)
        case _        => failAskState("66", "bad first character reference digit")
      }
    }

    // ==== middle-level internals ====

    def readAttributes(is11: Boolean, tname: QName): StatePull[List[Attr]] = {
      def loop(attributes: VectorBuilder[Attr]): StatePull[List[Attr]] =
        space >> {
          peekChar.flatMap {
            case Some(c) if isNCNameStart(c) =>
              for {
                name <- readQName
                _ <- space
                _ <- acceptChar('=', "25", "'=' character expected")
                _ <- space
                delimiter <- assert(c => c == '"' || c == '\'',
                                    "10",
                                    "single or double quote expected around attribute value")
                value <- readAttributeValue(is11, Some(delimiter), new StringBuilder, new VectorBuilder)
                res <- loop(attributes += Attr(name, value))
              } yield res
            case Some(_) => pureState(attributes.result().toList)
            case None    => failState("1", "unexpected end of input", None)
          }
        }
      loop(new VectorBuilder)
    }

    def readAttributeValue(is11: Boolean,
                           delim: Option[Char],
                           current: StringBuilder,
                           builder: VectorBuilder[XmlEvent.XmlTexty]): StatePull[List[XmlEvent.XmlTexty]] = {
      val delimiters = delim.fold(valueDelimiters)(valueDelimiters + _)
      untilChar(delimiters.contains(_), current) >> {
        nextChar.flatMap {
          case c if delim.contains(c) =>
            if (current.nonEmpty)
              builder += XmlEvent.XmlString(current.toString, false)
            pureState(builder.result().toList)
          case '\r' =>
            nextChar.flatMap {
              case '\n' => readAttributeValue(is11, delim, current.append('\n'), builder)
              case c    => readAttributeValue(is11, delim, current.append(' '), builder)
            }
          case c if isXmlWhitespace(c) =>
            readAttributeValue(is11, delim, current.append(' '), builder)
          case '&' =>
            builder += XmlEvent.XmlString(current.toString, false)
            peekChar.flatMap {
              case Some('#') =>
                advanceState >> readCharRef(is11).flatMap { n =>
                  builder += XmlEvent.XmlCharRef(n)
                  readAttributeValue(is11, delim, new StringBuilder, builder)
                }
              case Some(_) =>
                readNamedEntity.flatMap { s =>
                  builder += XmlEvent.XmlEntityRef(s)
                  readAttributeValue(is11, delim, new StringBuilder, builder)
                }
              case None =>
                failState("1", "unexpected end of input", None)
            }
          case c =>
            failAskState("10", s"unexpected character '$c'")
        }
      }
    }

    def readNamedEntity: StatePull[String] =
      readNCName.flatTap(_ => acceptChar(';', "68", "named entity must end with a semicolon"))

    def completeStartTag(is11: Boolean, name: QName): StatePull[XmlEvent.StartTag] =
      for {
        attributes <- readAttributes(is11, name)
        _ <- space
        isEmpty <- peekChar.flatMap {
          case Some('/') => advanceState >> pureState(true)
          case Some(_)   => pureState(false)
          case None      => failState[Boolean]("44", "unexpected end of input", None)
        }
        _ <- acceptChar('>', "44", "missing closing '>'")
      } yield XmlEvent.StartTag(name, attributes, isEmpty)

    /** We read '<[CDATA[' so far */
    def readCDATABody(sb: StringBuilder): StatePull[String] =
      untilChar(c => c == '\n' || c == '\r' || c == ']' || c == '&', sb) >> {
        nextChar.flatMap {
          case '\n' => readCDATABody(sb.append('\n'))
          case ']' =>
            peekChar.flatMap {
              case Some(']') =>
                advanceState >>
                  checkCDATAEnd(sb).flatMap {
                    case true  => pureState(sb.result())
                    case false => readCDATABody(sb)
                  }
              case Some(_) =>
                readCDATABody(sb.append(']'))
              case None =>
                failState("1", "unexpected end of input", None)
            }
          case '&' =>
            accept("gt;").flatMap { n =>
              if (n == 3) {
                sb.append('>')
              } else {
                sb.append("&gt;".substring(0, n + 1))
              }
              readCDATABody(sb)
            }
          case _ =>
            // must be '\r'
            peekChar.flatMap {
              case Some(c) =>
                if (c == '\n') advanceState >> readCDATABody(sb.append('\n'))
                else readCDATABody(sb.append(' '))
              case None =>
                failState("1", "unexpected end of input", None)
            }
        }
      }

    def checkCDATAEnd(sb: StringBuilder): StatePull[Boolean] =
      peekChar.flatMap {
        case Some('>') =>
          // done
          advanceState >> pureState(true)
        case Some(']') =>
          advanceState >> checkCDATAEnd(sb.append(']'))
        case Some(_) =>
          sb.append("]]")
          pureState(false)
        case None =>
          failState("1", "unexpected end of input", None)
      }

    def readCharData(is11: Boolean): StatePull[XmlEvent] =
      peekChar.flatMap {
        case Some('<') =>
          readMarkupToken.flatMap {
            case MarkupToken.CommentToken =>
              readCharData(is11)
            case MarkupToken.DeclToken(n) =>
              failAskState("14", s"unexpected declaration '$n'")
            case MarkupToken.CDataToken =>
              readCDATABody(new StringBuilder).map(XmlEvent.XmlString(_, true))
            case MarkupToken.EndToken(name) =>
              pureState(XmlEvent.EndTag(name))
            case MarkupToken.StartToken(name) =>
              completeStartTag(is11, name).widen
            case MarkupToken.PIToken(target) if !target.equalsIgnoreCase("xml") =>
              readPIBody.map(XmlEvent.XmlPI(target, _))
            case t =>
              failAskState("43", s"unexpected token ${t.render}")
          }
        case Some('&') =>
          advanceState >> peekChar.flatMap {
            case Some('#') =>
              advanceState >> readCharRef(is11).map(XmlEvent.XmlCharRef)
            case Some(_) =>
              readNamedEntity.map(XmlEvent.XmlEntityRef)
            case None =>
              failState[XmlEvent]("1", "unexpected end of input", None)
          }
        case Some(_) =>
          slowPath(new StringBuilder).widen
        case None =>
          eosState(XmlEvent.EndDocument)
      }

    def slowPath(sb: StringBuilder): StatePull[XmlEvent.XmlString] =
      untilChar(c => c == '<' || c == '&' || c == '\r', sb) >> {
        peekChar.flatMap {
          case Some('<') | Some('&') | None => pureState(XmlEvent.XmlString(sb.toString, false))
          case Some(_) =>
            advanceState >> peekChar.flatMap {
              case Some('\n') =>
                sb.append('\n')
                advanceState >> slowPath(sb)
              case Some(_) =>
                sb.append('\n')
                slowPath(sb)
              case None =>
                failState("14", "unexpected end of input", None)
            }
        }

      }

    // ==== high-level internals

    def scanPrologToken0: StatePull[Option[Unit]] =
      peekChar.flatMap {
        case Some('<') =>
          readMarkupToken.flatMap {
            case MarkupToken.PIToken(name) if name.equalsIgnoreCase("xml") =>
              handleXmlDecl.flatMap { case (is11, decl) =>
                appendState(decl) >> scanPrologToken1(is11)
              }
            case MarkupToken.PIToken(name) =>
              readPIBody.flatMap { body =>
                appendState(XmlEvent.XmlPI(name, body)) >> scanPrologToken1(false)
              }
            case MarkupToken.DeclToken(name) =>
              handleDecl(name) >> scanPrologToken2(false)
            case MarkupToken.StartToken(name) =>
              readElement(false, name).map(Some(_))
            case MarkupToken.CommentToken =>
              scanPrologToken1(false)
            case t =>
              failAskState("22", s"unexpected markup $t")
          }
        case Some(_) =>
          scanPrologToken1(false)
        case None =>
          pureState(None)
      }

    def scanPrologToken1(is11: Boolean): StatePull[Option[Unit]] =
      scanMisc.flatMap {
        case Some(MarkupToken.PIToken(name)) if !name.equalsIgnoreCase("xml") =>
          readPIBody.flatMap { body =>
            appendState(XmlEvent.XmlPI(name, body)) >> scanPrologToken1(is11)
          }
        case Some(MarkupToken.DeclToken(name)) =>
          handleDecl(name) >> scanPrologToken2(is11)
        case Some(MarkupToken.StartToken(name)) =>
          readElement(is11, name).map(Some(_))
        case Some(t) =>
          failAskState("22", s"unexpected markup $t")
        case None =>
          pureState(None)
      }

    def handleXmlDecl: StatePull[(Boolean, XmlEvent.XmlDecl)] =
      for {
        _ <- assert(isXmlWhitespace(_), "24", "space is expected after xml")
        _ <- space
        _ <- acceptString("version", "24", "expected 'version' attribute")
        _ <- space
        _ <- acceptChar('=', "24", "expected '=' after version")
        _ <- space
        delimiter <- assert(c => c == '"' || c == '\'', "24", "simple or double quote expected")
        _ <- acceptChar('1', "26", "expected major version 1")
        _ <- acceptChar('.', "26", "expected dot")
        sb = new StringBuilder("1.")
        _ <- untilChar(!_.isDigit, sb)
        version = sb.result()
        res <-
          if (version.length == 2) {
            failAskState("26", "expected non empty minor version")
          } else {
            for {
              _ <- acceptChar(delimiter, "24", "expected delimiter to close version attribute value")
              encodingDetails <- readEncoding(false)
              (hasSpace, encoding) = encodingDetails
              standalone <- readStandalone(hasSpace)
              _ <- space
              _ <- acceptString("?>", "23", "expected end of PI")
            } yield (version == "1.1", XmlEvent.XmlDecl(version, encoding, standalone))
          }
      } yield res

    def readEncoding(hasSpace: Boolean): StatePull[(Boolean, Option[String])] =
      peekChar.flatMap {
        case Some(c) if isXmlWhitespace(c) =>
          space >> readEncoding(true)
        case Some('e') =>
          if (hasSpace) {
            for {
              _ <- acceptString("encoding", "80", "expected 'encoding' attribute")
              _ <- space
              _ <- acceptChar('=', "80", "expected '='")
              _ <- space
              delimiter <- assert(c => c == '"' || c == '\'', "80", "simple or double quote expected")
              fst <- assert(c => (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z'),
                            "81",
                            "wrong encoding name character")
              sb = new StringBuilder().append(fst)
              _ <- untilChar(
                c =>
                  !((c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9') || c == '.' || c == '_' || c == '-'),
                sb)
              _ <- acceptChar(delimiter, "80", "'encoding' attribute value must end with proper delimiter")
            } yield (false, Some(sb.result()))
          } else {
            failAskState("80", "expected space before 'encoding' attribute")
          }
        case Some(_) =>
          pureState((hasSpace, None))
        case None =>
          eosState((hasSpace, None))
      }

    def readStandalone(hasSpace: Boolean): StatePull[Option[Boolean]] =
      peekChar.flatMap {
        case Some(c) if isXmlWhitespace(c) =>
          space >> readStandalone(true)
        case Some('s') =>
          if (hasSpace) {
            for {
              _ <- acceptString("standalone", "32", "expected 'standalone' attribute")
              _ <- space
              _ <- acceptChar('=', "32", "expected '='")
              _ <- space
              delimiter <- assert(c => c == '"' || c == '\'', "32", "simple or double quote expected")
              sa <- nextChar.flatMap {
                case 'y' =>
                  acceptString("es", "32", "expected 'yes' or 'no'").as(true)
                case 'n' =>
                  acceptChar('o', "32", "expected 'yes' or 'no'").as(false)
                case _ => failAskState[Boolean]("32", "expected 'yes' or 'no'")
              }
              _ <- acceptChar(delimiter, "32", "'standalone' attribute value must end with proper delimiter")
            } yield Some(sa)
          } else {
            failAskState("32", "expected space before 'standalone' attribute")
          }
        case Some(_) => pureState(None)
        case None    => eosState(None)
      }

    def handleDecl(name: String): StatePull[Unit] =
      name match {
        case "DOCTYPE" =>
          for {
            _ <- assert(isXmlWhitespace(_), "28", "space is expected after DOCTYPE")
            _ <- space
            docname <- readNCName
            _ <- space
            systemid <- peekChar.flatMap {
              case Some(c) if isNCNameStart(c) => readExternalID.map(_.some)
              case Some(_)                     => pureState(none[String])
              case None                        => eosState(none[String])
            }
            _ <- space
            res <- nextChar.flatMap {
              case '>' =>
                // done
                appendState(XmlEvent.XmlDoctype(name, docname, systemid))
              case '[' =>
                skipInternalDTD >> appendState(XmlEvent.XmlDoctype(name, docname, systemid))
              case c =>
                failAskState[Unit]("28", s"end of doctype or internal DTD expected but got $c")
            }
          } yield res
        case _ =>
          failAskState[Unit]("22", "expected DOCTYPE declaration")
      }

    def scanPrologToken2(is11: Boolean): StatePull[Option[Unit]] =
      scanMisc.flatMap {
        case Some(MarkupToken.PIToken(name)) =>
          readPIBody.flatMap { body =>
            appendState(XmlEvent.XmlPI(name, body)) >> scanPrologToken2(is11)
          }
        case Some(MarkupToken.StartToken(name)) =>
          readElement(is11, name).map(Some(_))
        case Some(t) =>
          failAskState("22", s"unexpected markup $t")
        case None =>
          pureState(None)
      }

    def readElement(is11: Boolean, name: QName): StatePull[Unit] =
      for {
        startTag <- completeStartTag(is11, name)
        _ <- appendState(startTag)
        res <-
          if (startTag.isEmpty) appendState(XmlEvent.EndTag(name))
          else readContent(is11, name)
      } yield res

    def readContent(is11: Boolean, name: QName): StatePull[Unit] =
      for {
        last <- readCharData(is11)
        res <- last match {
          case XmlEvent.EndTag(n) if n == name =>
            // we are done reading that content
            appendState(last)
          case XmlEvent.EndTag(n) =>
            failAskState("GIMatch", s"unexpected closing tag '</${n.render}>' (expected '</${name.render}>')")
          case XmlEvent.StartTag(name1, _, false) =>
            // parse child element, and continue
            appendState(last) >>
              readContent(is11, name1) >>
              readContent(is11, name)
          case XmlEvent.StartTag(name1, _, true) =>
            // parse child element, and continue
            appendState(last) >>
              appendState(XmlEvent.EndTag(name1)) >>
              readContent(is11, name)
          case _ =>
            // just emit and continue
            appendState(last) >>
              readContent(is11, name)
        }
      } yield res

    def go: StatePull[Unit] =
      scanPrologToken0.flatMap {
        case Some(_) => go
        case None    => StateT.liftF(Pull.done)
      }
    s => Stream.suspend(Stream.emit(T.create(s))).flatMap(go.runA(_, new VectorBuilder).stream)
  }

}
