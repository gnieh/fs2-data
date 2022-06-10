/*
 * Copyright 2022 Lucas Satabin
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
package dom

import cats.syntax.all._
import scala.collection.mutable.ListBuffer

class XmlTreeException(msg: String) extends Exception(msg)

class TreeParser[F[_], Node](implicit F: RaiseThrowable[F], builder: DocumentBuilder[Node]) {

  private def next(
      chunk: Chunk[XmlEvent],
      idx: Int,
      rest: Stream[F, XmlEvent]): Pull[F, INothing, (XmlEvent, Chunk[XmlEvent], Int, Stream[F, XmlEvent])] =
    peek(chunk, idx, rest).map { case (evt, chunk, idx, rest) => (evt, chunk, idx + 1, rest) }

  private def peek(
      chunk: Chunk[XmlEvent],
      idx: Int,
      rest: Stream[F, XmlEvent]): Pull[F, INothing, (XmlEvent, Chunk[XmlEvent], Int, Stream[F, XmlEvent])] =
    if (idx >= chunk.size) {
      rest.pull.uncons.flatMap {
        case Some((hd, tl)) => peek(hd, 0, tl)
        case None           => Pull.raiseError(new XmlTreeException("unexpected end of input"))
      }
    } else {
      Pull.pure((chunk(idx), chunk, idx, rest))
    }

  private def expect(evt: XmlEvent,
                     chunk: Chunk[XmlEvent],
                     idx: Int,
                     rest: Stream[F, XmlEvent]): Pull[F, INothing, (Chunk[XmlEvent], Int, Stream[F, XmlEvent])] =
    next(chunk, idx, rest).flatMap {
      case (`evt`, chunk, idx, rest) => Pull.pure((chunk, idx, rest))
      case (evt, _, _, _)            => Pull.raiseError(new XmlTreeException(s"unepexted event '$evt'"))
    }

  private def prolog(chunk: Chunk[XmlEvent], idx: Int, rest: Stream[F, XmlEvent]): Pull[F,
                                                                                        INothing,
                                                                                        (Option[XmlEvent.XmlDecl],
                                                                                         Option[XmlEvent.XmlDoctype],
                                                                                         List[builder.Misc],
                                                                                         Chunk[XmlEvent],
                                                                                         Int,
                                                                                         Stream[F, XmlEvent])] =
    peek(chunk, idx, rest)
      .map {
        case (decl @ XmlEvent.XmlDecl(_, _, _), chunk, idx, rest) =>
          (Some(decl), chunk, idx + 1, rest)
        case (_, chunk, idx, rest) =>
          (None, chunk, idx, rest)
      }
      .flatMap { case (decl, chunk, idx, rest) =>
        (chunk, idx, rest, none[XmlEvent.XmlDoctype], new ListBuffer[builder.Misc]).tailRecM {
          case (chunk, idx, rest, doctype, misc) =>
            peek(chunk, idx, rest).flatMap {
              case (dt @ XmlEvent.XmlDoctype(_, _, _), chunk, idx, rest) =>
                doctype match {
                  case Some(_) => Pull.raiseError(new XmlTreeException("duplicate doctype"))
                  case None    => Pull.pure((chunk, idx + 1, rest, Some(dt), misc).asLeft)
                }
              case (XmlEvent.Comment(comment), chunk, idx, rest) =>
                Pull.pure((chunk, idx + 1, rest, doctype, misc ++= builder.makeComment(comment)).asLeft)
              case (XmlEvent.XmlPI(target, content), chunk, idx, rest) =>
                Pull.pure((chunk, idx + 1, rest, doctype, misc += builder.makePI(target, content)).asLeft)
              case (_, chunk, idx, rest) =>
                Pull.pure((decl, doctype, misc.result(), chunk, idx, rest).asRight)
            }
        }
      }

  private def element(
      chunk: Chunk[XmlEvent],
      idx: Int,
      rest: Stream[F, XmlEvent]): Pull[F, INothing, (builder.Elem, Chunk[XmlEvent], Int, Stream[F, XmlEvent])] =
    next(chunk, idx, rest).flatMap {
      case (XmlEvent.StartTag(name, attrs, isEmpty), chunk, idx, rest) =>
        (chunk, idx, rest, new ListBuffer[builder.Content]).tailRecM { case (chunk, idx, rest, children) =>
          peek(chunk, idx, rest).flatMap {
            case (XmlEvent.EndTag(`name`), chunk, idx, rest) =>
              Pull.pure((builder.makeElement(name, attrs, isEmpty, children.result()), chunk, idx + 1, rest).asRight)
            case (XmlEvent.EndTag(name), _, _, _) =>
              Pull.raiseError(new XmlTreeException(s"unexpected closing tag '$name'"))
            case (XmlEvent.StartTag(_, _, _), chunk, idx, rest) =>
              element(chunk, idx, rest).map { case (node, chunk, idx, rest) =>
                (chunk, idx, rest, children += node).asLeft
              }
            case (texty: XmlEvent.XmlTexty, chunk, idx, rest) =>
              Pull.pure((chunk, idx + 1, rest, children += builder.makeText(texty)).asLeft)
            case (XmlEvent.Comment(comment), chunk, idx, rest) =>
              Pull.pure((chunk, idx + 1, rest, children ++= builder.makeComment(comment)).asLeft)
            case (XmlEvent.XmlPI(target, content), chunk, idx, rest) =>
              Pull.pure((chunk, idx + 1, rest, children += builder.makePI(target, content)).asLeft)
            case (evt, _, _, _) =>
              Pull.raiseError(new XmlTreeException(s"unexpected event '$evt'"))
          }
        }
      case (evt, _, _, _) =>
        Pull.raiseError(new XmlTreeException(s"unexpected event '$evt'"))
    }

  private def postlog(
      chunk: Chunk[XmlEvent],
      idx: Int,
      rest: Stream[F, XmlEvent]): Pull[F, INothing, (List[builder.Misc], Chunk[XmlEvent], Int, Stream[F, XmlEvent])] =
    (chunk, idx, rest, new ListBuffer[builder.Misc]).tailRecM { case (chunk, idx, rest, misc) =>
      peek(chunk, idx, rest).flatMap {
        case (XmlEvent.Comment(comment), chunk, idx, rest) =>
          Pull.pure((chunk, idx + 1, rest, misc ++= builder.makeComment(comment)).asLeft)
        case (XmlEvent.XmlPI(target, content), chunk, idx, rest) =>
          Pull.pure((chunk, idx + 1, rest, misc += builder.makePI(target, content)).asLeft)
        case (_, chunk, idx, rest) =>
          Pull.pure((misc.result(), chunk, idx, rest).asRight)
      }
    }

  private def document(chunk: Chunk[XmlEvent],
                       idx: Int,
                       rest: Stream[F, XmlEvent]): Pull[F, Node, (Chunk[XmlEvent], Int, Stream[F, XmlEvent])] =
    next(chunk, idx, rest).flatMap {
      case (XmlEvent.StartDocument, chunk, idx, rest) =>
        for {
          (decl, doctype, prolog, chunk, idx, rest) <- prolog(chunk, idx, rest)
          (node, chunk, idx, rest) <- element(chunk, idx, rest)
          (postlog, chunk, idx, rest) <- postlog(chunk, idx, rest)
          (chunk, idx, rest) <- expect(XmlEvent.EndDocument, chunk, idx, rest)
          () <- Pull.output1(
            builder.makeDocument(decl.map(_.version),
                                 decl.flatMap(_.encoding),
                                 decl.flatMap(_.standalone),
                                 doctype,
                                 prolog,
                                 node,
                                 postlog))
        } yield (chunk, idx, rest)
      case (evt, _, _, _) => Pull.raiseError(new XmlTreeException(s"unexpected event '$evt'"))
    }

  def pipe: Pipe[F, XmlEvent, Node] = {
    def go(chunk: Chunk[XmlEvent], idx: Int, rest: Stream[F, XmlEvent]): Pull[F, Node, Unit] =
      if (idx >= chunk.size) {
        rest.pull.uncons.flatMap {
          case Some((hd, tl)) => go(hd, 0, tl)
          case None           => Pull.done
        }
      } else {
        document(chunk, idx, rest).flatMap { case (chunk, idx, rest) => go(chunk, idx, rest) }
      }
    s => go(Chunk.empty, 0, s).stream
  }

}
