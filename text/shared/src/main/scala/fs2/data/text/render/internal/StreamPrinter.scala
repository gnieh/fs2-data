/*
 * Copyright 2024 fs2-data Project
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

package fs2.data.text.render
package internal

import cats.collections.Dequeue
import cats.data.{Chain, NonEmptyList}
import fs2.{Chunk, Pipe, Pull, Stream}

private class RenderingContext(var fit: Int, var hpl: Int, var lines: NonEmptyList[String], var col: Int)

private[render] class StreamPrinter[F[_], Event](width: Int, indentSize: Int)(implicit render: Renderable[Event])
    extends Pipe[F, Event, String] {

  private val emptyGroups = (0, 0, Dequeue.empty[(Int, Int, Chain[Annotated])])

  private def push(groups: Dequeue[(Int, Int, Chain[Annotated])],
                   evt: Annotated): Dequeue[(Int, Int, Chain[Annotated])] =
    groups.unsnoc match {
      case Some(((ghpl, gindent, group), groups)) => groups.snoc((ghpl, gindent, group.append(evt)))
      case None                                   => Dequeue.empty // should never happen
    }

  private def pop(groups: Dequeue[(Int, Int, Chain[Annotated])],
                  buffer: Chain[Annotated]): Pull[F, Annotated, (Dequeue[(Int, Int, Chain[Annotated])])] =
    groups.unsnoc match {
      case Some(((ghpl, gindent, group), groups)) =>
        Pull.pure((groups.snoc((ghpl, gindent, group.concat(buffer)))))
      case None =>
        Pull.output(Chunk.chain(buffer)).as(Dequeue.empty)
    }

  private def check(hpl: Int,
                    indent: Int,
                    groups: Dequeue[(Int, Int, Chain[Annotated])],
                    ghpl: Int): Pull[F, Annotated, (Int, Int, Dequeue[(Int, Int, Chain[Annotated])])] =
    if (ghpl <= hpl - (indent * indentSize) && groups.size <= width - (indent * indentSize)) {
      // groups still fits
      Pull.pure((hpl, indent, groups))
    } else {
      // group does not fit, uncons first buffer
      groups.uncons match {
        case Some(((_, _, buffer), groups)) =>
          Pull.output(Chunk.chain(buffer.prepend(Annotated.GroupBegin(Position.TooFar)))) >> (groups.uncons match {
            case Some(((hpl, indent, _), _)) => check(hpl, indent, groups, ghpl) // check inner groups recursively
            case None                        => Pull.pure(emptyGroups)
          })
        case None =>
          Pull.pure(emptyGroups) // should never happen
      }
    }

  private def annotate(chunk: Chunk[DocEvent],
                       idx: Int,
                       rest: Stream[F, DocEvent],
                       pos: Int,
                       aligns: NonEmptyList[Int],
                       hpl: Int,
                       indent: Int,
                       groups: Dequeue[(Int, Int, Chain[Annotated])]): Pull[F, Annotated, Unit] =
    if (idx >= chunk.size) {
      rest.pull.uncons.flatMap {
        case Some((hd, tl)) => annotate(hd, 0, tl, pos, aligns, hpl, indent, groups)
        case None           => Pull.done
      }
    } else {
      val evt = chunk(idx)
      evt match {
        case DocEvent.Text(text) =>
          val size = text.size
          val pos1 = pos + size
          if (groups.isEmpty) {
            // no open group we can emit immediately
            Pull
              .output1(Annotated.Text(text)) >> annotate(chunk, idx + 1, rest, pos1, aligns, hpl, indent, groups)
          } else {
            // there is an open group, append the event to the current group
            check(hpl, indent, push(groups, Annotated.Text(text)), pos1).flatMap { case (hpl, gindent, groups) =>
              annotate(chunk, idx + 1, rest, pos1, aligns, hpl, gindent, groups)
            }
          }

        case DocEvent.Line =>
          if (groups.isEmpty) {
            // no open group we can emit immediately a new line
            Pull
              .output1(Annotated.Line(pos + 1)) >> annotate(chunk, idx + 1, rest, pos + 1, aligns, hpl, indent, groups)
          } else {
            // there is an open group, append the event to the current group
            check(hpl, indent, push(groups, Annotated.Line(pos + 1)), pos + 1).flatMap { case (hpl, indent, groups) =>
              annotate(chunk, idx + 1, rest, pos + 1, aligns, hpl, indent, groups)
            }
          }

        case DocEvent.LineBreak =>
          if (groups.isEmpty) {
            // no open group we can emit immediately a new line
            Pull.output1(Annotated.LineBreak(pos)) >> annotate(chunk, idx + 1, rest, pos, aligns, hpl, indent, groups)
          } else {
            // there is an open group, append the event to the current group
            check(hpl, indent, push(groups, Annotated.LineBreak(pos)), pos).flatMap { case (hpl, indent, groups) =>
              annotate(chunk, idx + 1, rest, pos, aligns, hpl, indent, groups)
            }
          }

        case DocEvent.GroupBegin =>
          val hpl1 = pos + width - aligns.head
          if (groups.isEmpty)
            // this is the top-level group, turn on the buffer mechanism
            annotate(chunk,
                     idx + 1,
                     rest,
                     pos,
                     aligns,
                     hpl1,
                     aligns.head,
                     groups.snoc((hpl1, aligns.head, Chain.empty)))
          else
            // starting a new group, puts a new empty buffer in the group dequeue, and check for overflow
            check(hpl, indent, groups.snoc((hpl1, aligns.head, Chain.empty)), pos).flatMap {
              case (hpl, indent, groups) =>
                annotate(chunk, idx + 1, rest, pos, aligns, hpl, indent, groups)
            }

        case DocEvent.GroupEnd =>
          groups.unsnoc match {
            case None =>
              // closing unknown group, just ignore it
              annotate(chunk, idx + 1, rest, pos, aligns, hpl, indent, groups)

            case Some(((hpl, indent, group), groups)) =>
              // closing a group, pop it from the buffer dequeue, and continue
              pop(groups, group.prepend(Annotated.GroupBegin(Position.Small(pos))).append(Annotated.GroupEnd))
                .flatMap { groups =>
                  annotate(chunk, idx + 1, rest, pos, aligns, hpl, indent, groups)
                }

          }

        case DocEvent.IndentBegin =>
          // increment the current indentation level
          if (groups.isEmpty) {
            // no open group we can emit immediately a new line
            Pull.output1(Annotated.IndentBegin) >> annotate(chunk,
                                                            idx + 1,
                                                            rest,
                                                            pos,
                                                            NonEmptyList(aligns.head + 1, aligns.tail),
                                                            hpl,
                                                            indent + 1,
                                                            groups)
          } else {
            // there is an open group, append the event to the current group
            check(hpl, indent, push(groups, Annotated.IndentBegin), pos).flatMap { case (hpl, indent, groups) =>
              annotate(chunk, idx + 1, rest, pos, NonEmptyList(aligns.head + 1, aligns.tail), hpl, indent, groups)
            }
          }

        case DocEvent.IndentEnd =>
          // decrement the current indentation level
          if (groups.isEmpty) {
            // no open group we can emit immediately a new line
            Pull.output1(Annotated.IndentEnd) >> annotate(chunk,
                                                          idx + 1,
                                                          rest,
                                                          pos,
                                                          NonEmptyList(aligns.head - 1, aligns.tail),
                                                          hpl,
                                                          indent - 1,
                                                          groups)
          } else {
            // there is an open group, append the event to the current group
            check(hpl, indent, push(groups, Annotated.IndentEnd), pos).flatMap { case (hpl, indent, groups) =>
              annotate(chunk, idx + 1, rest, pos, NonEmptyList(aligns.head - 1, aligns.tail), hpl, indent, groups)
            }
          }

        case DocEvent.AlignBegin =>
          // push new indentation level
          if (groups.isEmpty) {
            // no open group we can emit immediately a new line
            Pull
              .output1(Annotated.AlignBegin) >> annotate(chunk, idx + 1, rest, pos, pos :: aligns, hpl, indent, groups)
          } else {
            // there is an open group, append the event to the current group
            check(hpl, indent, push(groups, Annotated.AlignBegin), pos).flatMap { case (hpl, indent, groups) =>
              annotate(chunk, idx + 1, rest, pos, pos :: aligns, hpl, indent, groups)
            }
          }

        case DocEvent.AlignEnd =>
          // restore to previous indentation level
          val aligns1 =
            aligns match {
              case NonEmptyList(_, i :: is) => NonEmptyList(i, is)
              case NonEmptyList(_, Nil)     => NonEmptyList.one(0)
            }
          if (groups.isEmpty) {
            // no open group we can emit immediately a new line
            Pull.output1(Annotated.AlignEnd) >> annotate(chunk, idx + 1, rest, pos, aligns1, hpl, indent, groups)
          } else {
            // there is an open group, append the event to the current group
            check(hpl, indent, push(groups, Annotated.AlignEnd), pos).flatMap { case (hpl, indent, groups) =>
              annotate(chunk, idx + 1, rest, pos, aligns1, hpl, indent, groups)
            }
          }
      }
    }

  private def renderAnnotated(chunk: Chunk[Annotated],
                              chunkSize: Int,
                              idx: Int,
                              rest: Stream[F, Annotated],
                              ctx: RenderingContext,
                              chunkAcc: StringBuilder): Pull[F, String, Unit] =
    if (idx >= chunkSize) {
      Pull.output1(chunkAcc.result()) >> rest.pull.uncons.flatMap {
        case Some((hd, tl)) =>
          chunkAcc.setLength(0)
          renderAnnotated(hd, hd.size, 0, tl, ctx, chunkAcc)
        case None =>
          Pull.done
      }
    } else {
      chunk(idx) match {
        case Annotated.Text(text) =>
          ctx.col += text.size
          renderAnnotated(chunk, chunkSize, idx + 1, rest, ctx, chunkAcc.append(text))
        case Annotated.Line(pos) if ctx.fit == 0 =>
          ctx.hpl = pos + width
          ctx.col = ctx.lines.head.size
          renderAnnotated(chunk, chunkSize, idx + 1, rest, ctx, chunkAcc.append('\n').append(ctx.lines.head))
        case Annotated.Line(_) =>
          ctx.col += 1
          renderAnnotated(chunk, chunkSize, idx + 1, rest, ctx, chunkAcc.append(' '))
        case Annotated.LineBreak(pos) if ctx.fit == 0 =>
          ctx.hpl = pos + width
          ctx.col = ctx.lines.head.size
          renderAnnotated(chunk, chunkSize, idx + 1, rest, ctx, chunkAcc.append('\n').append(ctx.lines.head))
        case Annotated.LineBreak(_) =>
          renderAnnotated(chunk, chunkSize, idx + 1, rest, ctx, chunkAcc)
        case Annotated.GroupBegin(Position.TooFar) if ctx.fit == 0 =>
          renderAnnotated(chunk, chunkSize, idx + 1, rest, ctx, chunkAcc)
        case Annotated.GroupBegin(Position.Small(pos)) if ctx.fit == 0 =>
          ctx.fit = if (pos <= ctx.hpl) 1 else 0
          renderAnnotated(chunk, chunkSize, idx + 1, rest, ctx, chunkAcc)
        case Annotated.GroupBegin(_) =>
          ctx.fit += 1
          renderAnnotated(chunk, chunkSize, idx + 1, rest, ctx, chunkAcc)
        case Annotated.GroupEnd if ctx.fit == 0 =>
          renderAnnotated(chunk, chunkSize, idx + 1, rest, ctx, chunkAcc)
        case Annotated.GroupEnd =>
          ctx.fit -= 1
          renderAnnotated(chunk, chunkSize, idx + 1, rest, ctx, chunkAcc)
        case Annotated.IndentBegin =>
          ctx.lines = NonEmptyList(ctx.lines.head + (" " * indentSize), ctx.lines.tail)
          renderAnnotated(chunk, chunkSize, idx + 1, rest, ctx, chunkAcc)
        case Annotated.IndentEnd =>
          ctx.lines = NonEmptyList(ctx.lines.head.drop(indentSize), ctx.lines.tail)
          renderAnnotated(chunk, chunkSize, idx + 1, rest, ctx, chunkAcc)
        case Annotated.AlignBegin =>
          ctx.lines = (" " * ctx.col) :: ctx.lines
          renderAnnotated(chunk, chunkSize, idx + 1, rest, ctx, chunkAcc)
        case Annotated.AlignEnd =>
          ctx.lines = NonEmptyList.fromList(ctx.lines.tail).getOrElse(NonEmptyList.one(""))
          renderAnnotated(chunk, chunkSize, idx + 1, rest, ctx, chunkAcc)
      }
    }

  def apply(events: Stream[F, Event]): Stream[F, String] = {
    val annotated =
      annotate(
        Chunk.empty,
        0,
        Stream.suspend(Stream.emit(render.newRenderer())).flatMap(renderer => events.flatMap(renderer.doc(_))),
        0,
        NonEmptyList.one(0),
        0,
        0,
        Dequeue.empty
      ).streamNoScope

    Stream
      .suspend(Stream.emit(new RenderingContext(0, width, NonEmptyList.one(""), 0)))
      .flatMap(renderAnnotated(Chunk.empty, 0, 0, annotated, _, new StringBuilder).stream)
  }
}
