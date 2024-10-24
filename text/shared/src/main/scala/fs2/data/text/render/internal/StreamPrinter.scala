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

private case class OpenGroup(hpl: Int, indent: Int, group: Chain[Annotated])
private class AnnotationContext(var pos: Int,
                                var aligns: NonEmptyIntList,
                                var hpl: Int,
                                var indent: Int,
                                var groups: Dequeue[OpenGroup])
private class RenderingContext(var fit: Int, var hpl: Int, var lines: NonEmptyList[String], var col: Int)

private[render] class StreamPrinter[F[_], Event](width: Int, indentSize: Int)(implicit render: Renderable[Event])
    extends Pipe[F, Event, String] {

  private def push(annctx: AnnotationContext, evt: Annotated): Unit =
    annctx.groups.unsnoc match {
      case Some((OpenGroup(ghpl, gindent, group), groups)) =>
        annctx.groups = groups.snoc(OpenGroup(ghpl, gindent, group.append(evt)))
      case None => // should never happen
    }

  private def pop(buffer: Chain[Annotated],
                  annctx: AnnotationContext,
                  rctx: RenderingContext,
                  chunkAcc: StringBuilder): Unit =
    annctx.groups.unsnoc match {
      case Some((OpenGroup(ghpl, gindent, group), groups)) =>
        annctx.groups = groups.snoc(OpenGroup(ghpl, gindent, group.concat(buffer)))
      case None =>
        annctx.groups = Dequeue.empty
        buffer.iterator.foreach(renderAnnotated(_, rctx, chunkAcc))
    }

  private def check(annctx: AnnotationContext, rctx: RenderingContext, chunkAcc: StringBuilder): Unit =
    if (annctx.pos <= annctx.hpl - (annctx.indent * indentSize) && annctx.groups.size <= width - (annctx.indent * indentSize)) {
      // groups still fits
    } else {
      // group does not fit, uncons first buffer
      annctx.groups.uncons match {
        case Some((OpenGroup(_, _, buffer), groups)) =>
          renderGroupBegin(Position.TooFar, rctx)
          buffer.iterator.foreach(renderAnnotated(_, rctx, chunkAcc))
          groups.uncons match {
            case Some((OpenGroup(newhpl, newindent, _), _)) =>
              annctx.hpl = newhpl
              annctx.indent = newindent
              annctx.groups = groups
              check(annctx, rctx, chunkAcc) // check inner groups recursively
            case None =>
              annctx.hpl = 0
              annctx.indent = 0
              annctx.groups = Dequeue.empty
          }
        case None =>
        // should never happen
      }
    }

  private def process(chunk: Chunk[DocEvent],
                      chunkSize: Int,
                      idx: Int,
                      rest: Stream[F, DocEvent],
                      annctx: AnnotationContext,
                      rctx: RenderingContext,
                      chunkAcc: StringBuilder): Pull[F, String, Unit] =
    if (idx >= chunkSize) {
      Pull.output1(chunkAcc.result()) >>
        rest.pull.uncons.flatMap {
          case Some((hd, tl)) =>
            chunkAcc.setLength(0)
            process(hd, hd.size, 0, tl, annctx, rctx, chunkAcc)
          case None => Pull.done
        }
    } else {
      val evt = chunk(idx)
      evt match {
        case DocEvent.Text(text) =>
          val size = text.size
          annctx.pos += size
          if (annctx.groups.isEmpty) {
            // no open group we can emit immediately
            renderText(text, rctx, chunkAcc)
          } else {
            // there is an open group, append the event to the current group
            push(annctx, Annotated.Text(text))
            check(annctx, rctx, chunkAcc)
          }

        case DocEvent.Line =>
          annctx.pos += 1
          if (annctx.groups.isEmpty) {
            // no open group we can emit immediately a new line
            renderLine(annctx.pos, rctx, chunkAcc)
          } else {
            // there is an open group, append the event to the current group
            push(annctx, Annotated.Line(annctx.pos))
            check(annctx, rctx, chunkAcc)
          }

        case DocEvent.LineBreak =>
          if (annctx.groups.isEmpty) {
            // no open group we can emit immediately a new line
            renderLineBreak(annctx.pos, rctx, chunkAcc)
          } else {
            // there is an open group, append the event to the current group
            push(annctx, Annotated.LineBreak(annctx.pos))
            check(annctx, rctx, chunkAcc)
          }

        case DocEvent.GroupBegin =>
          val hpl1 = annctx.pos + width - annctx.aligns.head
          if (annctx.groups.isEmpty) {
            // this is the top-level group, turn on the buffer mechanism
            annctx.hpl = hpl1
            annctx.indent = annctx.aligns.head
            annctx.groups = annctx.groups.snoc(OpenGroup(hpl1, annctx.indent, Chain.empty))
          } else {
            // starting a new group, puts a new empty buffer in the group dequeue, and check for overflow
            annctx.groups = annctx.groups.snoc(OpenGroup(hpl1, annctx.aligns.head, Chain.empty))
            check(annctx, rctx, chunkAcc)
          }

        case DocEvent.GroupEnd =>
          annctx.groups.unsnoc match {
            case None =>
            // closing unknown group, just ignore it

            case Some((OpenGroup(newhpl, newindent, group), groups)) =>
              // closing a group, pop it from the buffer dequeue, and continue
              annctx.groups = groups
              pop(group.prepend(Annotated.GroupBegin(Position.Small(annctx.pos))).append(Annotated.GroupEnd),
                  annctx,
                  rctx,
                  chunkAcc)
              annctx.hpl = newhpl
              annctx.indent = newindent

          }

        case DocEvent.IndentBegin =>
          // increment the current indentation level
          annctx.aligns = annctx.aligns.incHead
          if (annctx.groups.isEmpty) {
            // no open group we can emit immediately a new line
            renderIndentBegin(rctx)
            annctx.indent += 1
          } else {
            // there is an open group, append the event to the current group
            push(annctx, Annotated.IndentBegin)
            check(annctx, rctx, chunkAcc)
          }

        case DocEvent.IndentEnd =>
          // decrement the current indentation level
          annctx.aligns = annctx.aligns.decHead
          if (annctx.groups.isEmpty) {
            // no open group we can emit immediately a new line
            renderIndentEnd(rctx)
            annctx.indent -= 1
          } else {
            // there is an open group, append the event to the current group
            push(annctx, Annotated.IndentEnd)
            check(annctx, rctx, chunkAcc)
          }

        case DocEvent.AlignBegin =>
          // push new indentation level
          annctx.aligns = annctx.pos :: annctx.aligns
          if (annctx.groups.isEmpty) {
            // no open group we can emit immediately a new line
            renderAlignBegin(rctx)
          } else {
            // there is an open group, append the event to the current group
            push(annctx, Annotated.AlignBegin)
            check(annctx, rctx, chunkAcc)
          }

        case DocEvent.AlignEnd =>
          // restore to previous indentation level
          annctx.aligns = annctx.aligns.pop
          if (annctx.groups.isEmpty) {
            // no open group we can emit immediately a new line
            renderAlignEnd(rctx)
          } else {
            // there is an open group, append the event to the current group
            push(annctx, Annotated.AlignEnd)
            check(annctx, rctx, chunkAcc)
          }
      }
      process(chunk, chunkSize, idx + 1, rest, annctx, rctx, chunkAcc)
    }

  // rendering

  private def renderText(text: String, ctx: RenderingContext, chunkAcc: StringBuilder): Unit = {
    ctx.col += text.size
    chunkAcc.append(text)
  }

  private def renderLine(pos: Int, ctx: RenderingContext, chunkAcc: StringBuilder): Unit = if (ctx.fit == 0) {
    ctx.hpl = pos + width
    ctx.col = ctx.lines.head.size
    val _ = chunkAcc.append('\n').append(ctx.lines.head)
  } else {
    ctx.col += 1
    chunkAcc.append(' ')
  }

  private def renderLineBreak(pos: Int, ctx: RenderingContext, chunkAcc: StringBuilder): Unit =
    if (ctx.fit == 0) {
      ctx.hpl = pos + width
      ctx.col = ctx.lines.head.size
      val _ = chunkAcc.append('\n').append(ctx.lines.head)
    }

  private def renderGroupBegin(pos: Position, ctx: RenderingContext): Unit =
    if (ctx.fit == 0) {
      pos match {
        case Position.TooFar =>
        // too far, do nothing
        case Position.Small(pos) =>
          ctx.fit = if (pos <= ctx.hpl) 1 else 0
      }
    } else {
      ctx.fit += 1
    }

  private def renderGroupEnd(ctx: RenderingContext): Unit =
    if (ctx.fit > 0) {
      ctx.fit -= 1
    }

  private def renderIndentBegin(ctx: RenderingContext): Unit = {
    ctx.lines = NonEmptyList(ctx.lines.head + (" " * indentSize), ctx.lines.tail)
  }

  private def renderIndentEnd(ctx: RenderingContext): Unit = {
    ctx.lines = NonEmptyList(ctx.lines.head.drop(indentSize), ctx.lines.tail)
  }

  private def renderAlignBegin(ctx: RenderingContext): Unit = {
    ctx.lines = (" " * ctx.col) :: ctx.lines
  }

  private def renderAlignEnd(ctx: RenderingContext): Unit = {
    ctx.lines = NonEmptyList.fromList(ctx.lines.tail).getOrElse(NonEmptyList.one(""))
  }

  private def renderAnnotated(annotated: Annotated, ctx: RenderingContext, chunkAcc: StringBuilder): Unit =
    annotated match {
      case Annotated.Text(text)      => renderText(text, ctx, chunkAcc)
      case Annotated.Line(pos)       => renderLine(pos, ctx, chunkAcc)
      case Annotated.LineBreak(pos)  => renderLineBreak(pos, ctx, chunkAcc)
      case Annotated.GroupBegin(pos) => renderGroupBegin(pos, ctx)
      case Annotated.GroupEnd        => renderGroupEnd(ctx)
      case Annotated.IndentBegin     => renderIndentBegin(ctx)
      case Annotated.IndentEnd       => renderIndentEnd(ctx)
      case Annotated.AlignBegin      => renderAlignBegin(ctx)
      case Annotated.AlignEnd        => renderAlignEnd(ctx)
    }

  def apply(events: Stream[F, Event]): Stream[F, String] =
    Stream.suspend(Stream.emit(render.newRenderer())).flatMap { renderer =>
      process(
        Chunk.empty,
        0,
        0,
        events.flatMap(renderer.doc(_)),
        new AnnotationContext(0, One(0), 0, 0, Dequeue.empty),
        new RenderingContext(0, width, NonEmptyList.one(""), 0),
        new StringBuilder
      ).stream

    }
}
