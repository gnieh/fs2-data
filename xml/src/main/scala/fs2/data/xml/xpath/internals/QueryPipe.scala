package fs2
package data
package xml
package xpath
package internals

import automaton._

import cats.effect.Concurrent
import cats.effect.std.Queue
import cats.syntax.all._

private[xpath] class QueryPipe[F[_]: Concurrent](dfa: PDFA[LocationMatch, StartElement])
    extends Pipe[F, XmlEvent, Stream[F, XmlEvent]] {

  private def resolveAttr(attrs: List[Attr]): Map[QName, String] =
    attrs.map { case Attr(n, v) => (n, v.widen[XmlEvent].mkString_("")) }.toMap

  private def go(chunk: Chunk[XmlEvent],
                 idx: Int,
                 rest: Stream[F, XmlEvent],
                 depth: Int,
                 queues: List[(Int, Queue[F, Option[XmlEvent]])],
                 resetting: Boolean,
                 q: Int): Pull[F, Stream[F, XmlEvent], Unit] =
    if (idx >= chunk.size) {
      rest.pull.uncons.flatMap {
        case Some((hd, tl)) => go(hd, 0, tl, depth = depth, queues, resetting, q = q)
        case None           => Pull.done
      }
    } else {
      chunk(idx) match {
        case evt @ XmlEvent.StartTag(name, attr, isEmpty) =>
          dfa.step(q, StartElement(name, resolveAttr(attr))) match {
            case Some(q) =>
              val updateQueues =
                if (!resetting && dfa.finals.contains(q)) {
                  // this is a new match, spawn a new down stream
                  Pull.eval(Queue.unbounded[F, Option[XmlEvent]]).flatMap { queue =>
                    Pull.output1(Stream.fromQueueNoneTerminated(queue, 1)).as((depth, queue) :: queues)
                  }
                } else {
                  Pull.pure(queues)
                }
              updateQueues
                .evalMap { queues =>
                  queues.traverse_(_._2.offer(Some(evt))).as(queues)
                }
                .flatMap(go(chunk, idx + 1, rest, depth = depth + 1, _, resetting, q = q))
            case None =>
              Pull.eval(queues.traverse_(_._2.offer(Some(evt)))) >> go(chunk,
                                                                       idx + 1,
                                                                       rest,
                                                                       depth = depth + 1,
                                                                       queues,
                                                                       true,
                                                                       q = q)
          }
        case evt @ XmlEvent.EndTag(_) =>
          val (top, nested) = queues.span(_._1 == depth - 1)
          Pull.eval(queues.traverse_(_._2.offer(Some(evt)))) >> Pull
            .eval(top.traverse_(_._2.offer(None))) >> go(chunk,
                                                         idx + 1,
                                                         rest,
                                                         depth - 1,
                                                         nested,
                                                         if (depth == 1) false else resetting,
                                                         if (depth == 1) dfa.init else q)
        case evt =>
          Pull.eval(queues.traverse_(_._2.offer(Some(evt)))) >> go(chunk, idx + 1, rest, depth, queues, resetting, q)
      }
    }

  def apply(s: Stream[F, XmlEvent]): Stream[F, Stream[F, XmlEvent]] =
    go(Chunk.empty, 0, s, 0, Nil, false, dfa.init).stream

}
