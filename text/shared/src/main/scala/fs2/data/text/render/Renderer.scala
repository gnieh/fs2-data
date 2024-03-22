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

import fs2.{Chunk, Pure, Stream}

trait Renderer[Event] {

  /** Behaves like a new line, or as a space if undone by a group. */
  val line: Stream[Pure, DocEvent] =
    Stream.emit(DocEvent.Line)

  /** Behaves like a new line, or as empty if undone by a group. */
  val linebreak: Stream[Pure, DocEvent] =
    Stream.emit(DocEvent.LineBreak)

  /** Behaves like a space if it fits on the page, otherwise
    * as a new line. */
  val softline: Stream[Pure, DocEvent] =
    Stream.emits(DocEvent.GroupBegin :: DocEvent.Line :: DocEvent.GroupEnd :: Nil)

  /** Empty if it fits on the page, otherwise renders a new line. */
  val softbreak: Stream[Pure, DocEvent] =
    Stream.emits(DocEvent.GroupBegin :: DocEvent.LineBreak :: DocEvent.GroupEnd :: Nil)

  /** Increment current indentation level by one. */
  val indent: Stream[Pure, DocEvent] =
    Stream.emit(DocEvent.IndentBegin)

  /** Decrement current indentation level by one. */
  val unindent: Stream[Pure, DocEvent] =
    Stream.emit(DocEvent.IndentEnd)

  /**
    * Splits words in the given text into a stream of document events.
    * This is a utility method, when you want to reformat a text using
    * the pretty printer. The pretty printing algorithm assumes that each
    * `DocEvent.Text` is an atomic value, so if it contains new lines it
    * can break the computations.
    *
    * Between 2 words, it adds a `DocEvent.Line` event. Empty lines are
    * represented as two consecutive line breaks.
    *
    * @param text The text to split
    * @param wordBoundary The regular expression on which to split words
    */
  def words(text: String, wordBoundary: String = raw"\s+"): Stream[Pure, DocEvent] =
    Stream
      .emit(text.trim())
      .through(fs2.text.lines)
      .map { line =>
        if (line.matches(raw"\s*")) {
          // empty line
          Stream.emit(DocEvent.LineBreak)
        } else {
          // split line words
          Stream
            .chunk(Chunk.array(line.split(wordBoundary)))
            .map(w => Stream.emit(DocEvent.Text(w)))
            .intersperse(softline)
            .flatten
        }
      }
      .intersperse(softline)
      .flatten

  /** Transforms the event into a stream of document events.
    * The stream may be partial (e.g. opening a group when the event describes a new tree node).
    */
  def doc(evt: Event): Stream[Pure, DocEvent]

}
