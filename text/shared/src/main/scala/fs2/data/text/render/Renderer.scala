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

  /**
    * Splits words in the given text into a stream of document events.
    * This is a utility method, when you want to reformat a text using
    * the pretty printer. The pretty printing algorithm assumes that each
    * `DocEvent.Text` is an atomic value, so if it contains new lines it
    * can break the computations.
    *
    * Between 2 words, it adds a `DocEvent.Line` event. Empty lines are not
    * Generated.
    *
    * @param text The text to split
    * @param wordBoundary The regular expression on which to split words
    */
  def words(text: String, wordBoundary: String = raw"\s+"): Stream[Pure, DocEvent] =
    Stream
      .chunk(Chunk.array(text.split(wordBoundary)))
      .filter(_.nonEmpty)
      .map(DocEvent.Text(_))
      .intersperse(DocEvent.Line)

  /** Transforms the event into a stream of document events.
    * The stream may be partial (e.g. opening a group when the event describes a new tree node).
    */
  def doc(evt: Event): Stream[Pure, DocEvent]

}
