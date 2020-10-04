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

private[internals] case class Context[F[_]](chunk: Chunk[Char],
                                            idx: Int,
                                            rest: Stream[F, Char],
                                            chunkAcc: List[XmlEvent]) {
  def nextIdx: Context[F] = copy(idx = idx + 1)
  def isEndOfChunk: Boolean = idx >= chunk.size
  def accumulate(evt: XmlEvent, tail: XmlEvent*): Context[F] =
    copy(chunkAcc = tail.toList reverse_::: (evt :: chunkAcc))
}

private[internals] object Context {
  def eos[F[_]]: Context[F] = Context[F](Chunk.empty, 0, Stream.empty, Nil)
  def apply[F[_]](chunk: Chunk[Char], rest: Stream[F, Char]): Context[F] = Context(chunk, 0, rest, Nil)
}
