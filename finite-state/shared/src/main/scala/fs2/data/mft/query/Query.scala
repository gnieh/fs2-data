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

package fs2.data.mft.query

import cats.data.NonEmptyList

/* An abstract representation of query language that consists of
 * nested for loops over some paths and tagged element construction.
 *
 * ''Note for implementers'': a path is always relative to the closes enclosing `for` clause.
 */
sealed trait Query[Tag, Path]
object Query {
  case class Empty[Tag, Path]() extends Query[Tag, Path]
  case class ForClause[Tag, Path](variable: String, source: Path, result: Query[Tag, Path]) extends Query[Tag, Path]
  case class LetClause[Tag, Path](variable: String, query: Query[Tag, Path], result: Query[Tag, Path])
      extends Query[Tag, Path]
  case class Ordpath[Tag, Path](path: Path, default: Option[Tag]) extends Query[Tag, Path]
  case class Variable[Tag, Path](name: String) extends Query[Tag, Path]
  case class Node[Tag, Path](tag: Tag, child: Query[Tag, Path]) extends Query[Tag, Path]
  case class Leaf[Tag, Path](tag: Tag) extends Query[Tag, Path]
  case class Sequence[Tag, Path](elements: NonEmptyList[Query[Tag, Path]]) extends Query[Tag, Path]
  case class LeafFunction[Tag, Path](f: Tag => Either[String, Tag]) extends Query[Tag, Path]

  def empty[Tag, Path]: Query[Tag, Path] = Empty()
  def variable[Tag, Path](v: String): Query[Tag, Path] = Variable(v)
  def node[Tag, Path](tag: Tag, child: Query[Tag, Path]): Query[Tag, Path] = Node(tag, child)
}
