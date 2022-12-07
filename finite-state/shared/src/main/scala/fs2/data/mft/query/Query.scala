package fs2.data.mft.query

import cats.data.NonEmptyList

/* An abstract representation of query language that consists of
 * nested for loops over some paths and tagged element construction.
 *
 * ''Note for implementers'': a path is always relative to the closes enclosing `for` clause.
 */
sealed trait Query[Tag, Path]
object Query {
  case class ForClause[Tag, Path](variable: String, source: Path, result: Query[Tag, Path]) extends Query[Tag, Path]
  case class LetClause[Tag, Path](variable: String, query: Query[Tag, Path], result: Query[Tag, Path])
      extends Query[Tag, Path]
  case class Ordpath[Tag, Path](path: Path) extends Query[Tag, Path]
  case class Variable[Tag, Path](name: String) extends Query[Tag, Path]
  case class Element[Tag, Path](tag: Tag, child: Option[Query[Tag, Path]]) extends Query[Tag, Path]
  case class Sequence[Tag, Path](elements: NonEmptyList[Query[Tag, Path]]) extends Query[Tag, Path]
}
