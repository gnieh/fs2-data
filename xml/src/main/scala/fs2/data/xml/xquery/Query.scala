package fs2.data.xml
package xquery

import cats.data.NonEmptyList
import fs2.data.xml.xpath.Location

sealed trait Tree
sealed trait Query extends Tree
sealed trait Clause extends Query

case class Str(s: String) extends Tree

case class Element(name: QName, children: List[Tree]) extends Query

case class ForClause(variable: String, source: Ordpath, result: Query) extends Clause

case class LetClause(variable: String, query: Query, result: Query) extends Clause

case class ListClause(queries: NonEmptyList[Query]) extends Clause

case class Ordpath(variable: String, path: Option[List[Location]]) extends Clause
