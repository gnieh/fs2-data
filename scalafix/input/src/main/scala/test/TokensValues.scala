/*
rule = json-parse
*/
package test

import fs2._
import fs2.data.json._

object Parser {
  def parse[Json](s: String)(implicit builder: ast.Builder[Json]) =
    Stream.emit(s).covary[Fallible].through(tokens).through(ast.values).compile.onlyOrError
}
