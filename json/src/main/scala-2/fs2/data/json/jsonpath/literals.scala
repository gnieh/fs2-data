/*
 * Copyright 2019-2022 Lucas Satabin
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
package json
package jsonpath

import org.typelevel.literally.Literally

import scala.language.experimental.macros

object literals {

  implicit class JsonPathStringContext(val sc: StringContext) extends AnyVal {
    def jsonpath(args: Any*): JsonPath = macro JsonPathInterpolator.make
  }

  object JsonPathInterpolator extends Literally[JsonPath] {

    def validate(c: Context)(string: String): Either[String, c.Expr[JsonPath]] = {
      import c.universe._
      JsonPathParser.either(string) match {
        case Left(t)  => Left(t.getMessage)
        case Right(v) => Right(c.Expr(q"_root_.fs2.data.json.jsonpath.JsonPathParser.either($string).toOption.get"))
      }
    }

    def make(c: Context)(args: c.Expr[Any]*): c.Expr[JsonPath] = apply(c)(args: _*)

  }
}
