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

import cats.Show
import cats.implicits._

/** A JSON trace context, positioning the current location in the JSON value
  * starting from the root.
  */
sealed trait JsonContext

object JsonContext {
  case object Root extends JsonContext
  case class Key(name: String, parent: JsonContext) extends JsonContext
  case class Index(idx: Long, parent: JsonContext) extends JsonContext

  implicit object JsonContextShow extends Show[JsonContext] {
    def show(t: JsonContext): String = t match {
      case Root               => "/"
      case Key(name, Root)    => show"/$name"
      case Key(name, parent)  => show"$parent/$name"
      case Index(idx, Root)   => show"/$idx"
      case Index(idx, parent) => show"$parent/$idx"
    }
  }
}
