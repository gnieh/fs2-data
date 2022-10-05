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
package csv
package internals

private[csv] object RowWriter {

  def encodeColumn(separator: Char, mode: EscapeMode)(in: String): String = {
    if (mode == EscapeMode.Never) {
      in
    } else {
      val (escape, extraChars) = needsQuotes(in, separator)
      if (escape || mode == EscapeMode.Always) {
        val sb = new StringBuilder(in.length + extraChars + 2)
        sb.append("\"")
        in.foreach {
          case '"' => sb.append("\"\"")
          case c   => sb.append(c)
        }
        sb.append("\"")
        sb.result()
      } else {
        in
      }
    }
  }

  private def needsQuotes(in: String, separator: Char): (Boolean, Int) = {
    in.foldLeft(false -> 0) {
      case ((_, extraChars), `separator`) => true -> extraChars
      case ((_, extraChars), '\n')        => true -> extraChars
      case ((_, extraChars), '"')         => true -> (extraChars + 1)
      case (state, _)                     => state
    }
  }

}
