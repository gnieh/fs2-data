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

package fs2.data.json
package circe

import io.circe._

object CirceJsonParserSpec extends JsonParserTest[Json] {
  // use the jawn parser to guarantee identical cross-platform semantics
  def parse(content: String): Either[Throwable, Json] =
    try {
      jawn.parse(content)
    } catch { // on Scala.js, jawn may throw fatal exceptions on inputs with implementation-specific behaviour
      case t: Throwable => Left(t)
    }
}
