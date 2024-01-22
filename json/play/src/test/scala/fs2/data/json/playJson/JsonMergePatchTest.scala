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
package mergepatch

import playJson._
import diffson.playJson._

import _root_.play.api.libs.json._

object CirceJsonMergePatchTest extends JsonMergePatchTest[JsValue] {
  def makeInt(i: Int): JsValue = JsNumber.apply(BigDecimal(i))

  def makeString(s: String): JsValue = JsString(s)

  def makeTrue: JsValue = JsTrue
}
