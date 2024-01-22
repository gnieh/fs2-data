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

package fs2
package data
package xml

import cats.{Eq, Show}

case class QName(prefix: Option[String], local: String) {

  def render: String = prefix match {
    case Some(prefix) => f"${prefix}:$local"
    case None         => local
  }

}

object QName {

  implicit val show: Show[QName] = _.render

  implicit val eq: Eq[QName] = Eq.fromUniversalEquals

  def apply(n: String): QName =
    QName(None, n)

}
