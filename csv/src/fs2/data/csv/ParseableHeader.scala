/*
 * Copyright 2019 Lucas Satabin
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
package fs2.data.csv

/** A typeclass describing what it means to be a parseable
  * CSV header.
  */
trait ParseableHeader[Header] {

  def parse(name: String): Header

}

object ParseableHeader {

  def apply[Header: ParseableHeader]: ParseableHeader[Header] =
    implicitly[ParseableHeader[Header]]

  implicit object StringParseableHeader extends ParseableHeader[String] {
    def parse(name: String) = name
  }

  implicit object NonEmptyStringParseableHeader extends ParseableHeader[Option[String]] {
    def parse(name: String) =
      if(name.isEmpty)
        None
      else
        Some(name)
  }

}
