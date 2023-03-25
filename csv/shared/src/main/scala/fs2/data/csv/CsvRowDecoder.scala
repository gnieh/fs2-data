/*
 * Copyright 2023 Lucas Satabin
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

object CsvRowDecoder {

  @inline
  def apply[T: CsvRowDecoder[*, Header], Header]: CsvRowDecoder[T, Header] = implicitly[CsvRowDecoder[T, Header]]

  @inline
  def instance[T, Header](f: CsvRow[Header] => DecoderResult[T]): CsvRowDecoder[T, Header] = f(_)

}
