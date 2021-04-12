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

class CsvException(msg: String, inner: Throwable = null) extends Exception(msg, inner)

class DecoderError(msg: String, inner: Throwable = null) extends CsvException(msg, inner)

object DecoderError {

  /** Signals that a column was missing. Use this exception in your own decoders in this case to ensure
    * correct behaviour when using defaults and @CsvEmbed.
    */
  class ColumnMissing(msg: String, inner: Throwable = null) extends DecoderError(msg, inner)
}

class HeaderError(msg: String, inner: Throwable = null) extends CsvException(msg, inner)

/** Raised when processing a Csv row whose width doesn't match the width of the Csv header row */
class HeaderSizeError(msg: String, val expectedColumns: Int, val actualColumns: Int, inner: Throwable = null)
    extends HeaderError(msg, inner)
