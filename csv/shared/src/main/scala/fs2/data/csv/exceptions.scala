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

package fs2.data.csv

class CsvException(msg: String, val line: Option[Long], inner: Throwable = null)
    extends Exception(line.fold(msg)(l => s"$msg in line $l"), inner) {
  def withLine(line: Option[Long]): CsvException = new CsvException(msg, line, inner)
}

class DecoderError(msg: String, override val line: Option[Long] = None, inner: Throwable = null)
    extends CsvException(msg, line, inner) {
  override def withLine(line: Option[Long]): DecoderError = new DecoderError(msg, line, inner)
}

class HeaderError(msg: String, override val line: Option[Long] = None, inner: Throwable = null)
    extends DecoderError(msg, line, inner) {
  override def withLine(line: Option[Long]): HeaderError = new HeaderError(msg, line, inner)
}

/** Raised when processing a Csv row whose width doesn't match the width of the Csv header row */
class HeaderSizeError(msg: String,
                      val expectedColumns: Int,
                      val actualColumns: Int,
                      override val line: Option[Long] = None,
                      inner: Throwable = null)
    extends HeaderError(msg, line, inner) {
  def this(expectedColumns: Int, actualColumns: Int, line: Option[Long]) =
    this(s"Headers have size $expectedColumns but row has size $actualColumns. Both numbers must match!",
         expectedColumns,
         actualColumns,
         line)

  override def withLine(line: Option[Long]): HeaderSizeError =
    new HeaderSizeError(msg, expectedColumns, actualColumns, line, inner)
}
