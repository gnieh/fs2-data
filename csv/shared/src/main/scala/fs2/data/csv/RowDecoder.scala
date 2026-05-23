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

import cats.syntax.all._

/** Describes how a row can be decoded to the given type.
  *
  * `RowDecoder` provides convenient methods such as `map`, `emap`, or `flatMap`
  * to build new decoders out of more basic one.
  *
  * Actually, `RowDecoder` has a [[https://typelevel.org/cats/api/cats/MonadError.html cats `MonadError`]]
  * instance. To get the full power of it, import `cats.syntax.all._`.
  */

object RowDecoder {
  @inline
  def apply[T: RowDecoder]: RowDecoder[T] = implicitly[RowDecoder[T]]

  @inline
  def instance[T](f: Row => DecoderResult[T]): RowDecoder[T] = row => f(row)

  def forColumns[T, C1: CellDecoder](f: C1 => T): RowDecoder[T] =
    instance(row => row.asAt[C1](0).map(f))

  def forColumns[T, C1: CellDecoder, C2: CellDecoder](f: (C1, C2) => T): RowDecoder[T] =
    instance(row => (row.asAt[C1](0), row.asAt[C2](1)).mapN(f))

  def forColumns[T, C1: CellDecoder, C2: CellDecoder, C3: CellDecoder](f: (C1, C2, C3) => T): RowDecoder[T] =
    instance(row => (row.asAt[C1](0), row.asAt[C2](1), row.asAt[C3](2)).mapN(f))

  def forColumns[T, C1: CellDecoder, C2: CellDecoder, C3: CellDecoder, C4: CellDecoder](
      f: (C1, C2, C3, C4) => T): RowDecoder[T] =
    instance(row => (row.asAt[C1](0), row.asAt[C2](1), row.asAt[C3](2), row.asAt[C4](3)).mapN(f))

  def forColumns[T, C1: CellDecoder, C2: CellDecoder, C3: CellDecoder, C4: CellDecoder, C5: CellDecoder](
      f: (C1, C2, C3, C4, C5) => T): RowDecoder[T] =
    instance(row => (row.asAt[C1](0), row.asAt[C2](1), row.asAt[C3](2), row.asAt[C4](3), row.asAt[C5](4)).mapN(f))

  def forColumns[T,
                 C1: CellDecoder,
                 C2: CellDecoder,
                 C3: CellDecoder,
                 C4: CellDecoder,
                 C5: CellDecoder,
                 C6: CellDecoder](f: (C1, C2, C3, C4, C5, C6) => T): RowDecoder[T] =
    instance(row =>
      (row.asAt[C1](0), row.asAt[C2](1), row.asAt[C3](2), row.asAt[C4](3), row.asAt[C5](4), row.asAt[C6](5)).mapN(f))

  def forColumns[T,
                 C1: CellDecoder,
                 C2: CellDecoder,
                 C3: CellDecoder,
                 C4: CellDecoder,
                 C5: CellDecoder,
                 C6: CellDecoder,
                 C7: CellDecoder](f: (C1, C2, C3, C4, C5, C6, C7) => T): RowDecoder[T] =
    instance(row =>
      (row.asAt[C1](0),
       row.asAt[C2](1),
       row.asAt[C3](2),
       row.asAt[C4](3),
       row.asAt[C5](4),
       row.asAt[C6](5),
       row.asAt[C7](6)).mapN(f))

  def forColumns[T,
                 C1: CellDecoder,
                 C2: CellDecoder,
                 C3: CellDecoder,
                 C4: CellDecoder,
                 C5: CellDecoder,
                 C6: CellDecoder,
                 C7: CellDecoder,
                 C8: CellDecoder](f: (C1, C2, C3, C4, C5, C6, C7, C8) => T): RowDecoder[T] =
    instance(row =>
      (row.asAt[C1](0),
       row.asAt[C2](1),
       row.asAt[C3](2),
       row.asAt[C4](3),
       row.asAt[C5](4),
       row.asAt[C6](5),
       row.asAt[C7](6),
       row.asAt[C8](7)).mapN(f))

  def forColumns[T,
                 C1: CellDecoder,
                 C2: CellDecoder,
                 C3: CellDecoder,
                 C4: CellDecoder,
                 C5: CellDecoder,
                 C6: CellDecoder,
                 C7: CellDecoder,
                 C8: CellDecoder,
                 C9: CellDecoder](f: (C1, C2, C3, C4, C5, C6, C7, C8, C9) => T): RowDecoder[T] =
    instance(row =>
      (row.asAt[C1](0),
       row.asAt[C2](1),
       row.asAt[C3](2),
       row.asAt[C4](3),
       row.asAt[C5](4),
       row.asAt[C6](5),
       row.asAt[C7](6),
       row.asAt[C8](7),
       row.asAt[C9](8)).mapN(f))

  def forColumns[T,
                 C1: CellDecoder,
                 C2: CellDecoder,
                 C3: CellDecoder,
                 C4: CellDecoder,
                 C5: CellDecoder,
                 C6: CellDecoder,
                 C7: CellDecoder,
                 C8: CellDecoder,
                 C9: CellDecoder,
                 C10: CellDecoder](f: (C1, C2, C3, C4, C5, C6, C7, C8, C9, C10) => T): RowDecoder[T] =
    instance(row =>
      (row.asAt[C1](0),
       row.asAt[C2](1),
       row.asAt[C3](2),
       row.asAt[C4](3),
       row.asAt[C5](4),
       row.asAt[C6](5),
       row.asAt[C7](6),
       row.asAt[C8](7),
       row.asAt[C9](8),
       row.asAt[C10](9)).mapN(f))

  def forColumns[T,
                 C1: CellDecoder,
                 C2: CellDecoder,
                 C3: CellDecoder,
                 C4: CellDecoder,
                 C5: CellDecoder,
                 C6: CellDecoder,
                 C7: CellDecoder,
                 C8: CellDecoder,
                 C9: CellDecoder,
                 C10: CellDecoder,
                 C11: CellDecoder](f: (C1, C2, C3, C4, C5, C6, C7, C8, C9, C10, C11) => T): RowDecoder[T] =
    instance(row =>
      (row.asAt[C1](0),
       row.asAt[C2](1),
       row.asAt[C3](2),
       row.asAt[C4](3),
       row.asAt[C5](4),
       row.asAt[C6](5),
       row.asAt[C7](6),
       row.asAt[C8](7),
       row.asAt[C9](8),
       row.asAt[C10](9),
       row.asAt[C11](10)).mapN(f))

  def forColumns[T,
                 C1: CellDecoder,
                 C2: CellDecoder,
                 C3: CellDecoder,
                 C4: CellDecoder,
                 C5: CellDecoder,
                 C6: CellDecoder,
                 C7: CellDecoder,
                 C8: CellDecoder,
                 C9: CellDecoder,
                 C10: CellDecoder,
                 C11: CellDecoder,
                 C12: CellDecoder](f: (C1, C2, C3, C4, C5, C6, C7, C8, C9, C10, C11, C12) => T): RowDecoder[T] =
    instance(row =>
      (row.asAt[C1](0),
       row.asAt[C2](1),
       row.asAt[C3](2),
       row.asAt[C4](3),
       row.asAt[C5](4),
       row.asAt[C6](5),
       row.asAt[C7](6),
       row.asAt[C8](7),
       row.asAt[C9](8),
       row.asAt[C10](9),
       row.asAt[C11](10),
       row.asAt[C12](11)).mapN(f))

  def forColumns[T,
                 C1: CellDecoder,
                 C2: CellDecoder,
                 C3: CellDecoder,
                 C4: CellDecoder,
                 C5: CellDecoder,
                 C6: CellDecoder,
                 C7: CellDecoder,
                 C8: CellDecoder,
                 C9: CellDecoder,
                 C10: CellDecoder,
                 C11: CellDecoder,
                 C12: CellDecoder,
                 C13: CellDecoder](f: (C1, C2, C3, C4, C5, C6, C7, C8, C9, C10, C11, C12, C13) => T): RowDecoder[T] =
    instance(row =>
      (row.asAt[C1](0),
       row.asAt[C2](1),
       row.asAt[C3](2),
       row.asAt[C4](3),
       row.asAt[C5](4),
       row.asAt[C6](5),
       row.asAt[C7](6),
       row.asAt[C8](7),
       row.asAt[C9](8),
       row.asAt[C10](9),
       row.asAt[C11](10),
       row.asAt[C12](11),
       row.asAt[C13](12)).mapN(f))

  def forColumns[T,
                 C1: CellDecoder,
                 C2: CellDecoder,
                 C3: CellDecoder,
                 C4: CellDecoder,
                 C5: CellDecoder,
                 C6: CellDecoder,
                 C7: CellDecoder,
                 C8: CellDecoder,
                 C9: CellDecoder,
                 C10: CellDecoder,
                 C11: CellDecoder,
                 C12: CellDecoder,
                 C13: CellDecoder,
                 C14: CellDecoder](
      f: (C1, C2, C3, C4, C5, C6, C7, C8, C9, C10, C11, C12, C13, C14) => T): RowDecoder[T] =
    instance(row =>
      (row.asAt[C1](0),
       row.asAt[C2](1),
       row.asAt[C3](2),
       row.asAt[C4](3),
       row.asAt[C5](4),
       row.asAt[C6](5),
       row.asAt[C7](6),
       row.asAt[C8](7),
       row.asAt[C9](8),
       row.asAt[C10](9),
       row.asAt[C11](10),
       row.asAt[C12](11),
       row.asAt[C13](12),
       row.asAt[C14](13)).mapN(f))

  def forColumns[T,
                 C1: CellDecoder,
                 C2: CellDecoder,
                 C3: CellDecoder,
                 C4: CellDecoder,
                 C5: CellDecoder,
                 C6: CellDecoder,
                 C7: CellDecoder,
                 C8: CellDecoder,
                 C9: CellDecoder,
                 C10: CellDecoder,
                 C11: CellDecoder,
                 C12: CellDecoder,
                 C13: CellDecoder,
                 C14: CellDecoder,
                 C15: CellDecoder](
      f: (C1, C2, C3, C4, C5, C6, C7, C8, C9, C10, C11, C12, C13, C14, C15) => T): RowDecoder[T] =
    instance(row =>
      (row.asAt[C1](0),
       row.asAt[C2](1),
       row.asAt[C3](2),
       row.asAt[C4](3),
       row.asAt[C5](4),
       row.asAt[C6](5),
       row.asAt[C7](6),
       row.asAt[C8](7),
       row.asAt[C9](8),
       row.asAt[C10](9),
       row.asAt[C11](10),
       row.asAt[C12](11),
       row.asAt[C13](12),
       row.asAt[C14](13),
       row.asAt[C15](14)).mapN(f))

  def forColumns[T,
                 C1: CellDecoder,
                 C2: CellDecoder,
                 C3: CellDecoder,
                 C4: CellDecoder,
                 C5: CellDecoder,
                 C6: CellDecoder,
                 C7: CellDecoder,
                 C8: CellDecoder,
                 C9: CellDecoder,
                 C10: CellDecoder,
                 C11: CellDecoder,
                 C12: CellDecoder,
                 C13: CellDecoder,
                 C14: CellDecoder,
                 C15: CellDecoder,
                 C16: CellDecoder](
      f: (C1, C2, C3, C4, C5, C6, C7, C8, C9, C10, C11, C12, C13, C14, C15, C16) => T): RowDecoder[T] =
    instance(row =>
      (row.asAt[C1](0),
       row.asAt[C2](1),
       row.asAt[C3](2),
       row.asAt[C4](3),
       row.asAt[C5](4),
       row.asAt[C6](5),
       row.asAt[C7](6),
       row.asAt[C8](7),
       row.asAt[C9](8),
       row.asAt[C10](9),
       row.asAt[C11](10),
       row.asAt[C12](11),
       row.asAt[C13](12),
       row.asAt[C14](13),
       row.asAt[C15](14),
       row.asAt[C16](15)).mapN(f))

  def forColumns[T,
                 C1: CellDecoder,
                 C2: CellDecoder,
                 C3: CellDecoder,
                 C4: CellDecoder,
                 C5: CellDecoder,
                 C6: CellDecoder,
                 C7: CellDecoder,
                 C8: CellDecoder,
                 C9: CellDecoder,
                 C10: CellDecoder,
                 C11: CellDecoder,
                 C12: CellDecoder,
                 C13: CellDecoder,
                 C14: CellDecoder,
                 C15: CellDecoder,
                 C16: CellDecoder,
                 C17: CellDecoder](
      f: (C1, C2, C3, C4, C5, C6, C7, C8, C9, C10, C11, C12, C13, C14, C15, C16, C17) => T): RowDecoder[T] =
    instance(row =>
      (row.asAt[C1](0),
       row.asAt[C2](1),
       row.asAt[C3](2),
       row.asAt[C4](3),
       row.asAt[C5](4),
       row.asAt[C6](5),
       row.asAt[C7](6),
       row.asAt[C8](7),
       row.asAt[C9](8),
       row.asAt[C10](9),
       row.asAt[C11](10),
       row.asAt[C12](11),
       row.asAt[C13](12),
       row.asAt[C14](13),
       row.asAt[C15](14),
       row.asAt[C16](15),
       row.asAt[C17](16)).mapN(f))

  def forColumns[T,
                 C1: CellDecoder,
                 C2: CellDecoder,
                 C3: CellDecoder,
                 C4: CellDecoder,
                 C5: CellDecoder,
                 C6: CellDecoder,
                 C7: CellDecoder,
                 C8: CellDecoder,
                 C9: CellDecoder,
                 C10: CellDecoder,
                 C11: CellDecoder,
                 C12: CellDecoder,
                 C13: CellDecoder,
                 C14: CellDecoder,
                 C15: CellDecoder,
                 C16: CellDecoder,
                 C17: CellDecoder,
                 C18: CellDecoder](
      f: (C1, C2, C3, C4, C5, C6, C7, C8, C9, C10, C11, C12, C13, C14, C15, C16, C17, C18) => T): RowDecoder[T] =
    instance(row =>
      (row.asAt[C1](0),
       row.asAt[C2](1),
       row.asAt[C3](2),
       row.asAt[C4](3),
       row.asAt[C5](4),
       row.asAt[C6](5),
       row.asAt[C7](6),
       row.asAt[C8](7),
       row.asAt[C9](8),
       row.asAt[C10](9),
       row.asAt[C11](10),
       row.asAt[C12](11),
       row.asAt[C13](12),
       row.asAt[C14](13),
       row.asAt[C15](14),
       row.asAt[C16](15),
       row.asAt[C17](16),
       row.asAt[C18](17)).mapN(f))

  def forColumns[T,
                 C1: CellDecoder,
                 C2: CellDecoder,
                 C3: CellDecoder,
                 C4: CellDecoder,
                 C5: CellDecoder,
                 C6: CellDecoder,
                 C7: CellDecoder,
                 C8: CellDecoder,
                 C9: CellDecoder,
                 C10: CellDecoder,
                 C11: CellDecoder,
                 C12: CellDecoder,
                 C13: CellDecoder,
                 C14: CellDecoder,
                 C15: CellDecoder,
                 C16: CellDecoder,
                 C17: CellDecoder,
                 C18: CellDecoder,
                 C19: CellDecoder](
      f: (C1, C2, C3, C4, C5, C6, C7, C8, C9, C10, C11, C12, C13, C14, C15, C16, C17, C18, C19) => T): RowDecoder[T] =
    instance(row =>
      (row.asAt[C1](0),
       row.asAt[C2](1),
       row.asAt[C3](2),
       row.asAt[C4](3),
       row.asAt[C5](4),
       row.asAt[C6](5),
       row.asAt[C7](6),
       row.asAt[C8](7),
       row.asAt[C9](8),
       row.asAt[C10](9),
       row.asAt[C11](10),
       row.asAt[C12](11),
       row.asAt[C13](12),
       row.asAt[C14](13),
       row.asAt[C15](14),
       row.asAt[C16](15),
       row.asAt[C17](16),
       row.asAt[C18](17),
       row.asAt[C19](18)).mapN(f))

  def forColumns[T,
                 C1: CellDecoder,
                 C2: CellDecoder,
                 C3: CellDecoder,
                 C4: CellDecoder,
                 C5: CellDecoder,
                 C6: CellDecoder,
                 C7: CellDecoder,
                 C8: CellDecoder,
                 C9: CellDecoder,
                 C10: CellDecoder,
                 C11: CellDecoder,
                 C12: CellDecoder,
                 C13: CellDecoder,
                 C14: CellDecoder,
                 C15: CellDecoder,
                 C16: CellDecoder,
                 C17: CellDecoder,
                 C18: CellDecoder,
                 C19: CellDecoder,
                 C20: CellDecoder](
      f: (C1, C2, C3, C4, C5, C6, C7, C8, C9, C10, C11, C12, C13, C14, C15, C16, C17, C18, C19, C20) => T)
      : RowDecoder[T] =
    instance(row =>
      (row.asAt[C1](0),
       row.asAt[C2](1),
       row.asAt[C3](2),
       row.asAt[C4](3),
       row.asAt[C5](4),
       row.asAt[C6](5),
       row.asAt[C7](6),
       row.asAt[C8](7),
       row.asAt[C9](8),
       row.asAt[C10](9),
       row.asAt[C11](10),
       row.asAt[C12](11),
       row.asAt[C13](12),
       row.asAt[C14](13),
       row.asAt[C15](14),
       row.asAt[C16](15),
       row.asAt[C17](16),
       row.asAt[C18](17),
       row.asAt[C19](18),
       row.asAt[C20](19)).mapN(f))

  def forColumns[T,
                 C1: CellDecoder,
                 C2: CellDecoder,
                 C3: CellDecoder,
                 C4: CellDecoder,
                 C5: CellDecoder,
                 C6: CellDecoder,
                 C7: CellDecoder,
                 C8: CellDecoder,
                 C9: CellDecoder,
                 C10: CellDecoder,
                 C11: CellDecoder,
                 C12: CellDecoder,
                 C13: CellDecoder,
                 C14: CellDecoder,
                 C15: CellDecoder,
                 C16: CellDecoder,
                 C17: CellDecoder,
                 C18: CellDecoder,
                 C19: CellDecoder,
                 C20: CellDecoder,
                 C21: CellDecoder](
      f: (C1, C2, C3, C4, C5, C6, C7, C8, C9, C10, C11, C12, C13, C14, C15, C16, C17, C18, C19, C20, C21) => T)
      : RowDecoder[T] =
    instance(row =>
      (row.asAt[C1](0),
       row.asAt[C2](1),
       row.asAt[C3](2),
       row.asAt[C4](3),
       row.asAt[C5](4),
       row.asAt[C6](5),
       row.asAt[C7](6),
       row.asAt[C8](7),
       row.asAt[C9](8),
       row.asAt[C10](9),
       row.asAt[C11](10),
       row.asAt[C12](11),
       row.asAt[C13](12),
       row.asAt[C14](13),
       row.asAt[C15](14),
       row.asAt[C16](15),
       row.asAt[C17](16),
       row.asAt[C18](17),
       row.asAt[C19](18),
       row.asAt[C20](19),
       row.asAt[C21](20)).mapN(f))

  def forColumns[T,
                 C1: CellDecoder,
                 C2: CellDecoder,
                 C3: CellDecoder,
                 C4: CellDecoder,
                 C5: CellDecoder,
                 C6: CellDecoder,
                 C7: CellDecoder,
                 C8: CellDecoder,
                 C9: CellDecoder,
                 C10: CellDecoder,
                 C11: CellDecoder,
                 C12: CellDecoder,
                 C13: CellDecoder,
                 C14: CellDecoder,
                 C15: CellDecoder,
                 C16: CellDecoder,
                 C17: CellDecoder,
                 C18: CellDecoder,
                 C19: CellDecoder,
                 C20: CellDecoder,
                 C21: CellDecoder,
                 C22: CellDecoder](
      f: (C1, C2, C3, C4, C5, C6, C7, C8, C9, C10, C11, C12, C13, C14, C15, C16, C17, C18, C19, C20, C21, C22) => T)
      : RowDecoder[T] =
    instance(row =>
      (row.asAt[C1](0),
       row.asAt[C2](1),
       row.asAt[C3](2),
       row.asAt[C4](3),
       row.asAt[C5](4),
       row.asAt[C6](5),
       row.asAt[C7](6),
       row.asAt[C8](7),
       row.asAt[C9](8),
       row.asAt[C10](9),
       row.asAt[C11](10),
       row.asAt[C12](11),
       row.asAt[C13](12),
       row.asAt[C14](13),
       row.asAt[C15](14),
       row.asAt[C16](15),
       row.asAt[C17](16),
       row.asAt[C18](17),
       row.asAt[C19](18),
       row.asAt[C20](19),
       row.asAt[C21](20),
       row.asAt[C22](21)).mapN(f))
}
