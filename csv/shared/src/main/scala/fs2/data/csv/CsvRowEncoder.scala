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

import cats.data.NonEmptyList

object CsvRowEncoder {

  @inline
  def apply[T: CsvRowEncoder[*, Header], Header]: CsvRowEncoder[T, Header] = implicitly[CsvRowEncoder[T, Header]]

  @inline
  def instance[T, Header](f: T => NonEmptyList[(Header, String)]): CsvRowEncoder[T, Header] =
    (t: T) => CsvRow.fromNelHeaders(f(t))

  /**
   * Create a CsvRowEncoder from a function that produces a list of values in the same order as the provided headers.
   * This is "unsafe" because it relies on the caller to ensure that the headers match the produced values.
   * Failure to do so will blow up at runtime with an exception (for unequal lengths) or wrong output (for wrong order).
   */
  def unsafeStatic[T, Header](hs: NonEmptyList[Header])(f: T => NonEmptyList[String]): StaticCsvRowEncoder[T, Header] =
    new CsvRowEncoder[T, Header] with StaticHeaders[T, Header] {
      override val headers: NonEmptyList[Header] = hs
      override def apply(t: T): CsvRow[Header] = CsvRow.unsafe(f(t), hs)
    }

  def forColumns[T, H, C1: CellEncoder](h1: H)(f: T => C1): StaticCsvRowEncoder[T, H] =
    unsafeStatic(NonEmptyList.one(h1))(t => NonEmptyList.one(CellEncoder[C1].apply(f(t))))

  def forColumns[T, H, C1: CellEncoder, C2: CellEncoder](h1: H, h2: H)(f: T => (C1, C2)): StaticCsvRowEncoder[T, H] =
    unsafeStatic(NonEmptyList.of(h1, h2)) { t =>
      val (c1, c2) = f(t)
      NonEmptyList.of(CellEncoder[C1].apply(c1), CellEncoder[C2].apply(c2))
    }

  def forColumns[T, H, C1: CellEncoder, C2: CellEncoder, C3: CellEncoder](h1: H, h2: H, h3: H)(
      f: T => (C1, C2, C3)): StaticCsvRowEncoder[T, H] =
    unsafeStatic(NonEmptyList.of(h1, h2, h3)) { t =>
      val (c1, c2, c3) = f(t)
      NonEmptyList.of(CellEncoder[C1].apply(c1), CellEncoder[C2].apply(c2), CellEncoder[C3].apply(c3))
    }

  def forColumns[T, H, C1: CellEncoder, C2: CellEncoder, C3: CellEncoder, C4: CellEncoder](h1: H, h2: H, h3: H, h4: H)(
      f: T => (C1, C2, C3, C4)): StaticCsvRowEncoder[T, H] =
    unsafeStatic(NonEmptyList.of(h1, h2, h3, h4)) { t =>
      val (c1, c2, c3, c4) = f(t)
      NonEmptyList.of(CellEncoder[C1].apply(c1),
                      CellEncoder[C2].apply(c2),
                      CellEncoder[C3].apply(c3),
                      CellEncoder[C4].apply(c4))
    }

  def forColumns[T, H, C1: CellEncoder, C2: CellEncoder, C3: CellEncoder, C4: CellEncoder, C5: CellEncoder](h1: H,
                                                                                                            h2: H,
                                                                                                            h3: H,
                                                                                                            h4: H,
                                                                                                            h5: H)(
      f: T => (C1, C2, C3, C4, C5)): StaticCsvRowEncoder[T, H] =
    unsafeStatic(NonEmptyList.of(h1, h2, h3, h4, h5)) { t =>
      val (c1, c2, c3, c4, c5) = f(t)
      NonEmptyList.of(CellEncoder[C1].apply(c1),
                      CellEncoder[C2].apply(c2),
                      CellEncoder[C3].apply(c3),
                      CellEncoder[C4].apply(c4),
                      CellEncoder[C5].apply(c5))
    }

  def forColumns[T,
                 H,
                 C1: CellEncoder,
                 C2: CellEncoder,
                 C3: CellEncoder,
                 C4: CellEncoder,
                 C5: CellEncoder,
                 C6: CellEncoder](h1: H, h2: H, h3: H, h4: H, h5: H, h6: H)(
      f: T => (C1, C2, C3, C4, C5, C6)): StaticCsvRowEncoder[T, H] =
    unsafeStatic(NonEmptyList.of(h1, h2, h3, h4, h5, h6)) { t =>
      val (c1, c2, c3, c4, c5, c6) = f(t)
      NonEmptyList.of(
        CellEncoder[C1].apply(c1),
        CellEncoder[C2].apply(c2),
        CellEncoder[C3].apply(c3),
        CellEncoder[C4].apply(c4),
        CellEncoder[C5].apply(c5),
        CellEncoder[C6].apply(c6)
      )
    }

  def forColumns[T,
                 H,
                 C1: CellEncoder,
                 C2: CellEncoder,
                 C3: CellEncoder,
                 C4: CellEncoder,
                 C5: CellEncoder,
                 C6: CellEncoder,
                 C7: CellEncoder](h1: H, h2: H, h3: H, h4: H, h5: H, h6: H, h7: H)(
      f: T => (C1, C2, C3, C4, C5, C6, C7)): StaticCsvRowEncoder[T, H] =
    unsafeStatic(NonEmptyList.of(h1, h2, h3, h4, h5, h6, h7)) { t =>
      val (c1, c2, c3, c4, c5, c6, c7) = f(t)
      NonEmptyList.of(
        CellEncoder[C1].apply(c1),
        CellEncoder[C2].apply(c2),
        CellEncoder[C3].apply(c3),
        CellEncoder[C4].apply(c4),
        CellEncoder[C5].apply(c5),
        CellEncoder[C6].apply(c6),
        CellEncoder[C7].apply(c7)
      )
    }

  def forColumns[T,
                 H,
                 C1: CellEncoder,
                 C2: CellEncoder,
                 C3: CellEncoder,
                 C4: CellEncoder,
                 C5: CellEncoder,
                 C6: CellEncoder,
                 C7: CellEncoder,
                 C8: CellEncoder](h1: H, h2: H, h3: H, h4: H, h5: H, h6: H, h7: H, h8: H)(
      f: T => (C1, C2, C3, C4, C5, C6, C7, C8)): StaticCsvRowEncoder[T, H] =
    unsafeStatic(NonEmptyList.of(h1, h2, h3, h4, h5, h6, h7, h8)) { t =>
      val (c1, c2, c3, c4, c5, c6, c7, c8) = f(t)
      NonEmptyList.of(
        CellEncoder[C1].apply(c1),
        CellEncoder[C2].apply(c2),
        CellEncoder[C3].apply(c3),
        CellEncoder[C4].apply(c4),
        CellEncoder[C5].apply(c5),
        CellEncoder[C6].apply(c6),
        CellEncoder[C7].apply(c7),
        CellEncoder[C8].apply(c8)
      )
    }

  def forColumns[T,
                 H,
                 C1: CellEncoder,
                 C2: CellEncoder,
                 C3: CellEncoder,
                 C4: CellEncoder,
                 C5: CellEncoder,
                 C6: CellEncoder,
                 C7: CellEncoder,
                 C8: CellEncoder,
                 C9: CellEncoder](h1: H, h2: H, h3: H, h4: H, h5: H, h6: H, h7: H, h8: H, h9: H)(
      f: T => (C1, C2, C3, C4, C5, C6, C7, C8, C9)): StaticCsvRowEncoder[T, H] =
    unsafeStatic(NonEmptyList.of(h1, h2, h3, h4, h5, h6, h7, h8, h9)) { t =>
      val (c1, c2, c3, c4, c5, c6, c7, c8, c9) = f(t)
      NonEmptyList.of(
        CellEncoder[C1].apply(c1),
        CellEncoder[C2].apply(c2),
        CellEncoder[C3].apply(c3),
        CellEncoder[C4].apply(c4),
        CellEncoder[C5].apply(c5),
        CellEncoder[C6].apply(c6),
        CellEncoder[C7].apply(c7),
        CellEncoder[C8].apply(c8),
        CellEncoder[C9].apply(c9)
      )
    }

  def forColumns[T,
                 H,
                 C1: CellEncoder,
                 C2: CellEncoder,
                 C3: CellEncoder,
                 C4: CellEncoder,
                 C5: CellEncoder,
                 C6: CellEncoder,
                 C7: CellEncoder,
                 C8: CellEncoder,
                 C9: CellEncoder,
                 C10: CellEncoder](h1: H, h2: H, h3: H, h4: H, h5: H, h6: H, h7: H, h8: H, h9: H, h10: H)(
      f: T => (C1, C2, C3, C4, C5, C6, C7, C8, C9, C10)): StaticCsvRowEncoder[T, H] =
    unsafeStatic(NonEmptyList.of(h1, h2, h3, h4, h5, h6, h7, h8, h9, h10)) { t =>
      val (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10) = f(t)
      NonEmptyList.of(
        CellEncoder[C1].apply(c1),
        CellEncoder[C2].apply(c2),
        CellEncoder[C3].apply(c3),
        CellEncoder[C4].apply(c4),
        CellEncoder[C5].apply(c5),
        CellEncoder[C6].apply(c6),
        CellEncoder[C7].apply(c7),
        CellEncoder[C8].apply(c8),
        CellEncoder[C9].apply(c9),
        CellEncoder[C10].apply(c10)
      )
    }

  def forColumns[T,
                 H,
                 C1: CellEncoder,
                 C2: CellEncoder,
                 C3: CellEncoder,
                 C4: CellEncoder,
                 C5: CellEncoder,
                 C6: CellEncoder,
                 C7: CellEncoder,
                 C8: CellEncoder,
                 C9: CellEncoder,
                 C10: CellEncoder,
                 C11: CellEncoder](h1: H, h2: H, h3: H, h4: H, h5: H, h6: H, h7: H, h8: H, h9: H, h10: H, h11: H)(
      f: T => (C1, C2, C3, C4, C5, C6, C7, C8, C9, C10, C11)): StaticCsvRowEncoder[T, H] =
    unsafeStatic(NonEmptyList.of(h1, h2, h3, h4, h5, h6, h7, h8, h9, h10, h11)) { t =>
      val (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11) = f(t)
      NonEmptyList.of(
        CellEncoder[C1].apply(c1),
        CellEncoder[C2].apply(c2),
        CellEncoder[C3].apply(c3),
        CellEncoder[C4].apply(c4),
        CellEncoder[C5].apply(c5),
        CellEncoder[C6].apply(c6),
        CellEncoder[C7].apply(c7),
        CellEncoder[C8].apply(c8),
        CellEncoder[C9].apply(c9),
        CellEncoder[C10].apply(c10),
        CellEncoder[C11].apply(c11)
      )
    }

  def forColumns[T,
                 H,
                 C1: CellEncoder,
                 C2: CellEncoder,
                 C3: CellEncoder,
                 C4: CellEncoder,
                 C5: CellEncoder,
                 C6: CellEncoder,
                 C7: CellEncoder,
                 C8: CellEncoder,
                 C9: CellEncoder,
                 C10: CellEncoder,
                 C11: CellEncoder,
                 C12: CellEncoder](h1: H,
                                   h2: H,
                                   h3: H,
                                   h4: H,
                                   h5: H,
                                   h6: H,
                                   h7: H,
                                   h8: H,
                                   h9: H,
                                   h10: H,
                                   h11: H,
                                   h12: H)(
      f: T => (C1, C2, C3, C4, C5, C6, C7, C8, C9, C10, C11, C12)): StaticCsvRowEncoder[T, H] =
    unsafeStatic(NonEmptyList.of(h1, h2, h3, h4, h5, h6, h7, h8, h9, h10, h11, h12)) { t =>
      val (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12) = f(t)
      NonEmptyList.of(
        CellEncoder[C1].apply(c1),
        CellEncoder[C2].apply(c2),
        CellEncoder[C3].apply(c3),
        CellEncoder[C4].apply(c4),
        CellEncoder[C5].apply(c5),
        CellEncoder[C6].apply(c6),
        CellEncoder[C7].apply(c7),
        CellEncoder[C8].apply(c8),
        CellEncoder[C9].apply(c9),
        CellEncoder[C10].apply(c10),
        CellEncoder[C11].apply(c11),
        CellEncoder[C12].apply(c12)
      )
    }

  def forColumns[T,
                 H,
                 C1: CellEncoder,
                 C2: CellEncoder,
                 C3: CellEncoder,
                 C4: CellEncoder,
                 C5: CellEncoder,
                 C6: CellEncoder,
                 C7: CellEncoder,
                 C8: CellEncoder,
                 C9: CellEncoder,
                 C10: CellEncoder,
                 C11: CellEncoder,
                 C12: CellEncoder,
                 C13: CellEncoder](h1: H,
                                   h2: H,
                                   h3: H,
                                   h4: H,
                                   h5: H,
                                   h6: H,
                                   h7: H,
                                   h8: H,
                                   h9: H,
                                   h10: H,
                                   h11: H,
                                   h12: H,
                                   h13: H)(
      f: T => (C1, C2, C3, C4, C5, C6, C7, C8, C9, C10, C11, C12, C13)): StaticCsvRowEncoder[T, H] =
    unsafeStatic(NonEmptyList.of(h1, h2, h3, h4, h5, h6, h7, h8, h9, h10, h11, h12, h13)) { t =>
      val (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13) = f(t)
      NonEmptyList.of(
        CellEncoder[C1].apply(c1),
        CellEncoder[C2].apply(c2),
        CellEncoder[C3].apply(c3),
        CellEncoder[C4].apply(c4),
        CellEncoder[C5].apply(c5),
        CellEncoder[C6].apply(c6),
        CellEncoder[C7].apply(c7),
        CellEncoder[C8].apply(c8),
        CellEncoder[C9].apply(c9),
        CellEncoder[C10].apply(c10),
        CellEncoder[C11].apply(c11),
        CellEncoder[C12].apply(c12),
        CellEncoder[C13].apply(c13)
      )
    }

  def forColumns[T,
                 H,
                 C1: CellEncoder,
                 C2: CellEncoder,
                 C3: CellEncoder,
                 C4: CellEncoder,
                 C5: CellEncoder,
                 C6: CellEncoder,
                 C7: CellEncoder,
                 C8: CellEncoder,
                 C9: CellEncoder,
                 C10: CellEncoder,
                 C11: CellEncoder,
                 C12: CellEncoder,
                 C13: CellEncoder,
                 C14: CellEncoder](h1: H,
                                   h2: H,
                                   h3: H,
                                   h4: H,
                                   h5: H,
                                   h6: H,
                                   h7: H,
                                   h8: H,
                                   h9: H,
                                   h10: H,
                                   h11: H,
                                   h12: H,
                                   h13: H,
                                   h14: H)(
      f: T => (C1, C2, C3, C4, C5, C6, C7, C8, C9, C10, C11, C12, C13, C14)): StaticCsvRowEncoder[T, H] =
    unsafeStatic(NonEmptyList.of(h1, h2, h3, h4, h5, h6, h7, h8, h9, h10, h11, h12, h13, h14)) { t =>
      val (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14) = f(t)
      NonEmptyList.of(
        CellEncoder[C1].apply(c1),
        CellEncoder[C2].apply(c2),
        CellEncoder[C3].apply(c3),
        CellEncoder[C4].apply(c4),
        CellEncoder[C5].apply(c5),
        CellEncoder[C6].apply(c6),
        CellEncoder[C7].apply(c7),
        CellEncoder[C8].apply(c8),
        CellEncoder[C9].apply(c9),
        CellEncoder[C10].apply(c10),
        CellEncoder[C11].apply(c11),
        CellEncoder[C12].apply(c12),
        CellEncoder[C13].apply(c13),
        CellEncoder[C14].apply(c14)
      )
    }

  def forColumns[T,
                 H,
                 C1: CellEncoder,
                 C2: CellEncoder,
                 C3: CellEncoder,
                 C4: CellEncoder,
                 C5: CellEncoder,
                 C6: CellEncoder,
                 C7: CellEncoder,
                 C8: CellEncoder,
                 C9: CellEncoder,
                 C10: CellEncoder,
                 C11: CellEncoder,
                 C12: CellEncoder,
                 C13: CellEncoder,
                 C14: CellEncoder,
                 C15: CellEncoder](h1: H,
                                   h2: H,
                                   h3: H,
                                   h4: H,
                                   h5: H,
                                   h6: H,
                                   h7: H,
                                   h8: H,
                                   h9: H,
                                   h10: H,
                                   h11: H,
                                   h12: H,
                                   h13: H,
                                   h14: H,
                                   h15: H)(
      f: T => (C1, C2, C3, C4, C5, C6, C7, C8, C9, C10, C11, C12, C13, C14, C15)): StaticCsvRowEncoder[T, H] =
    unsafeStatic(NonEmptyList.of(h1, h2, h3, h4, h5, h6, h7, h8, h9, h10, h11, h12, h13, h14, h15)) { t =>
      val (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15) = f(t)
      NonEmptyList.of(
        CellEncoder[C1].apply(c1),
        CellEncoder[C2].apply(c2),
        CellEncoder[C3].apply(c3),
        CellEncoder[C4].apply(c4),
        CellEncoder[C5].apply(c5),
        CellEncoder[C6].apply(c6),
        CellEncoder[C7].apply(c7),
        CellEncoder[C8].apply(c8),
        CellEncoder[C9].apply(c9),
        CellEncoder[C10].apply(c10),
        CellEncoder[C11].apply(c11),
        CellEncoder[C12].apply(c12),
        CellEncoder[C13].apply(c13),
        CellEncoder[C14].apply(c14),
        CellEncoder[C15].apply(c15)
      )
    }

  def forColumns[T,
                 H,
                 C1: CellEncoder,
                 C2: CellEncoder,
                 C3: CellEncoder,
                 C4: CellEncoder,
                 C5: CellEncoder,
                 C6: CellEncoder,
                 C7: CellEncoder,
                 C8: CellEncoder,
                 C9: CellEncoder,
                 C10: CellEncoder,
                 C11: CellEncoder,
                 C12: CellEncoder,
                 C13: CellEncoder,
                 C14: CellEncoder,
                 C15: CellEncoder,
                 C16: CellEncoder](h1: H,
                                   h2: H,
                                   h3: H,
                                   h4: H,
                                   h5: H,
                                   h6: H,
                                   h7: H,
                                   h8: H,
                                   h9: H,
                                   h10: H,
                                   h11: H,
                                   h12: H,
                                   h13: H,
                                   h14: H,
                                   h15: H,
                                   h16: H)(
      f: T => (C1, C2, C3, C4, C5, C6, C7, C8, C9, C10, C11, C12, C13, C14, C15, C16)): StaticCsvRowEncoder[T, H] =
    unsafeStatic(NonEmptyList.of(h1, h2, h3, h4, h5, h6, h7, h8, h9, h10, h11, h12, h13, h14, h15, h16)) { t =>
      val (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16) = f(t)
      NonEmptyList.of(
        CellEncoder[C1].apply(c1),
        CellEncoder[C2].apply(c2),
        CellEncoder[C3].apply(c3),
        CellEncoder[C4].apply(c4),
        CellEncoder[C5].apply(c5),
        CellEncoder[C6].apply(c6),
        CellEncoder[C7].apply(c7),
        CellEncoder[C8].apply(c8),
        CellEncoder[C9].apply(c9),
        CellEncoder[C10].apply(c10),
        CellEncoder[C11].apply(c11),
        CellEncoder[C12].apply(c12),
        CellEncoder[C13].apply(c13),
        CellEncoder[C14].apply(c14),
        CellEncoder[C15].apply(c15),
        CellEncoder[C16].apply(c16)
      )
    }

  def forColumns[T,
                 H,
                 C1: CellEncoder,
                 C2: CellEncoder,
                 C3: CellEncoder,
                 C4: CellEncoder,
                 C5: CellEncoder,
                 C6: CellEncoder,
                 C7: CellEncoder,
                 C8: CellEncoder,
                 C9: CellEncoder,
                 C10: CellEncoder,
                 C11: CellEncoder,
                 C12: CellEncoder,
                 C13: CellEncoder,
                 C14: CellEncoder,
                 C15: CellEncoder,
                 C16: CellEncoder,
                 C17: CellEncoder](h1: H,
                                   h2: H,
                                   h3: H,
                                   h4: H,
                                   h5: H,
                                   h6: H,
                                   h7: H,
                                   h8: H,
                                   h9: H,
                                   h10: H,
                                   h11: H,
                                   h12: H,
                                   h13: H,
                                   h14: H,
                                   h15: H,
                                   h16: H,
                                   h17: H)(
      f: T => (C1, C2, C3, C4, C5, C6, C7, C8, C9, C10, C11, C12, C13, C14, C15, C16, C17)): StaticCsvRowEncoder[T, H] =
    unsafeStatic(NonEmptyList.of(h1, h2, h3, h4, h5, h6, h7, h8, h9, h10, h11, h12, h13, h14, h15, h16, h17)) { t =>
      val (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17) = f(t)
      NonEmptyList.of(
        CellEncoder[C1].apply(c1),
        CellEncoder[C2].apply(c2),
        CellEncoder[C3].apply(c3),
        CellEncoder[C4].apply(c4),
        CellEncoder[C5].apply(c5),
        CellEncoder[C6].apply(c6),
        CellEncoder[C7].apply(c7),
        CellEncoder[C8].apply(c8),
        CellEncoder[C9].apply(c9),
        CellEncoder[C10].apply(c10),
        CellEncoder[C11].apply(c11),
        CellEncoder[C12].apply(c12),
        CellEncoder[C13].apply(c13),
        CellEncoder[C14].apply(c14),
        CellEncoder[C15].apply(c15),
        CellEncoder[C16].apply(c16),
        CellEncoder[C17].apply(c17)
      )
    }

  def forColumns[T,
                 H,
                 C1: CellEncoder,
                 C2: CellEncoder,
                 C3: CellEncoder,
                 C4: CellEncoder,
                 C5: CellEncoder,
                 C6: CellEncoder,
                 C7: CellEncoder,
                 C8: CellEncoder,
                 C9: CellEncoder,
                 C10: CellEncoder,
                 C11: CellEncoder,
                 C12: CellEncoder,
                 C13: CellEncoder,
                 C14: CellEncoder,
                 C15: CellEncoder,
                 C16: CellEncoder,
                 C17: CellEncoder,
                 C18: CellEncoder](h1: H,
                                   h2: H,
                                   h3: H,
                                   h4: H,
                                   h5: H,
                                   h6: H,
                                   h7: H,
                                   h8: H,
                                   h9: H,
                                   h10: H,
                                   h11: H,
                                   h12: H,
                                   h13: H,
                                   h14: H,
                                   h15: H,
                                   h16: H,
                                   h17: H,
                                   h18: H)(
      f: T => (C1, C2, C3, C4, C5, C6, C7, C8, C9, C10, C11, C12, C13, C14, C15, C16, C17, C18))
      : StaticCsvRowEncoder[T, H] =
    unsafeStatic(NonEmptyList.of(h1, h2, h3, h4, h5, h6, h7, h8, h9, h10, h11, h12, h13, h14, h15, h16, h17, h18)) {
      t =>
        val (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18) = f(t)
        NonEmptyList.of(
          CellEncoder[C1].apply(c1),
          CellEncoder[C2].apply(c2),
          CellEncoder[C3].apply(c3),
          CellEncoder[C4].apply(c4),
          CellEncoder[C5].apply(c5),
          CellEncoder[C6].apply(c6),
          CellEncoder[C7].apply(c7),
          CellEncoder[C8].apply(c8),
          CellEncoder[C9].apply(c9),
          CellEncoder[C10].apply(c10),
          CellEncoder[C11].apply(c11),
          CellEncoder[C12].apply(c12),
          CellEncoder[C13].apply(c13),
          CellEncoder[C14].apply(c14),
          CellEncoder[C15].apply(c15),
          CellEncoder[C16].apply(c16),
          CellEncoder[C17].apply(c17),
          CellEncoder[C18].apply(c18)
        )
    }

  def forColumns[T,
                 H,
                 C1: CellEncoder,
                 C2: CellEncoder,
                 C3: CellEncoder,
                 C4: CellEncoder,
                 C5: CellEncoder,
                 C6: CellEncoder,
                 C7: CellEncoder,
                 C8: CellEncoder,
                 C9: CellEncoder,
                 C10: CellEncoder,
                 C11: CellEncoder,
                 C12: CellEncoder,
                 C13: CellEncoder,
                 C14: CellEncoder,
                 C15: CellEncoder,
                 C16: CellEncoder,
                 C17: CellEncoder,
                 C18: CellEncoder,
                 C19: CellEncoder](h1: H,
                                   h2: H,
                                   h3: H,
                                   h4: H,
                                   h5: H,
                                   h6: H,
                                   h7: H,
                                   h8: H,
                                   h9: H,
                                   h10: H,
                                   h11: H,
                                   h12: H,
                                   h13: H,
                                   h14: H,
                                   h15: H,
                                   h16: H,
                                   h17: H,
                                   h18: H,
                                   h19: H)(
      f: T => (C1, C2, C3, C4, C5, C6, C7, C8, C9, C10, C11, C12, C13, C14, C15, C16, C17, C18, C19))
      : StaticCsvRowEncoder[T, H] =
    unsafeStatic(
      NonEmptyList.of(h1, h2, h3, h4, h5, h6, h7, h8, h9, h10, h11, h12, h13, h14, h15, h16, h17, h18, h19)) { t =>
      val (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19) = f(t)
      NonEmptyList.of(
        CellEncoder[C1].apply(c1),
        CellEncoder[C2].apply(c2),
        CellEncoder[C3].apply(c3),
        CellEncoder[C4].apply(c4),
        CellEncoder[C5].apply(c5),
        CellEncoder[C6].apply(c6),
        CellEncoder[C7].apply(c7),
        CellEncoder[C8].apply(c8),
        CellEncoder[C9].apply(c9),
        CellEncoder[C10].apply(c10),
        CellEncoder[C11].apply(c11),
        CellEncoder[C12].apply(c12),
        CellEncoder[C13].apply(c13),
        CellEncoder[C14].apply(c14),
        CellEncoder[C15].apply(c15),
        CellEncoder[C16].apply(c16),
        CellEncoder[C17].apply(c17),
        CellEncoder[C18].apply(c18),
        CellEncoder[C19].apply(c19)
      )
    }

  def forColumns[T,
                 H,
                 C1: CellEncoder,
                 C2: CellEncoder,
                 C3: CellEncoder,
                 C4: CellEncoder,
                 C5: CellEncoder,
                 C6: CellEncoder,
                 C7: CellEncoder,
                 C8: CellEncoder,
                 C9: CellEncoder,
                 C10: CellEncoder,
                 C11: CellEncoder,
                 C12: CellEncoder,
                 C13: CellEncoder,
                 C14: CellEncoder,
                 C15: CellEncoder,
                 C16: CellEncoder,
                 C17: CellEncoder,
                 C18: CellEncoder,
                 C19: CellEncoder,
                 C20: CellEncoder](h1: H,
                                   h2: H,
                                   h3: H,
                                   h4: H,
                                   h5: H,
                                   h6: H,
                                   h7: H,
                                   h8: H,
                                   h9: H,
                                   h10: H,
                                   h11: H,
                                   h12: H,
                                   h13: H,
                                   h14: H,
                                   h15: H,
                                   h16: H,
                                   h17: H,
                                   h18: H,
                                   h19: H,
                                   h20: H)(
      f: T => (C1, C2, C3, C4, C5, C6, C7, C8, C9, C10, C11, C12, C13, C14, C15, C16, C17, C18, C19, C20))
      : StaticCsvRowEncoder[T, H] =
    unsafeStatic(
      NonEmptyList.of(h1, h2, h3, h4, h5, h6, h7, h8, h9, h10, h11, h12, h13, h14, h15, h16, h17, h18, h19, h20)) { t =>
      val (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19, c20) = f(t)
      NonEmptyList.of(
        CellEncoder[C1].apply(c1),
        CellEncoder[C2].apply(c2),
        CellEncoder[C3].apply(c3),
        CellEncoder[C4].apply(c4),
        CellEncoder[C5].apply(c5),
        CellEncoder[C6].apply(c6),
        CellEncoder[C7].apply(c7),
        CellEncoder[C8].apply(c8),
        CellEncoder[C9].apply(c9),
        CellEncoder[C10].apply(c10),
        CellEncoder[C11].apply(c11),
        CellEncoder[C12].apply(c12),
        CellEncoder[C13].apply(c13),
        CellEncoder[C14].apply(c14),
        CellEncoder[C15].apply(c15),
        CellEncoder[C16].apply(c16),
        CellEncoder[C17].apply(c17),
        CellEncoder[C18].apply(c18),
        CellEncoder[C19].apply(c19),
        CellEncoder[C20].apply(c20)
      )
    }

  def forColumns[T,
                 H,
                 C1: CellEncoder,
                 C2: CellEncoder,
                 C3: CellEncoder,
                 C4: CellEncoder,
                 C5: CellEncoder,
                 C6: CellEncoder,
                 C7: CellEncoder,
                 C8: CellEncoder,
                 C9: CellEncoder,
                 C10: CellEncoder,
                 C11: CellEncoder,
                 C12: CellEncoder,
                 C13: CellEncoder,
                 C14: CellEncoder,
                 C15: CellEncoder,
                 C16: CellEncoder,
                 C17: CellEncoder,
                 C18: CellEncoder,
                 C19: CellEncoder,
                 C20: CellEncoder,
                 C21: CellEncoder](h1: H,
                                   h2: H,
                                   h3: H,
                                   h4: H,
                                   h5: H,
                                   h6: H,
                                   h7: H,
                                   h8: H,
                                   h9: H,
                                   h10: H,
                                   h11: H,
                                   h12: H,
                                   h13: H,
                                   h14: H,
                                   h15: H,
                                   h16: H,
                                   h17: H,
                                   h18: H,
                                   h19: H,
                                   h20: H,
                                   h21: H)(
      f: T => (C1, C2, C3, C4, C5, C6, C7, C8, C9, C10, C11, C12, C13, C14, C15, C16, C17, C18, C19, C20, C21))
      : StaticCsvRowEncoder[T, H] =
    unsafeStatic(
      NonEmptyList.of(h1, h2, h3, h4, h5, h6, h7, h8, h9, h10, h11, h12, h13, h14, h15, h16, h17, h18, h19, h20, h21)) {
      t =>
        val (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19, c20, c21) = f(t)
        NonEmptyList.of(
          CellEncoder[C1].apply(c1),
          CellEncoder[C2].apply(c2),
          CellEncoder[C3].apply(c3),
          CellEncoder[C4].apply(c4),
          CellEncoder[C5].apply(c5),
          CellEncoder[C6].apply(c6),
          CellEncoder[C7].apply(c7),
          CellEncoder[C8].apply(c8),
          CellEncoder[C9].apply(c9),
          CellEncoder[C10].apply(c10),
          CellEncoder[C11].apply(c11),
          CellEncoder[C12].apply(c12),
          CellEncoder[C13].apply(c13),
          CellEncoder[C14].apply(c14),
          CellEncoder[C15].apply(c15),
          CellEncoder[C16].apply(c16),
          CellEncoder[C17].apply(c17),
          CellEncoder[C18].apply(c18),
          CellEncoder[C19].apply(c19),
          CellEncoder[C20].apply(c20),
          CellEncoder[C21].apply(c21)
        )
    }

  def forColumns[T,
                 H,
                 C1: CellEncoder,
                 C2: CellEncoder,
                 C3: CellEncoder,
                 C4: CellEncoder,
                 C5: CellEncoder,
                 C6: CellEncoder,
                 C7: CellEncoder,
                 C8: CellEncoder,
                 C9: CellEncoder,
                 C10: CellEncoder,
                 C11: CellEncoder,
                 C12: CellEncoder,
                 C13: CellEncoder,
                 C14: CellEncoder,
                 C15: CellEncoder,
                 C16: CellEncoder,
                 C17: CellEncoder,
                 C18: CellEncoder,
                 C19: CellEncoder,
                 C20: CellEncoder,
                 C21: CellEncoder,
                 C22: CellEncoder](h1: H,
                                   h2: H,
                                   h3: H,
                                   h4: H,
                                   h5: H,
                                   h6: H,
                                   h7: H,
                                   h8: H,
                                   h9: H,
                                   h10: H,
                                   h11: H,
                                   h12: H,
                                   h13: H,
                                   h14: H,
                                   h15: H,
                                   h16: H,
                                   h17: H,
                                   h18: H,
                                   h19: H,
                                   h20: H,
                                   h21: H,
                                   h22: H)(
      f: T => (C1, C2, C3, C4, C5, C6, C7, C8, C9, C10, C11, C12, C13, C14, C15, C16, C17, C18, C19, C20, C21, C22))
      : StaticCsvRowEncoder[T, H] =
    unsafeStatic(
      NonEmptyList
        .of(h1, h2, h3, h4, h5, h6, h7, h8, h9, h10, h11, h12, h13, h14, h15, h16, h17, h18, h19, h20, h21, h22)) { t =>
      val (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19, c20, c21, c22) =
        f(t)
      NonEmptyList.of(
        CellEncoder[C1].apply(c1),
        CellEncoder[C2].apply(c2),
        CellEncoder[C3].apply(c3),
        CellEncoder[C4].apply(c4),
        CellEncoder[C5].apply(c5),
        CellEncoder[C6].apply(c6),
        CellEncoder[C7].apply(c7),
        CellEncoder[C8].apply(c8),
        CellEncoder[C9].apply(c9),
        CellEncoder[C10].apply(c10),
        CellEncoder[C11].apply(c11),
        CellEncoder[C12].apply(c12),
        CellEncoder[C13].apply(c13),
        CellEncoder[C14].apply(c14),
        CellEncoder[C15].apply(c15),
        CellEncoder[C16].apply(c16),
        CellEncoder[C17].apply(c17),
        CellEncoder[C18].apply(c18),
        CellEncoder[C19].apply(c19),
        CellEncoder[C20].apply(c20),
        CellEncoder[C21].apply(c21),
        CellEncoder[C22].apply(c22)
      )
    }
}
