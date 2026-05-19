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

object RowEncoder {

  @inline
  def apply[T: RowEncoder]: RowEncoder[T] = implicitly[RowEncoder[T]]

  @inline
  def instance[T](f: T => NonEmptyList[String]): RowEncoder[T] = (t: T) => Row(f(t))

  def forColumns[T, C1: CellEncoder](f: T => C1): RowEncoder[T] =
    instance(t => NonEmptyList.one(CellEncoder[C1].apply(f(t))))

  def forColumns[T, C1: CellEncoder, C2: CellEncoder](f: T => (C1, C2)): RowEncoder[T] =
    instance { t =>
      val (c1, c2) = f(t)
      NonEmptyList.of(CellEncoder[C1].apply(c1), CellEncoder[C2].apply(c2))
    }

  def forColumns[T, C1: CellEncoder, C2: CellEncoder, C3: CellEncoder](f: T => (C1, C2, C3)): RowEncoder[T] =
    instance { t =>
      val (c1, c2, c3) = f(t)
      NonEmptyList.of(CellEncoder[C1].apply(c1), CellEncoder[C2].apply(c2), CellEncoder[C3].apply(c3))
    }

  def forColumns[T, C1: CellEncoder, C2: CellEncoder, C3: CellEncoder, C4: CellEncoder](
      f: T => (C1, C2, C3, C4)): RowEncoder[T] =
    instance { t =>
      val (c1, c2, c3, c4) = f(t)
      NonEmptyList.of(CellEncoder[C1].apply(c1),
                      CellEncoder[C2].apply(c2),
                      CellEncoder[C3].apply(c3),
                      CellEncoder[C4].apply(c4))
    }

  def forColumns[T, C1: CellEncoder, C2: CellEncoder, C3: CellEncoder, C4: CellEncoder, C5: CellEncoder](
      f: T => (C1, C2, C3, C4, C5)): RowEncoder[T] =
    instance { t =>
      val (c1, c2, c3, c4, c5) = f(t)
      NonEmptyList.of(CellEncoder[C1].apply(c1),
                      CellEncoder[C2].apply(c2),
                      CellEncoder[C3].apply(c3),
                      CellEncoder[C4].apply(c4),
                      CellEncoder[C5].apply(c5))
    }

  def forColumns[T,
                 C1: CellEncoder,
                 C2: CellEncoder,
                 C3: CellEncoder,
                 C4: CellEncoder,
                 C5: CellEncoder,
                 C6: CellEncoder](f: T => (C1, C2, C3, C4, C5, C6)): RowEncoder[T] =
    instance { t =>
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
                 C1: CellEncoder,
                 C2: CellEncoder,
                 C3: CellEncoder,
                 C4: CellEncoder,
                 C5: CellEncoder,
                 C6: CellEncoder,
                 C7: CellEncoder](f: T => (C1, C2, C3, C4, C5, C6, C7)): RowEncoder[T] =
    instance { t =>
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
                 C1: CellEncoder,
                 C2: CellEncoder,
                 C3: CellEncoder,
                 C4: CellEncoder,
                 C5: CellEncoder,
                 C6: CellEncoder,
                 C7: CellEncoder,
                 C8: CellEncoder](f: T => (C1, C2, C3, C4, C5, C6, C7, C8)): RowEncoder[T] =
    instance { t =>
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
                 C1: CellEncoder,
                 C2: CellEncoder,
                 C3: CellEncoder,
                 C4: CellEncoder,
                 C5: CellEncoder,
                 C6: CellEncoder,
                 C7: CellEncoder,
                 C8: CellEncoder,
                 C9: CellEncoder](f: T => (C1, C2, C3, C4, C5, C6, C7, C8, C9)): RowEncoder[T] =
    instance { t =>
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
                 C1: CellEncoder,
                 C2: CellEncoder,
                 C3: CellEncoder,
                 C4: CellEncoder,
                 C5: CellEncoder,
                 C6: CellEncoder,
                 C7: CellEncoder,
                 C8: CellEncoder,
                 C9: CellEncoder,
                 C10: CellEncoder](f: T => (C1, C2, C3, C4, C5, C6, C7, C8, C9, C10)): RowEncoder[T] =
    instance { t =>
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
                 C11: CellEncoder](f: T => (C1, C2, C3, C4, C5, C6, C7, C8, C9, C10, C11)): RowEncoder[T] =
    instance { t =>
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
                 C12: CellEncoder](f: T => (C1, C2, C3, C4, C5, C6, C7, C8, C9, C10, C11, C12)): RowEncoder[T] =
    instance { t =>
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
                 C13: CellEncoder](f: T => (C1, C2, C3, C4, C5, C6, C7, C8, C9, C10, C11, C12, C13)): RowEncoder[T] =
    instance { t =>
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
                 C14: CellEncoder](
      f: T => (C1, C2, C3, C4, C5, C6, C7, C8, C9, C10, C11, C12, C13, C14)): RowEncoder[T] =
    instance { t =>
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
                 C15: CellEncoder](
      f: T => (C1, C2, C3, C4, C5, C6, C7, C8, C9, C10, C11, C12, C13, C14, C15)): RowEncoder[T] =
    instance { t =>
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
                 C16: CellEncoder](
      f: T => (C1, C2, C3, C4, C5, C6, C7, C8, C9, C10, C11, C12, C13, C14, C15, C16)): RowEncoder[T] =
    instance { t =>
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
                 C17: CellEncoder](
      f: T => (C1, C2, C3, C4, C5, C6, C7, C8, C9, C10, C11, C12, C13, C14, C15, C16, C17)): RowEncoder[T] =
    instance { t =>
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
                 C18: CellEncoder](
      f: T => (C1, C2, C3, C4, C5, C6, C7, C8, C9, C10, C11, C12, C13, C14, C15, C16, C17, C18)): RowEncoder[T] =
    instance { t =>
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
                 C19: CellEncoder](
      f: T => (C1, C2, C3, C4, C5, C6, C7, C8, C9, C10, C11, C12, C13, C14, C15, C16, C17, C18, C19)): RowEncoder[T] =
    instance { t =>
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
                 C20: CellEncoder](
      f: T => (C1, C2, C3, C4, C5, C6, C7, C8, C9, C10, C11, C12, C13, C14, C15, C16, C17, C18, C19, C20))
      : RowEncoder[T] =
    instance { t =>
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
                 C21: CellEncoder](
      f: T => (C1, C2, C3, C4, C5, C6, C7, C8, C9, C10, C11, C12, C13, C14, C15, C16, C17, C18, C19, C20, C21))
      : RowEncoder[T] =
    instance { t =>
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
                 C22: CellEncoder](
      f: T => (C1, C2, C3, C4, C5, C6, C7, C8, C9, C10, C11, C12, C13, C14, C15, C16, C17, C18, C19, C20, C21, C22))
      : RowEncoder[T] =
    instance { t =>
      val (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19, c20, c21, c22) = f(t)
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
