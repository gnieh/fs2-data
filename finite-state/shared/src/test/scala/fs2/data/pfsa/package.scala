/*
 * Copyright 2022 Lucas Satabin
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

package fs2.data

import cats.Eq
import org.scalacheck.{Arbitrary, Gen}

package object pfsa {

  implicit def arbRegular[CS: Eq, C](implicit a: Arbitrary[CS], CS: Pred[CS, C]): Arbitrary[Regular[CS]] =
    Arbitrary {
      val genLeaf: Gen[Regular[CS]] = Gen.oneOf(Gen.const(Regular.any),
                                                Gen.const(Regular.epsilon[CS]),
                                                Gen.const(Regular.empty),
                                                a.arbitrary.map(Regular.chars(_)))

      def genInternal(sz: Int): Gen[Regular[CS]] =
        Gen.oneOf(
          for {
            lsz <- Gen.choose(sz - 3, sz - 1)
            l <- sizedRegular(lsz)
            rsz <- Gen.choose(sz / 3, sz / 2)
            r <- sizedRegular(rsz)
            const <- Gen.oneOf[Regular[CS] => Regular[CS]](l && _, l || _, l ~ _)
          } yield const(r),
          for {
            sz <- Gen.choose(sz / 3, sz / 2)
            re <- sizedRegular(sz)
          } yield !re
        )

      def sizedRegular(sz: Int) =
        if (sz <= 0) genLeaf
        else Gen.frequency((1, genLeaf), (3, genInternal(sz)))

      Gen.sized(sz => sizedRegular(sz))
    }
}
