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

package fs2.data.pfsa

import cats.Show
import org.scalacheck.{Arbitrary, Gen}
import weaver._
import weaver.scalacheck._

import Regular._

object RegularSpec extends SimpleIOSuite with Checkers {

  type Regex = Regular[Set[Char]]

  implicit val charSetShow: Show[Set[Char]] = Show.show { set =>
    if (set.size <= 1)
      set.headOption.fold("∅")(_.toString())
    else
      set.toList.sorted.mkString("[", "", "]")

  }

  implicit val arbitraryChar: Arbitrary[Char] = Arbitrary(Gen.oneOf('a', 'b'))

  implicit object CharSetInstances extends Pred[Set[Char], Char] with Candidate[Set[Char], Char] {

    override def satsifies(p: Set[Char])(e: Char): Boolean = p.contains(e)

    override val always: Set[Char] = Set('a', 'b')

    override val never: Set[Char] = Set.empty

    override def and(p1: Set[Char], p2: Set[Char]): Set[Char] = p1.intersect(p2)

    override def or(p1: Set[Char], p2: Set[Char]): Set[Char] = p1.union(p2)

    override def not(p: Set[Char]): Set[Char] = always.diff(p)

    override def isSatisfiable(p: Set[Char]): Boolean = p.nonEmpty

    override def pick(set: Set[Char]): Option[Char] = set.headOption

  }

  test("empty") {
    val dfa = empty.deriveDFA
    forall(Gen.listOf(arbitraryChar.arbitrary)) { input =>
      expect.eql(false, dfa.recognizes(input))
    }
  }

  test("epsilon") {
    val dfa = epsilon.deriveDFA
    forall(Gen.listOf(arbitraryChar.arbitrary)) { input =>
      expect.eql(input.isEmpty, dfa.recognizes(input))
    }
  }

  test("any") {
    val dfa = any.deriveDFA
    forall(Gen.listOf(arbitraryChar.arbitrary)) { input =>
      expect.eql(input.size == 1, dfa.recognizes(input))
    }
  }

  test("any1") {
    val dfa = any.deriveDFA
    forall(Gen.listOfN(1, arbitraryChar.arbitrary)) { input =>
      expect.eql(true, dfa.recognizes(input))
    }
  }

  test("chars") {
    val gen =
      for {
        set <- Gen.listOf(arbitraryChar.arbitrary).map(_.toSet)
        c <- arbitraryChar.arbitrary
      } yield (set, c)
    forall(gen) { case (set, c) =>
      val dfa = chars(set).deriveDFA
      expect.eql(set.contains(c), dfa.recognizes(List(c)))
    }
  }

  pureTest("complex") {
    val dfa =
      ((chars(Set('a', 'b')) ~ chars(Set('a'))) || (chars(Set('b')).rep ~ !epsilon)).deriveDFA
    expect.all(dfa.recognizes("aa".toList),
               dfa.recognizes("ba".toList),
               dfa.recognizes("bbb".toList),
               dfa.recognizes("a".toList),
               !dfa.recognizes("".toList))
  }

}
