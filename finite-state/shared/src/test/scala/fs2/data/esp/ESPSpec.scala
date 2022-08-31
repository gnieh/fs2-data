/*
 * Copyright 2019-2022 Lucas Satabin
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
package mft

import pattern.{DecisionTree, Selectable}
import esp.{ESP, Tag, Rhs => ERhs}

import weaver._
import cats.effect.IO
import cats._
import cats.effect.Resource
import fs2.data.pattern.ConstructorTree

sealed trait Event
object Event {
  case class Open(name: String) extends Event
  case class Close(name: String) extends Event
  case class Text(t: String) extends Event

  implicit val eq: Eq[Event] = Eq.fromUniversalEquals

  implicit object conv extends Conversion[String, Event] {

    override def makeName(t: String): String = t

    override def makeOpenOut(t: String): Event = Open(t)

    override def makeCloseOut(t: String): Event = Close(t)

  }

  implicit object selectable extends Selectable[Event, Tag[Event]] {

    override def tree(e: Event): ConstructorTree[Tag[Event]] =
      e match {
        case Open(name)  => ConstructorTree(Tag.Open, List(ConstructorTree(Tag.Name(name), Nil)))
        case Close(name) => ConstructorTree(Tag.Close, List(ConstructorTree(Tag.Name(name), Nil)))
        case Text(t)     => ConstructorTree(Tag.Value(Text(t)), Nil)
      }

  }

}

object ESPSpec extends IOSuite {

  val Main = 0
  val Rev = 1

  val mft: MFT[String, Event, Event] =
    new MFT(
      Main,
      Map(
        Main -> Rules(
          Nil,
          List(
            EventSelector.Node("rev") -> Rhs.Concat(Rhs.Node("rev", Rhs.Call(Rev, Forest.Children, List(Rhs.Epsilon))),
                                                    Rhs.Call(Main, Forest.Siblings, Nil)),
            EventSelector.Node("a") -> Rhs.Concat(Rhs.Node("a", Rhs.Call(Main, Forest.Children, Nil)),
                                                  Rhs.Call(Main, Forest.Siblings, Nil)),
            EventSelector.Node("b") -> Rhs.Concat(Rhs.Node("b", Rhs.Call(Main, Forest.Children, Nil)),
                                                  Rhs.Call(Main, Forest.Siblings, Nil)),
            EventSelector.Node("c") -> Rhs.Concat(Rhs.Node("c", Rhs.Call(Main, Forest.Children, Nil)),
                                                  Rhs.Call(Main, Forest.Siblings, Nil)),
            EventSelector.Value(Event.Text("text")) -> Rhs.Concat(Rhs.Leaf(Event.Text("text")),
                                                                  Rhs.Call(Main, Forest.Children, Nil)),
            EventSelector.Epsilon -> Rhs.Epsilon
          )
        ),
        Rev -> Rules(
          List(0),
          List(
            EventSelector.Node("a") -> Rhs.Call(
              Rev,
              Forest.Siblings,
              List(Rhs.Concat(Rhs.Node("a", Rhs.Call(Rev, Forest.Children, List(Rhs.Epsilon))), Rhs.Param(0)))),
            EventSelector.Node("b") -> Rhs.Call(
              Rev,
              Forest.Siblings,
              List(Rhs.Concat(Rhs.Node("b", Rhs.Call(Rev, Forest.Children, List(Rhs.Epsilon))), Rhs.Param(0)))),
            EventSelector.Node("c") -> Rhs.Call(
              Rev,
              Forest.Siblings,
              List(Rhs.Concat(Rhs.Node("c", Rhs.Call(Rev, Forest.Children, List(Rhs.Epsilon))), Rhs.Param(0)))),
            EventSelector.Value(Event.Text("text")) -> Rhs
              .Call(Rev, Forest.Children, List(Rhs.Concat(Rhs.Leaf(Event.Text("text")), Rhs.Param(0)))),
            EventSelector.Epsilon -> Rhs.Param(0)
          )
        )
      )
    )

  type Res = ESP[IO, DecisionTree[Tag[Event], ERhs[Event]], Event, Event]

  override def sharedResource: Resource[IO, Res] = Resource.eval(mft.esp)

  test("reverse tree") { esp =>
    Stream
      .emits(
        List[Event](
          // format: off
          Event.Open("a"),
            Event.Open("rev"),
              Event.Open("b"),
                Event.Open("c"),
                Event.Close("c"),
                Event.Text("text"),
                Event.Open("b"),
                Event.Close("b"),
              Event.Close("b"),
              Event.Open("c"),
                Event.Text("text"),
                Event.Open("b"),
                Event.Close("b"),
              Event.Close("c"),
            Event.Close("rev"),
          Event.Close("a"),
          Event.Open("a"),
          Event.Close("a")
          // format: on
        ))
      .covary[IO]
      .through(esp.pipe)
      .compile
      .toList
      .map { events =>
        expect.eql(
          List[Event](
            // format: off
            Event.Open("a"),
              Event.Open("rev"),
                Event.Open("c"),
                  Event.Open("b"),
                  Event.Close("b"),
                  Event.Text("text"),
                Event.Close("c"),
                Event.Open("b"),
                  Event.Open("b"),
                  Event.Close("b"),
                  Event.Text("text"),
                  Event.Open("c"),
                  Event.Close("c"),
                Event.Close("b"),
              Event.Close("rev"),
            Event.Close("a"),
            Event.Open("a"),
            Event.Close("a")
            // format: on
          ),
          events
        )
      }
  }

}
