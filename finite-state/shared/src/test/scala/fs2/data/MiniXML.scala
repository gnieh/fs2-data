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

package fs2.data

import cats.Eq
import fs2.data.esp.{Conversion, Tag}
import fs2.data.pattern.{ConstructorTree, Selectable}
import cats.Show

sealed trait MiniXML
object MiniXML {
  case class Open(name: String) extends MiniXML
  case class Close(name: String) extends MiniXML
  case class Text(txt: String) extends MiniXML

  def text(txt: String): MiniXML = Text(txt)

  def open(name: String): MiniXML = Open(name)

  def close(name: String): MiniXML = Close(name)

  implicit val eq: Eq[MiniXML] = Eq.fromUniversalEquals

  implicit object MiniXMLSelectable extends Selectable[MiniXML, Tag[String]] {

    override def tree(e: MiniXML): ConstructorTree[Tag[String]] =
      e match {
        case Open(name)  => ConstructorTree(Tag.Open, List(ConstructorTree(Tag.Name(name), Nil)))
        case Close(name) => ConstructorTree(Tag.Close, List(ConstructorTree(Tag.Name(name), Nil)))
        case Text(t)     => ConstructorTree(Tag.Leaf, List(ConstructorTree(Tag.Value(t), Nil)))
      }

  }

  implicit object MiniXMLConversion extends Conversion[String, MiniXML] {

    override def makeOpen(t: String): MiniXML = MiniXML.Open(t)

    override def makeClose(t: String): MiniXML = MiniXML.Close(t)

    override def makeLeaf(t: String): MiniXML = MiniXML.Text(t)

  }

  implicit object MiniXMLShow extends Show[MiniXML] {
    def show(node: MiniXML): String =
      node match {
        case Open(name)  => s"<$name>"
        case Close(name) => s"</$name>"
        case Text(t)     => t
      }
  }

}
