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

package fs2.data.text.render.internal

private sealed trait NonEmptyIntList {
  def head: Int
  def ::(i: Int): NonEmptyIntList =
    More(i, this)
  def incHead: NonEmptyIntList
  def decHead: NonEmptyIntList
  def pop: NonEmptyIntList
}
private final case class One(head: Int) extends NonEmptyIntList {
  override def incHead: NonEmptyIntList = One(head + 1)
  override def decHead: NonEmptyIntList = One(head - 1)
  override lazy val pop: NonEmptyIntList = One(0)
}
private final case class More(head: Int, tail: NonEmptyIntList) extends NonEmptyIntList {
  override def incHead: NonEmptyIntList = More(head + 1, tail)
  override def decHead: NonEmptyIntList = More(head - 1, tail)
  override def pop: NonEmptyIntList = tail
}
