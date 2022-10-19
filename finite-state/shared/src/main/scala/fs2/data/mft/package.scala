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

package object mft {

  def dsl[Guard, InTag, OutTag](build: MFTBuilder[Guard, InTag, OutTag] => Unit): MFT[Guard, InTag, OutTag] = {
    val builder = new MFTBuilder[Guard, InTag, OutTag]
    build(builder)
    builder.build
  }

  def state[Guard, InTag, OutTag](args: Int, initial: Boolean = false)(implicit
      builder: MFTBuilder[Guard, InTag, OutTag]): builder.StateBuilder = {
    val q = builder.states.size
    if (initial)
      builder.initial = q
    val st = new builder.StateBuilder(q, args)
    builder.states += st
    st
  }

  def any[Guard, InTag, OutTag](implicit builder: MFTBuilder[Guard, InTag, OutTag]): builder.Guardable =
    builder.PatternBuilder.Any(None)

  def node[Guard, InTag, OutTag](in: InTag)(implicit builder: MFTBuilder[Guard, InTag, OutTag]): builder.Guardable =
    builder.PatternBuilder.Node(in, None)

  def anyNode[Guard, InTag, OutTag](implicit builder: MFTBuilder[Guard, InTag, OutTag]): builder.Guardable =
    builder.PatternBuilder.AnyNode(None)

  def leaf[Guard, InTag, OutTag](in: InTag)(implicit builder: MFTBuilder[Guard, InTag, OutTag]): builder.Guardable =
    builder.PatternBuilder.Leaf(in, None)

  def anyLeaf[Guard, InTag, OutTag](implicit builder: MFTBuilder[Guard, InTag, OutTag]): builder.Guardable =
    builder.PatternBuilder.AnyLeaf(None)

  def epsilon[Guard, InTag, OutTag](implicit builder: MFTBuilder[Guard, InTag, OutTag]): builder.PatternBuilder =
    builder.PatternBuilder.Epsilon

  def eps: Rhs[Nothing] =
    Rhs.Epsilon

  def y(i: Int): Rhs[Nothing] =
    Rhs.Param(i)

  def x0: Forest =
    Forest.Self

  def x1: Forest =
    Forest.First

  def x2: Forest =
    Forest.Second

  def node[OutTag](out: OutTag, children: Rhs[OutTag]): Rhs[OutTag] =
    Rhs.Node(out, children)

  def copy[OutTag](children: Rhs[OutTag]): Rhs[OutTag] =
    Rhs.CopyNode(children)

  def leaf[OutTag](out: OutTag): Rhs[OutTag] =
    Rhs.Leaf(out)

  def copy: Rhs[Nothing] =
    Rhs.CopyLeaf

}
