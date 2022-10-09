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

  def dsl[InTag, OutTag](build: MFTBuilder[InTag, OutTag] => Unit): MFT[InTag, OutTag] = {
    val builder = new MFTBuilder[InTag, OutTag]
    build(builder)
    builder.build
  }

  def state[InTag, OutTag](args: Int, initial: Boolean = false)(implicit
      builder: MFTBuilder[InTag, OutTag]): builder.StateBuilder = {
    val q = builder.states.size
    if (initial)
      builder.initial = q
    val st = new builder.StateBuilder(q, args)
    builder.states += st
    st
  }

  def any[InTag, OutTag](implicit builder: MFTBuilder[InTag, OutTag]): builder.PatternBuilder =
    builder.PatternBuilder.Any

  def node[InTag, OutTag](in: InTag)(implicit builder: MFTBuilder[InTag, OutTag]): builder.PatternBuilder =
    builder.PatternBuilder.Node(in)

  def anyNode[InTag, OutTag](implicit builder: MFTBuilder[InTag, OutTag]): builder.PatternBuilder =
    builder.PatternBuilder.AnyNode

  def leaf[InTag, OutTag](in: InTag)(implicit builder: MFTBuilder[InTag, OutTag]): builder.PatternBuilder =
    builder.PatternBuilder.Leaf(in)

  def anyLeaf[InTag, OutTag](implicit builder: MFTBuilder[InTag, OutTag]): builder.PatternBuilder =
    builder.PatternBuilder.AnyLeaf

  def epsilon[InTag, OutTag](implicit builder: MFTBuilder[InTag, OutTag]): builder.PatternBuilder =
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
