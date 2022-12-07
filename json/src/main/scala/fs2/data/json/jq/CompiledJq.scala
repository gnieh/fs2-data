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

package fs2
package data
package json
package jq

import cats.data.NonEmptyList
import cats.syntax.all._

import esp.ESP
import tagged._
import pattern.Selectable
import tagged.TaggedJson
import esp.Tag
import esp.Conversion
import pattern.Evaluator
import pattern.ConstructorTree

class CompiledJq[F[_]: RaiseThrowable](esp: ESP[F, NonEmptyList[GuardTaggedMatcher], JsonTag, TaggedJson]) {

  private implicit object selected extends Selectable[TaggedJson, Tag[JsonTag]] {

    override def tree(e: TaggedJson): ConstructorTree[Tag[JsonTag]] =
      e match {
        case TaggedJson.EndObjectValue =>
          ConstructorTree(Tag.Close, List(ConstructorTree.noArgConstructor(Tag.Name(JsonTag.ObjectKey(None)))))
        case TaggedJson.StartArrayElement(idx) =>
          ConstructorTree(Tag.Open, List(ConstructorTree.noArgConstructor(Tag.Name(JsonTag.ArrayElement(Some(idx))))))
        case TaggedJson.StartObjectValue(key) =>
          ConstructorTree(Tag.Open, List(ConstructorTree.noArgConstructor(Tag.Name(JsonTag.ObjectKey(Some(key))))))
        case TaggedJson.EndArrayElement =>
          ConstructorTree(Tag.Close, List(ConstructorTree.noArgConstructor(Tag.Name(JsonTag.ArrayElement(None)))))
        case TaggedJson.Raw(token) =>
          token match {
            case Token.TrueValue =>
              ConstructorTree(Tag.Leaf, List(ConstructorTree.noArgConstructor(Tag.Value(JsonTag.Bool(Some(true))))))
            case Token.FalseValue =>
              ConstructorTree(Tag.Leaf, List(ConstructorTree.noArgConstructor(Tag.Value(JsonTag.Bool(Some(false))))))
            case Token.StringValue(s) =>
              ConstructorTree(Tag.Leaf, List(ConstructorTree.noArgConstructor(Tag.Value(JsonTag.Str(Some(s))))))
            case Token.StartArray =>
              ConstructorTree(Tag.Open, List(ConstructorTree.noArgConstructor(Tag.Name(JsonTag.Array))))
            case Token.EndArray =>
              ConstructorTree(Tag.Close, List(ConstructorTree.noArgConstructor(Tag.Name(JsonTag.Array))))
            case Token.StartObject =>
              ConstructorTree(Tag.Open, List(ConstructorTree.noArgConstructor(Tag.Name(JsonTag.Object))))
            case Token.EndObject =>
              ConstructorTree(Tag.Close, List(ConstructorTree.noArgConstructor(Tag.Name(JsonTag.Object))))
            case Token.NumberValue(n) =>
              ConstructorTree(Tag.Leaf, List(ConstructorTree.noArgConstructor(Tag.Value(JsonTag.Num(Some(n))))))
            case Token.Key(_) =>
              throw new Exception("this case should never occur, this is a bug")
            case Token.NullValue =>
              ConstructorTree(Tag.Leaf, List(ConstructorTree.noArgConstructor(Tag.Value(JsonTag.Null))))
          }
      }

  }

  private implicit object conversion extends Conversion[TaggedJson, TaggedJson] {

    override def makeOpen(t: TaggedJson): TaggedJson = t

    override def makeClose(t: TaggedJson): TaggedJson =
      t match {
        case TaggedJson.StartArrayElement(_)   => TaggedJson.EndArrayElement
        case TaggedJson.StartObjectValue(_)    => TaggedJson.EndObjectValue
        case TaggedJson.Raw(Token.StartArray)  => TaggedJson.Raw(Token.EndArray)
        case TaggedJson.Raw(Token.StartObject) => TaggedJson.Raw(Token.EndObject)
        case _                                 => t
      }

    override def makeLeaf(t: TaggedJson): TaggedJson = t

  }

  private implicit object evaluator extends Evaluator[NonEmptyList[GuardTaggedMatcher], Tag[TaggedJson]] {

    private def eval(guard: GuardTaggedMatcher, tree: ConstructorTree[Tag[TaggedJson]]): Boolean =
      (guard, tree) match {
        case (TaggedMatcher.Slice(start, end),
              ConstructorTree(Tag.Open, List(ConstructorTree(Tag.Name(TaggedJson.StartArrayElement(idx)), Nil)))) =>
          idx >= start && end.forall(idx < _)
        case (TaggedMatcher.Slice(_, _), _) =>
          false
        case (TaggedMatcher.Not(TaggedMatcher.StartObject),
              ConstructorTree(Tag.Open, List(ConstructorTree(Tag.Name(TaggedJson.Raw(Token.StartObject)), Nil)))) =>
          false
        case (TaggedMatcher.Not(TaggedMatcher.StartArray),
              ConstructorTree(Tag.Open, List(ConstructorTree(Tag.Name(TaggedJson.Raw(Token.StartArray)), Nil)))) =>
          false
        case (TaggedMatcher.Not(TaggedMatcher.Index(idx1)),
              ConstructorTree(Tag.Open, List(ConstructorTree(Tag.Name(TaggedJson.StartArrayElement(idx2)), Nil)))) =>
          idx1 =!= idx2
        case (TaggedMatcher.Not(TaggedMatcher.Slice(start, end)),
              ConstructorTree(Tag.Open, List(ConstructorTree(Tag.Name(TaggedJson.StartArrayElement(idx)), Nil)))) =>
          idx < start || end.exists(idx >= _)
        case (TaggedMatcher.Not(TaggedMatcher.Field(name1)),
              ConstructorTree(Tag.Open, List(ConstructorTree(Tag.Name(TaggedJson.StartObjectValue(name2)), Nil)))) =>
          name1 =!= name2
        case (TaggedMatcher.Not(_), _) =>
          true
      }

    override def eval(guard: NonEmptyList[GuardTaggedMatcher],
                      tree: ConstructorTree[Tag[TaggedJson]]): Option[Tag[TaggedJson]] =
      guard.forall(eval(_, tree)).guard[Option].as(Tag.Open)

  }

  def pipe(in: Stream[F, Token]): Stream[F, Token] =
    ???
    //in.through(JsonTagger.pipe).through(esp.pipe).map(untag(_)).unNone

}
