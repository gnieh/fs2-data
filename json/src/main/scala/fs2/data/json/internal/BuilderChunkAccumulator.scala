/*
 * Copyright 2023 Lucas Satabin
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
package internals

import scala.collection.immutable.{TreeMap, VectorBuilder}
import scala.collection.mutable.ListBuffer

import ast.Builder

/** A chunk accumulator that allows for building a stream of AST values
  * for the provided [[ast.Builder Builder]].
  *
  * The design is inspired by the jawn `Facade` and `FContext`.
  */
private[json] final class BuilderChunkAccumulator[Json](builder: Builder[Json]) extends ChunkAccumulator[Json] {

  private[this] final val chunkAcc: VectorBuilder[Json] = new VectorBuilder

  private trait Context {
    def string(s: String): Unit
    def value(v: Json): Unit
    def finish(): Json
  }

  private def toplevelContext(): Context =
    new Context {
      override def string(s: String): Unit = chunkAcc.addOne(builder.makeString(s))
      override def value(v: Json): Unit = chunkAcc.addOne(v)
      override def finish(): Json = builder.makeNull
    }
  private def arrayContext(): Context =
    new Context {
      private[this] val vs = ListBuffer.empty[Json]
      override def string(s: String): Unit = vs.addOne(builder.makeString(s))
      override def value(v: Json): Unit = vs.addOne(v)
      override def finish(): Json = builder.makeArray(vs)

    }
  private def objectContext(): Context =
    new Context {
      private[this] var key: String = null
      private[this] var vs = TreeMap.empty[String, Json]
      override def string(s: String): Unit = key = s
      override def value(v: Json): Unit = {
        vs = vs.updated(key, v)
        key = null
      }
      override def finish(): Json = builder.makeObject(vs)
    }

  private[this] var context: Context = toplevelContext()
  private[this] var stack: List[Context] = Nil

  override def startObject(): this.type = {
    stack = context :: stack
    context = objectContext()
    this
  }

  override def key(key: String): this.type = {
    context.string(key)
    this
  }

  override def endObject(): this.type =
    if (stack.isEmpty) {
      chunkAcc.addOne(context.finish())
      context = toplevelContext()
      this
    } else {
      val v = context.finish()
      context = stack.head
      context.value(v)
      stack = stack.tail
      this
    }

  override def startArray(): this.type = {
    stack = context :: stack
    context = arrayContext()
    this
  }

  override def endArray(): this.type =
    if (stack.isEmpty) {
      chunkAcc.addOne(context.finish())
      context = toplevelContext()
      this
    } else {
      val v = context.finish()
      context = stack.head
      context.value(v)
      stack = stack.tail
      this
    }

  override def nullValue(): this.type = {
    context.value(builder.makeNull)
    this
  }

  override def trueValue(): this.type = {
    context.value(builder.makeTrue)
    this
  }

  override def falseValue(): this.type = {
    context.value(builder.makeFalse)
    this
  }

  override def numberValue(value: String): this.type = {
    context.value(builder.makeNumber(value))
    this
  }

  override def stringValue(value: String): this.type = {
    context.value(builder.makeString(value))
    this
  }

  override def chunk(): Chunk[Json] =
    Chunk.from(chunkAcc.result())

  override def flush(): this.type = {
    chunkAcc.clear()
    this
  }

}
