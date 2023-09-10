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

import scala.collection.immutable.VectorBuilder

private[json] final class TokenChunkAccumulator extends ChunkAccumulator[Token] {

  private[this] final val chunkAcc: VectorBuilder[Token] = new VectorBuilder

  override def startObject(): this.type = {
    chunkAcc.addOne(Token.StartObject)
    this
  }

  override def key(key: String): this.type = {
    chunkAcc.addOne(Token.Key(key))
    this
  }

  override def endObject(): this.type = {
    chunkAcc.addOne(Token.EndObject)
    this
  }

  override def startArray(): this.type = {
    chunkAcc.addOne(Token.StartArray)
    this
  }

  override def endArray(): this.type = {
    chunkAcc.addOne(Token.EndArray)
    this
  }

  override def nullValue(): this.type = {
    chunkAcc.addOne(Token.NullValue)
    this
  }

  override def trueValue(): this.type = {
    chunkAcc.addOne(Token.TrueValue)
    this
  }

  override def falseValue(): this.type = {
    chunkAcc.addOne(Token.FalseValue)
    this
  }

  override def numberValue(value: String): this.type = {
    chunkAcc.addOne(Token.NumberValue(value))
    this
  }

  override def stringValue(value: String): this.type = {
    chunkAcc.addOne(Token.StringValue(value))
    this
  }

  override def chunk(): Chunk[Token] =
    Chunk.from(chunkAcc.result())

  override def flush(): this.type = {
    chunkAcc.clear()
    this
  }

}
