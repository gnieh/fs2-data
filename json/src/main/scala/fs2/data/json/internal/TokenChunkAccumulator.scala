package fs2
package data
package json
package internals

import scala.collection.mutable.ListBuffer

private[json] final class TokenChunkAccumulator extends ChunkAccumulator[Token] {

  private[this] final val chunkAcc: ListBuffer[Token] = new ListBuffer

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
    Chunk.seq(chunkAcc.result())

  override def flush(): this.type = {
    chunkAcc.clear()
    this
  }

}
