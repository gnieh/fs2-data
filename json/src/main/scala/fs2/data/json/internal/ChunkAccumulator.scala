package fs2
package data
package json
package internals

/** A chunk accumulator allows to handle events being processed from the JSON parser.
  * It is an abstraction that allows to represent some accumulation of results
  * that will be emitted in a chunk returned by the `chunk` method.
  *
  * When calling `flush`, all the fully constructed values are cleared from the
  * chunk under construction, but results still being built are kept.
  * This allows to accumulate values acros upstream chunk boundaries.
  */
private[internals] trait ChunkAccumulator[Res] {

  def startObject(): this.type

  def key(key: String): this.type

  def endObject(): this.type

  def startArray(): this.type

  def endArray(): this.type

  def nullValue(): this.type

  def trueValue(): this.type

  def falseValue(): this.type

  def numberValue(value: String): this.type

  def stringValue(value: String): this.type

  def chunk(): Chunk[Res]

  def flush(): this.type

}

private[internals] object ChunkAccumulator {

  def empty[Res]: ChunkAccumulator[Res] = new ChunkAccumulator[Res] {

    override def startObject(): this.type = this

    override def key(key: String): this.type = this

    override def endObject(): this.type = this

    override def startArray(): this.type = this

    override def endArray(): this.type = this

    override def nullValue(): this.type = this

    override def trueValue(): this.type = this

    override def falseValue(): this.type = this

    override def numberValue(value: String): this.type = this

    override def stringValue(value: String): this.type = this

    override def chunk(): Chunk[Res] = Chunk.empty

    override def flush(): this.type = this

  }

}
