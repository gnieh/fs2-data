package fs2.data.csv
package generic

sealed trait Simple
case object On extends Simple
case object Off extends Simple

sealed trait Complex
case object Active extends Complex
//object Inactive extends Complex
case class Numbered(n: Int) extends Complex
case class Unknown(state: String) extends Complex

sealed trait Alphabet
@CsvValue("A") case object Alpha extends Alphabet
@CsvValue("B") case object Beta extends Alphabet
case object Gamma extends Alphabet

case class IntWrapper(value: Int)
case class IntResultWrapper(value: DecoderResult[Int])
case class Thing(value: String, extra: Int)
case class ThingWrapper(thing: Thing)
case class Wrapper[T](value: T)
