package spray.json.streaming

import spray.json.deserializationError

/** Provides the streaming formats for numbers, String, and Symbol. Note that the number readers are
  * non-strict in they will happily round floating-point numbers to Integers. This is how the
  * non-streaming formats work as well.
  */
trait BasicStreamFormats {
  implicit object IntStreamReader extends JsonStreamReader[Int] {
    override def read(parser: PullParser): Int = parser.readNumber().toInt
  }

  implicit object LongStreamReader extends JsonStreamReader[Long] {
    override def read(parser: PullParser): Long = parser.readNumber().toLong
  }

  implicit object FloatStreamReader extends JsonStreamReader[Float] {
    override def read(parser: PullParser): Float = parser.readNumber().toFloat
  }

  implicit object DoubleStreamReader extends JsonStreamReader[Double] {
    override def read(parser: PullParser): Double = parser.readNumber().toDouble
  }

  implicit object ByteStreamReader extends JsonStreamReader[Byte] {
    override def read(parser: PullParser): Byte = parser.readNumber().toByte
  }

  implicit object ShortStreamReader extends JsonStreamReader[Short] {
    override def read(parser: PullParser): Short = parser.readNumber().toShort
  }

  implicit object BigDecimalStreamReader extends JsonStreamReader[BigDecimal] {
    override def read(parser: PullParser): BigDecimal = parser.readNumber()
  }

  implicit object BigIntStreamReader extends JsonStreamReader[BigInt] {
    override def read(parser: PullParser): BigInt = parser.readNumber().toBigInt
  }

  implicit object BooleanStreamReader extends JsonStreamReader[Boolean] {
    override def read(parser: PullParser): Boolean = parser.readBoolean()
  }

  implicit object CharStreamReader extends JsonStreamReader[Char] {
    override def read(parser: PullParser): Char = parser.readString() match {
      case x if x.length == 1 => x.charAt(0)
      case x => deserializationError("Expected Char as single-character JsString, but got " + x)
    }
  }

  implicit object StringStreamReader extends JsonStreamReader[String] {
    override def read(parser: PullParser): String = parser.readString()
  }

  implicit object SymbolStreamReader extends JsonStreamReader[Symbol] {
    override def read(parser: PullParser): Symbol = Symbol(parser.readString())
  }
}
