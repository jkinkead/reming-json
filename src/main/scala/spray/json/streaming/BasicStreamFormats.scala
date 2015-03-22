package spray.json.streaming

import spray.json.deserializationError

/** Provides the streaming formats for numbers, String, and Symbol. Note that the number readers are
  * non-strict in they will happily round floating-point numbers to Integers. This is how the
  * non-streaming formats work as well.
  */
trait BasicStreamFormats {
  implicit object IntStreamFormat extends JsonStreamFormat[Int] {
    override def write(value: Int, printer: JsonStreamPrinter): Unit = printer.printInt(value)
    override def read(parser: PullParser): Int = parser.readNumber().toInt
  }

  implicit object LongStreamFormat extends JsonStreamFormat[Long] {
    override def write(value: Long, printer: JsonStreamPrinter): Unit = printer.printLong(value)
    override def read(parser: PullParser): Long = parser.readNumber().toLong
  }

  implicit object FloatStreamFormat extends JsonStreamFormat[Float] {
    override def write(value: Float, printer: JsonStreamPrinter): Unit = printer.printFloat(value)
    override def read(parser: PullParser): Float = parser.readNumber().toFloat
  }

  implicit object DoubleStreamFormat extends JsonStreamFormat[Double] {
    override def write(value: Double, printer: JsonStreamPrinter): Unit = printer.printDouble(value)
    override def read(parser: PullParser): Double = parser.readNumber().toDouble
  }

  implicit object ByteStreamFormat extends JsonStreamFormat[Byte] {
    override def write(value: Byte, printer: JsonStreamPrinter): Unit = printer.printByte(value)
    override def read(parser: PullParser): Byte = parser.readNumber().toByte
  }

  implicit object ShortStreamFormat extends JsonStreamFormat[Short] {
    override def write(value: Short, printer: JsonStreamPrinter): Unit = printer.printShort(value)
    override def read(parser: PullParser): Short = parser.readNumber().toShort
  }

  implicit object BigDecimalStreamFormat extends JsonStreamFormat[BigDecimal] {
    override def write(value: BigDecimal, printer: JsonStreamPrinter): Unit = {
      printer.printBigDecimal(value)
    }
    override def read(parser: PullParser): BigDecimal = parser.readNumber()
  }

  implicit object BigIntStreamFormat extends JsonStreamFormat[BigInt] {
    override def write(value: BigInt, printer: JsonStreamPrinter): Unit = printer.printBigInt(value)
    override def read(parser: PullParser): BigInt = parser.readNumber().toBigInt
  }

  implicit object BooleanStreamFormat extends JsonStreamFormat[Boolean] {
    override def write(value: Boolean, printer: JsonStreamPrinter): Unit = {
      printer.printBoolean(value)
    }
    override def read(parser: PullParser): Boolean = parser.readBoolean()
  }

  implicit object CharStreamFormat extends JsonStreamFormat[Char] {
    override def write(value: Char, printer: JsonStreamPrinter): Unit = {
      printer.printString(value.toString)
    }
    override def read(parser: PullParser): Char = parser.readString() match {
      case x if x.length == 1 => x.charAt(0)
      case x => deserializationError("Expected Char as single-character JsString, but got " + x)
    }
  }

  implicit object StringStreamFormat extends JsonStreamFormat[String] {
    override def write(value: String, printer: JsonStreamPrinter): Unit = printer.printString(value)
    override def read(parser: PullParser): String = parser.readString()
  }

  implicit object SymbolStreamFormat extends JsonStreamFormat[Symbol] {
    override def write(value: Symbol, printer: JsonStreamPrinter): Unit = {
      printer.printString(value.name)
    }
    override def read(parser: PullParser): Symbol = Symbol(parser.readString())
  }
}
