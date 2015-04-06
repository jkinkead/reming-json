/*
 * Original implementation (C) 2009-2011 Debasish Ghosh
 * Adapted and extended in 2011 by Mathias Doenitz
 * Adapted to reming in 2015 by Jesse Kinkead
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package reming

/** Provides the streaming formats for numbers, String, and Symbol. Note that the number readers are
  * non-strict in that they will happily round floating-point numbers to Integers.
  */
trait BasicFormats {
  implicit object IntFormat extends JsonFormat[Int] {
    override def write(value: Int, printer: JsonPrinter): Unit = printer.printInt(value)
    override def read(parser: JsonParser): Int = parser.readNumber().toInt
  }

  implicit object LongFormat extends JsonFormat[Long] {
    override def write(value: Long, printer: JsonPrinter): Unit = printer.printLong(value)
    override def read(parser: JsonParser): Long = parser.readNumber().toLong
  }

  implicit object FloatFormat extends JsonFormat[Float] {
    override def write(value: Float, printer: JsonPrinter): Unit = printer.printFloat(value)
    override def read(parser: JsonParser): Float = parser.readNumber().toFloat
  }

  implicit object DoubleFormat extends JsonFormat[Double] {
    override def write(value: Double, printer: JsonPrinter): Unit = printer.printDouble(value)
    override def read(parser: JsonParser): Double = parser.readNumber().toDouble
  }

  implicit object ByteFormat extends JsonFormat[Byte] {
    override def write(value: Byte, printer: JsonPrinter): Unit = printer.printByte(value)
    override def read(parser: JsonParser): Byte = parser.readNumber().toByte
  }

  implicit object ShortFormat extends JsonFormat[Short] {
    override def write(value: Short, printer: JsonPrinter): Unit = printer.printShort(value)
    override def read(parser: JsonParser): Short = parser.readNumber().toShort
  }

  implicit object BigDecimalFormat extends JsonFormat[BigDecimal] {
    override def write(value: BigDecimal, printer: JsonPrinter): Unit = {
      printer.printBigDecimal(value)
    }
    override def read(parser: JsonParser): BigDecimal = parser.readNumber()
  }

  implicit object BigIntFormat extends JsonFormat[BigInt] {
    override def write(value: BigInt, printer: JsonPrinter): Unit = printer.printBigInt(value)
    override def read(parser: JsonParser): BigInt = parser.readNumber().toBigInt
  }

  implicit object BooleanFormat extends JsonFormat[Boolean] {
    override def write(value: Boolean, printer: JsonPrinter): Unit = {
      printer.printBoolean(value)
    }
    override def read(parser: JsonParser): Boolean = parser.readBoolean()
  }

  implicit object CharFormat extends JsonFormat[Char] {
    override def write(value: Char, printer: JsonPrinter): Unit = {
      printer.printString(value.toString)
    }
    override def read(parser: JsonParser): Char = parser.readString() match {
      case x if x.length == 1 => x.charAt(0)
      case x => deserializationError("Expected Char as single-character JsString, but got " + x)
    }
  }

  implicit object StringFormat extends JsonFormat[String] {
    override def write(value: String, printer: JsonPrinter): Unit = printer.printString(value)
    override def read(parser: JsonParser): String = parser.readString()
  }

  implicit object SymbolFormat extends JsonFormat[Symbol] {
    override def write(value: Symbol, printer: JsonPrinter): Unit = {
      printer.printString(value.name)
    }
    override def read(parser: JsonParser): Symbol = Symbol(parser.readString())
  }
}
