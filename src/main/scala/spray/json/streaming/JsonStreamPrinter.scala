/*
 * Original implementation (C) 2009-2011 Mathias Doenitz
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

package spray.json.streaming

import java.io.PrintWriter
import java.lang.{ StringBuilder => JavaStringBuilder }

import scala.annotation.tailrec

/** Printer used for streaming. */
abstract class JsonStreamPrinter(writer: PrintWriter) {
  def printNull(): Unit = writer.write("null")
  def printBoolean(value: Boolean): Unit = writer.print(value)

  // Direct numeric serialization (no need to go through BigDecimal)
  def printInt(value: Int): Unit = writer.print(value)
  def printLong(value: Long): Unit = writer.print(value)

  // Route through BigDecimal, to ensure serialization compatibility with JsNumber.
  def printByte(value: Byte): Unit = writer.print(BigDecimal(value))
  def printShort(value: Short): Unit = writer.print(BigDecimal(value))
  def printBigInt(value: BigInt): Unit = writer.print(BigDecimal(value))

  /** Write the given double, throwing a serialization exception if it is NaN or Infinity. */
  def printDouble(value: Double): Unit = if (value.isNaN || value.isInfinity) {
    serializationError("can't serialize NaN or (+|-)Infinity")
  } else {
    writer.print(BigDecimal(value))
  }
  /** Write the given float, throwing a serialization exception if it is NaN or Infinity. */
  def printFloat(value: Float): Unit = printDouble(value)

  def printBigDecimal(value: BigDecimal): Unit = writer.print(value)

  def requiresEncoding(c: Char): Boolean = {
    // from RFC 4627
    // unescaped = %x20-21 / %x23-5B / %x5D-10FFFF
    c match {
      case '"' => true
      case '\\' => true
      case c => c < 0x20
    }
  }

  def printString(value: String): Unit = {
    @tailrec def firstToBeEncoded(ix: Int = 0): Int = {
      if (ix == value.length) {
        -1
      } else if (requiresEncoding(value.charAt(ix))) {
        ix
      } else {
        firstToBeEncoded(ix + 1)
      }
    }

    writer.write('"')
    firstToBeEncoded() match {
      case -1 => writer.write(value)
      case first =>
        writer.write(value, 0, first)
        @tailrec def append(ix: Int): Unit =
          if (ix < value.length) {
            value.charAt(ix) match {
              case c if !requiresEncoding(c) => writer.write(c)
              case '"' => writer.write("\\\"")
              case '\\' => writer.write("\\\\")
              case '\b' => writer.write("\\b")
              case '\f' => writer.write("\\f")
              case '\n' => writer.write("\\n")
              case '\r' => writer.write("\\r")
              case '\t' => writer.write("\\t")
              case x if x <= 0xF =>
                writer.write("\\u000")
                writer.write(Integer.toHexString(x))
              case x if x <= 0xFF =>
                writer.write("\\u00")
                writer.write(Integer.toHexString(x))
              case x if x <= 0xFFF =>
                writer.write("\\u0")
                writer.write(Integer.toHexString(x))
              case x =>
                writer.write("\\u")
                writer.write(Integer.toHexString(x))
            }
            append(ix + 1)
          }
        append(first)
    }
    writer.write('"')
  }

  def print[T](value: T)(implicit jsonWriter: JsonStreamWriter[T]): Unit = {
    jsonWriter.write(value, this)
  }

  // Abstract methods (different between pretty and non-pretty implementations).

  /** Prints the start of an array. Items should then be printed with printArrayItem, after which
    * the array should be ended with endArray().
    */
  def startArray(): Unit

  /** Prints an item in an array. Undefined behavior if called while not in the middle of printing
    * an array.
    */
  def printArrayItem[T](value: T)(implicit itemWriter: JsonStreamWriter[T]): Unit

  /** Ends an array. Undefined behavior if called while not in the middle of printing an array. */
  def endArray(): Unit

  def printArray[T](vals: Iterable[T])(implicit itemWriter: JsonStreamWriter[T]): Unit

  def startObject(): Unit

  def printField[T](key: String, value: T)(implicit fieldWriter: JsonStreamWriter[T]): Unit

  def endObject(): Unit
}
