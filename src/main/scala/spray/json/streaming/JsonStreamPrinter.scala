package spray.json.streaming

import spray.json.{ serializationError, JsonPrinter }

import java.io.PrintWriter
import java.lang.{ StringBuilder => JavaStringBuilder }

/** Printer used for streaming. */
abstract class JsonStreamPrinter(writer: PrintWriter) {
  def printNull(): Unit = writer.print("null")
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

  def printString(value: String): Unit = {
    val escapedString = new JavaStringBuilder(value.length + 10)
    JsonPrinter.printString(value, escapedString)
    writer.print(escapedString)
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
