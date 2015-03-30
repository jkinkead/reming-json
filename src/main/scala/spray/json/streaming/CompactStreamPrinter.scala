package spray.json.streaming

import java.io.{ PrintWriter, Writer }

object CompactStreamPrinter {
  def printTo[T](writer: Writer, value: T)(implicit jsonWriter: JsonStreamWriter[T]): Unit = {
    printTo(new PrintWriter(writer), value)
  }
  def printTo[T](writer: PrintWriter, value: T)(implicit jsonWriter: JsonStreamWriter[T]): Unit = {
    new CompactStreamPrinter(writer).print(value)
  }
}

class CompactStreamPrinter(writer: PrintWriter) extends JsonStreamPrinter(writer) {
  var afterFirstItem = false

  override def startArray(): Unit = {
    writer.write('[')
    afterFirstItem = false
  }

  /** Prints a field within an object. */
  override def printArrayItem[T](value: T)(implicit itemWriter: JsonStreamWriter[T]): Unit = {
    if (afterFirstItem) {
      writer.write(',')
    }
    afterFirstItem = true
    print(value)
  }

  override def endArray(): Unit = {
    afterFirstItem = true
    writer.write(']')
  }

  /** Prints a whole iterable as an array. */
  override def printArray[T](values: Iterable[T])(implicit itemWriter: JsonStreamWriter[T]): Unit = {
    startArray()
    for (value <- values) printArrayItem(value)
    endArray()
  }

  override def startObject(): Unit = {
    writer.write('{')
    afterFirstItem = false
  }

  /** Prints a field within an object. */
  override def printField[T](key: String, value: T)(implicit fieldWriter: JsonStreamWriter[T]): Unit = {
    if (afterFirstItem) {
      writer.write(',')
    }
    afterFirstItem = true
    printString(key)
    writer.write(':')
    print(value)
  }

  override def endObject(): Unit = {
    afterFirstItem = true
    writer.write('}')
  }
}
