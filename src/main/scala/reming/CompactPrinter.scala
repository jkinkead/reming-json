/*
 * Original implementation (C) 2009-2011 Mathias Doenitz
 * Rewritten for reming in 2015 by Jesse Kinkead
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

import java.io.{ PrintWriter, StringWriter, Writer }

object CompactPrinter {
  /** Prints the given value to the given writer. */
  def printTo[T](writer: Writer, value: T)(implicit jsonWriter: JsonWriter[T]): Unit = {
    val printWriter = writer match {
      case p: PrintWriter => p
      case _ => new PrintWriter(writer)
    }
    new CompactPrinter(printWriter).print(value)
  }

  /** Prints directly to a string. Useful for debugging; not as efficient as streaming directly to a
    * writer.
    * @return the JSON representation of `value` as a string
    */
  def printToString[T](value: T)(implicit jsonWriter: JsonWriter[T]): String = {
    val sw = new StringWriter()
    printTo(sw, value)
    sw.toString
  }
}

class CompactPrinter(writer: PrintWriter) extends JsonPrinter(writer) {
  var afterFirstItem = false

  override def startArray(): Unit = {
    writer.write('[')
    afterFirstItem = false
  }

  /** Prints a field within an object. */
  override def printArrayItem[T](value: T)(implicit itemWriter: JsonWriter[T]): Unit = {
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
  override def printArray[T](values: Iterable[T])(implicit itemWriter: JsonWriter[T]): Unit = {
    startArray()
    for (value <- values) printArrayItem(value)
    endArray()
  }

  override def startObject(): Unit = {
    writer.write('{')
    afterFirstItem = false
  }

  /** Prints a field within an object. */
  override def printField[T](key: String, value: T)(implicit fieldWriter: JsonWriter[T]): Unit = {
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
