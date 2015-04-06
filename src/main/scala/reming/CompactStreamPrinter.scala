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
