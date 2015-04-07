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

object PrettyPrinter {
  /** Prints the given value to the given writer. */
  def printTo[T](writer: Writer, value: T)(implicit jsonWriter: JsonWriter[T]): Unit = {
    printTo(new PrintWriter(writer), value)
  }

  /** Prints the given value to the given writer. */
  def printTo[T](writer: PrintWriter, value: T)(implicit jsonWriter: JsonWriter[T]): Unit = {
    new PrettyPrinter(writer).print(value)
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

class PrettyPrinter(writer: PrintWriter) extends JsonPrinter(writer) {
  var indentLevel = 0
  val Indent = 2
  var afterFirstItem = false

  override def startArray(): Unit = {
    writer.print('[')
    afterFirstItem = false
  }

  /** Prints a field within an object. */
  override def printArrayItem[T](value: T)(implicit itemWriter: JsonWriter[T]): Unit = {
    if (afterFirstItem) {
      writer.print(", ")
    }
    afterFirstItem = true
    print(value)
  }

  override def endArray(): Unit = {
    afterFirstItem = true
    writer.print(']')
  }

  /** Prints a whole iterable as an array. */
  override def printArray[T](values: Iterable[T])(implicit itemWriter: JsonWriter[T]): Unit = {
    startArray()
    for (value <- values) printArrayItem(value)
    endArray()
  }

  override def startObject(): Unit = {
    writer.print("{\n")
    indentLevel += 1
    afterFirstItem = false
  }

  /** Prints a field within an object. */
  override def printField[T](key: String, value: T)(implicit fieldWriter: JsonWriter[T]): Unit = {
    if (afterFirstItem) {
      writer.print(",\n")
    }
    afterFirstItem = true
    printIndent(indentLevel * Indent)
    printString(key)
    writer.print(": ")
    print(value)
  }

  override def endObject(): Unit = {
    afterFirstItem = true
    writer.print('\n')
    indentLevel -= 1
    printIndent(indentLevel * Indent)
    writer.print('}')
  }

  private def printIndent(indent: Int): Unit = for (_ <- 0 until indent) writer.print(' ')
}
