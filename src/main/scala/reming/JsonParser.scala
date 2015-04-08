/*
 * Original implementation (C) 2011 Mathias Doenitz
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

import java.lang.{ StringBuilder => JavaStringBuilder }

import scala.annotation.{ switch, tailrec }
import scala.collection.mutable
import scala.io.Source

object JsonParser {
  def read[T](input: String)(implicit reader: JsonReader[T]): T = {
    reader.read(withString(input))
  }

  def read[T](source: Source)(implicit reader: JsonReader[T]): T = {
    reader.read(withSource(source))
  }

  /** @return an initialized pull parser for the given input */
  def withSource(source: Source): JsonParser = {
    val parser = new JsonParser(new ParserInput.CharIteratorBasedParserInput(source))
    parser.start()
    parser
  }

  /** @return an initialized pull parser for the given string */
  def withString(string: String): JsonParser = {
    val parser = new JsonParser(new ParserInput.StringBasedParserInput(string))
    parser.start()
    parser
  }

  /** @return an initialized pull parser for the given input */
  def withInput(input: ParserInput): JsonParser = {
    val parser = new JsonParser(input)
    parser.start()
    parser
  }
}

/** JSON parser with pull semantics. Borrows heavily from the main JsonParser by Mathias Doenitz.
  * Public methods will throw DeserializationException if the expected value isn't next in the
  * input.
  */
class JsonParser(input: ParserInput) {
  private val sb = new JavaStringBuilder

  private[reming] var cursorChar: Char = _

  ////////////////////// GRAMMAR ////////////////////////

  private def `false`() = advance() && ch('a') && ch('l') && ch('s') && ws('e')
  private def `null`() = advance() && ch('u') && ch('l') && ws('l')
  private def `true`() = advance() && ch('r') && ch('u') && ws('e')

  /** Reads a string from the input. Used by both object reading and value reading code.
    * See http://tools.ietf.org/html/rfc4627#section-2.5
    */
  private def `string`(): String = {
    require('"')
    sb.setLength(0)
    while (`char`()) cursorChar = input.nextUtf8Char()
    require('"')
    ws()
    sb.toString()
  }

  /** Attempts to read a character from the input, and appends it to the internal buffer. This is
    * meant to be called from within a string - it handles escaping, and stops at an unescaped
    * quote.
    * @return true if a character was appended
    */
  private def `char`(): Boolean =
    // simple bloom-filter that quick-matches the most frequent case of characters that are ok to append
    // (it doesn't match control chars, EOI, '"', '?', '\', 'b' and certain higher, non-ASCII chars)
    if (((1L << cursorChar) & ((31 - cursorChar) >> 31) & 0x7ffffffbefffffffL) != 0L) appendSB(cursorChar)
    else cursorChar match {
      case '"' | EOI => false
      case '\\' =>
        advance(); `escaped`()
      case c => (c >= ' ') && appendSB(c)
    }

  /** Reads an escaped character from the input, and appends it to the buffer.  This assumes the
    * backslash has already been read.
    * @return true if a character was appended
    */
  private def `escaped`(): Boolean = {
    def hexValue(c: Char): Int =
      if ('0' <= c && c <= '9') c - '0'
      else if ('a' <= c && c <= 'f') c - 87
      else if ('A' <= c && c <= 'F') c - 55
      else fail("hex digit")
    def unicode() = {
      var value = hexValue(cursorChar)
      advance()
      value = (value << 4) + hexValue(cursorChar)
      advance()
      value = (value << 4) + hexValue(cursorChar)
      advance()
      value = (value << 4) + hexValue(cursorChar)
      appendSB(value.toChar)
    }
    (cursorChar: @switch) match {
      case '"' | '/' | '\\' => appendSB(cursorChar)
      case 'b' => appendSB('\b')
      case 'f' => appendSB('\f')
      case 'n' => appendSB('\n')
      case 'r' => appendSB('\r')
      case 't' => appendSB('\t')
      case 'u' =>
        advance(); unicode()
      case _ => fail("JSON escape sequence")
    }
  }

  // http://tools.ietf.org/html/rfc4627#section-2.4
  private def `number`(): BigDecimal = {
    sb.setLength(0)
    if (ch('-')) {
      sb.append('-')
    }
    `int`()
    `frac`()
    `exp`()
    ws()
    BigDecimal(sb.toString)
  }

  private def `int`(): Unit = if (!ch('0')) oneOrMoreDigits() else sb.append('0')
  private def `frac`(): Unit = if (ch('.')) {
    sb.append('.')
    oneOrMoreDigits()
  }
  private def `exp`(): Unit = if (ch('e') || ch('E')) {
    if (ch('-')) {
      sb.append("e-")
    } else if (ch('+')) {
      sb.append("e+")
    } else {
      sb.append('e')
    }
    oneOrMoreDigits()
  }

  private def oneOrMoreDigits(): Unit = if (DIGIT()) zeroOrMoreDigits() else fail("DIGIT")
  @tailrec private def zeroOrMoreDigits(): Unit = if (DIGIT()) {
    sb.append(cursorChar)
    advance()
    zeroOrMoreDigits()
  }

  private def DIGIT(): Boolean = cursorChar >= '0' && cursorChar <= '9'

  /** Skips all whitespace under the cursor. */
  private def ws(): Unit =
    // fast test whether cursorChar is one of " \n\r\t"
    while (((1L << cursorChar) & ((cursorChar - 64) >> 31) & 0x100002600L) != 0L) { advance() }

  ////////////////////////// PRIVATE HELPERS ////////////////////////////
  private def appendSB(c: Char): Boolean = { sb.append(c); true }

  ////////////////////////// PROTECTED HELPERS //////////////////////////

  private def ch(c: Char): Boolean = if (cursorChar == c) { advance(); true } else false
  private def ws(c: Char): Boolean = if (ch(c)) { ws(); true } else false
  private def advance(): Boolean = { cursorChar = input.nextChar(); true }
  private def require(c: Char): Unit = if (!ch(c)) fail(s"'$c'")

  private def fail(target: String): Nothing = {
    val ParserInput.Line(lineNrOption, colOption, text) = input.currLine(cursorChar)
    val summary = {
      val unexpected =
        if (cursorChar != EOI) {
          val c = if (Character.isISOControl(cursorChar)) "\\u%04x" format cursorChar.toInt else cursorChar.toString
          s"character '$c'"
        } else "end-of-input"
      (lineNrOption, colOption) match {
        case (Some(lineNr), Some(col)) =>
          s"Unexpected $unexpected at (line $lineNr, position $col), expected $target"
        case _ =>
          s"Unexpected $unexpected, expected $target"
      }
    }
    val detail = {
      val sanitizedText = text.map(c ⇒ if (Character.isISOControl(c)) '?' else c)
      colOption match {
        case Some(col) => s"\n$sanitizedText\n${" " * (col - 1)}^\n"
        case None => s"\n$sanitizedText\n"
      }
    }
    if (detail.isEmpty) {
      deserializationError(s"$summary")
    } else {
      deserializationError(s"$summary:$detail")
    }
  }

  // End-of-input sigil, forced to a compile-time constant.
  private final val EOI = '\uFFFF'

  /** The mapping of object keys to handler functions for the current object. */
  private var fieldValueHolders: mutable.Map[String, ObjectValue[_]] = _

  /** Starts a parse. */
  private[reming] def start(): Unit = advance()

  /** Looks for a null in the stream. If the next item in the stream is a null literal, this will
    * read it and return true; else, it leaves the stream as-is and returns false.
    */
  def maybeReadNull(): Boolean = {
    if (cursorChar == 'n') {
      `null`()
      true
    } else {
      false
    }
  }

  /** Reads a single boolean literal from the input.
    * @return the value of the literal
    * @throws DeserializationException if the next item in the stream isn't a boolean
    */
  def readBoolean(): Boolean = {
    if (cursorChar == 'f') {
      `false`()
      false
    } else if (cursorChar == 't') {
      `true`()
      true
    } else {
      fail("start of boolean literal")
    }
  }

  def readInt(): Int = {
    sb.setLength(0)
    if (ch('-')) {
      sb.append('-')
    }
    `int`()
    ws()
    sb.toString.toInt
  }

  def readLong(): Long = {
    sb.setLength(0)
    if (ch('-')) {
      sb.append('-')
    }
    `int`()
    ws()
    sb.toString.toLong
  }

  /** Reads a number from the stream. */
  def readNumber(): BigDecimal = {
    (cursorChar: @switch) match {
      case '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' | '-' => `number`()
      case _ => fail("start of number literal")
    }
  }

  /** Reads a single string literal from the input.
    * @return the value of the string
    * @throws DeserializationException if the next item in the stream isn't a string
    */
  def readString(): String = {
    if (cursorChar == '"') {
      string()
    } else {
      fail("start of string literal")
    }
  }

  /** Reads a type from the stream. */
  def read[T]()(implicit handler: JsonReader[T]): T = handler.read(this)

  private def startArrayInternal(): Unit = {
    if (cursorChar == '[') {
      advance()
      ws()
    } else {
      fail("start of non-null array")
    }
  }

  /** Start a JS array, and reads the first item.
    * @throws DeserializationException if there isn't a start of an array in the stream
    */
  def startArray[T]()(implicit handler: JsonReader[T]): T = {
    startArrayInternal()
    handler.read(this)
  }

  /** Read an item from an array. Behavior is undefined if not inside of an array when called. */
  def readArrayItem[T]()(implicit handler: JsonReader[T]): T = {
    require(',')
    ws()
    handler.read(this)
  }

  /** Ends an array that was started with startArray. */
  def endArray(): Unit = {
    require(']')
    ws()
  }

  /** Reads an array into an iterator. If additional methods are called on this parser before the
    * returned iterator is exhausted, behavior is undefined.
    */
  def readArray[T]()(implicit handler: JsonReader[T]): Iterator[T] = {
    startArrayInternal()
    val self = this
    new Iterator[T]() {
      var hasNextCached: Option[Boolean] = None
      var isFirst = true
      override def hasNext(): Boolean = hasNextCached getOrElse {
        val atEnd = ws(']')
        if (!atEnd) {
          // Require a comma if this isn't the first element.
          if (!isFirst) {
            require(',')
            ws()
          }
          isFirst = false
        }
        hasNextCached = Some(!atEnd)
        !atEnd
      }

      override def next(): T = {
        hasNextCached = None
        handler.read(self)
      }
    }
  }

  private def startObjectInternal(): Unit = {
    if (cursorChar == '{') {
      advance()
      ws()
    } else {
      fail("start of non-null object")
    }
  }

  /** Start a JS object.
    * @throws DeserializationException if there isn't a start of an object in the stream
    */
  def startObject(): Unit = {
    startObjectInternal()
    fieldValueHolders = mutable.HashMap.empty
  }

  /** Registers a handler for a given object key. If not called within parsing an object, behavior
    * is undefined.
    */
  def readField[T](key: String)(implicit fieldReader: JsonReader[T]): ObjectValue[T] = {
    val valueHolder = new ObjectValue(key, fieldReader)
    fieldValueHolders(key) = valueHolder
    valueHolder
  }

  /** Finishes parsing an object. This will call any handlers registered for field names. */
  def endObject(): Unit = {
    // Copy the current handler map reference, in case one of them wants to parse an object.
    val holders = fieldValueHolders
    holders.values foreach { _.setDefault() }
    if (cursorChar != '}') {
      do {
        val key = string()
        require(':')
        ws()
        holders.get(key) match {
          case Some(holder) => holder.readValue(this)
          case None => skipNextValue()
        }
      } while (ws(','))
    }
    require('}')
    ws()
  }

  /** Reads an object with same-typed values into an iterator. If additional methods are called on
    * this parser before the returned iterator is exhausted, behavior is undefined.
    * @param T the type of the values of the object
    */
  def readObject[T]()(implicit handler: JsonReader[T]): Iterator[(String, T)] = {
    startObjectInternal()
    val self = this
    new Iterator[(String, T)]() {
      var hasNextCached: Option[Boolean] = None
      var isFirst = true
      override def hasNext(): Boolean = hasNextCached getOrElse {
        val atEnd = ws('}')
        if (!atEnd) {
          // Require a comma if this isn't the first element.
          if (!isFirst) {
            require(',')
            ws()
          }
          isFirst = false
        }
        hasNextCached = Some(!atEnd)
        !atEnd
      }

      override def next(): (String, T) = {
        val key = string()
        require(':')
        ws()
        val value = handler.read(self)
        hasNextCached = None
        (key -> value)
      }
    }
  }

  /** Skips the next value in the JSON stream. */
  def skipNextValue(): Unit = {
    (cursorChar: @switch) match {
      case 'f' => `false`()
      case 'n' => `null`()
      case 't' => `true`()
      case '{' =>
        startObjectInternal()
        endObject()
      case '[' =>
        startArrayInternal()
        while (cursorChar != ']') {
          skipNextValue()
          if (cursorChar != ']') {
            require(',')
            ws()
          }
        }
        advance()
        ws()
      case '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' | '-' => `number`()
      case '"' => `string`()
      case _ => fail("JSON Value")
    }
  }
}
