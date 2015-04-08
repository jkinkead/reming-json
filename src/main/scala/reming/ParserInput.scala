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
import java.nio.{ CharBuffer, ByteBuffer }
import java.nio.charset.Charset
import scala.annotation.tailrec

trait ParserInput {
  /** Advance the cursor and get the next char.
    * Since the char is required to be a 7-Bit ASCII char no decoding is required.
    */
  def nextChar(): Char

  /** Advance the cursor and get the next char, which could potentially be outside
    * of the 7-Bit ASCII range. Therefore decoding might be required.
    */
  def nextUtf8Char(): Char

  def currLine(currChar: Char): ParserInput.Line
}

object ParserInput {
  private final val EOI = '\uFFFF' // compile-time constant
  private final val ErrorChar = '\uFFFD' // compile-time constant, universal UTF-8 replacement character '�'

  def apply(string: String): StringBasedParserInput = new StringBasedParserInput(string)
  def apply(chars: Array[Char]): CharArrayBasedParserInput = new CharArrayBasedParserInput(chars)
  def apply(bytes: Array[Byte]): ByteArrayBasedParserInput = new ByteArrayBasedParserInput(bytes)

  case class Line(lineNr: Option[Int], column: Option[Int], text: String)

  /** Parent class for parser input with a numeric cursor. */
  abstract class DefaultParserInput extends ParserInput {
    protected var _cursor: Int = -1
    /** @return the line that encloses the given cursor index */
    def currLine(currChar: Char): Line = {
      val index = _cursor
      val sb = new java.lang.StringBuilder
      @tailrec def rec(ix: Int, lineStartIx: Int, lineNr: Int): Line =
        nextUtf8Char() match {
          case '\n' if index > ix =>
            sb.setLength(0); rec(ix + 1, ix + 1, lineNr + 1)
          case '\n' | EOI => Line(Some(lineNr), Some(index - lineStartIx + 1), sb.toString)
          case c => sb.append(c); rec(ix + 1, lineStartIx, lineNr)
        }
      _cursor = -1
      val line = rec(ix = 0, lineStartIx = 0, lineNr = 1)
      _cursor = index
      line
    }
  }

  private val UTF8 = Charset.forName("UTF-8")

  /** ParserInput reading directly off a byte array which is assumed to contain the UTF-8 encoded representation
    * of the JSON input, without requiring a separate decoding step.
    */
  class ByteArrayBasedParserInput(bytes: Array[Byte]) extends DefaultParserInput {
    private val byteBuffer = ByteBuffer.allocate(4)
    private val charBuffer = CharBuffer.allocate(1) // we currently don't support surrogate pairs!
    private val decoder = UTF8.newDecoder()
    def nextChar() = {
      _cursor += 1
      if (_cursor < bytes.length) (bytes(_cursor) & 0xFF).toChar else EOI
    }
    def nextUtf8Char() = {
      @tailrec def decode(byte: Byte, remainingBytes: Int): Char = {
        byteBuffer.put(byte)
        if (remainingBytes > 0) {
          _cursor += 1
          if (_cursor < bytes.length) decode(bytes(_cursor), remainingBytes - 1) else ErrorChar
        } else {
          byteBuffer.flip()
          val coderResult = decoder.decode(byteBuffer, charBuffer, false)
          charBuffer.flip()
          val result = if (coderResult.isUnderflow & charBuffer.hasRemaining) charBuffer.get() else ErrorChar
          byteBuffer.clear()
          charBuffer.clear()
          result
        }
      }

      _cursor += 1
      if (_cursor < bytes.length) {
        val byte = bytes(_cursor)
        if (byte >= 0) byte.toChar // 7-Bit ASCII
        else if ((byte & 0xE0) == 0xC0) decode(byte, 1) // 2-byte UTF-8 sequence
        else if ((byte & 0xF0) == 0xE0) decode(byte, 2) // 3-byte UTF-8 sequence
        else if ((byte & 0xF8) == 0xF0) decode(byte, 3) // 4-byte UTF-8 sequence, will probably produce an (unsupported) surrogate pair
        else ErrorChar
      } else EOI
    }
  }

  class StringBasedParserInput(string: String) extends DefaultParserInput {
    def nextChar(): Char = {
      _cursor += 1
      if (_cursor < string.length) string.charAt(_cursor) else EOI
    }
    def nextUtf8Char() = nextChar()
  }

  class CharArrayBasedParserInput(chars: Array[Char]) extends DefaultParserInput {
    def nextChar(): Char = {
      _cursor += 1
      if (_cursor < chars.length) chars(_cursor) else EOI
    }
    def nextUtf8Char() = nextChar()
  }

  /** Input wrapping a character iterator (like a Source). */
  class CharIteratorBasedParserInput(chars: Iterator[Char]) extends ParserInput {
    /** The maximum length of the debug line to return. */
    val MaxLineLength = 50

    /** Return up to MaxLineLength characters in the current line, without valid` or column
      * counters.
      */
    override def currLine(currChar: Char): Line = {
      var char = currChar
      val buffer = new JavaStringBuilder()
      while (char != EOI && buffer.length < MaxLineLength) {
        buffer.append(char)
        char = nextChar
      }
      Line(None, None, buffer.toString)
    }
    override def nextChar(): Char = {
      if (chars.hasNext) {
        chars.next
      } else {
        EOI
      }
    }
    override def nextUtf8Char() = nextChar()
  }
}
