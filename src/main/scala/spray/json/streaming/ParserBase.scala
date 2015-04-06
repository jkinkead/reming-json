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

package spray.json.streaming

import java.lang.{ StringBuilder => JStringBuilder }
import java.nio.{ CharBuffer, ByteBuffer }
import java.nio.charset.Charset
import scala.annotation.{ switch, tailrec }

/** Fast, no-dependency parser for JSON as defined by http://tools.ietf.org/html/rfc4627.
  */
abstract class ParserBase(input: ParserInput) {
  private final val EOI = '\uFFFF' // compile-time constant

  private val sb = new JStringBuilder

  protected[json] var cursorChar: Char = _

  ////////////////////// GRAMMAR ////////////////////////

  protected def `false`() = advance() && ch('a') && ch('l') && ch('s') && ws('e')
  protected def `null`() = advance() && ch('u') && ch('l') && ws('l')
  protected def `true`() = advance() && ch('r') && ch('u') && ws('e')

  /** Reads a string from the input. Used by both object reading and value reading code.
    * See http://tools.ietf.org/html/rfc4627#section-2.5
    */
  protected def `string`(): String = {
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
  protected def `number`(): BigDecimal = {
    val sign = if (ch('-')) {
      Seq('-')
    } else {
      Seq.empty
    }
    val chars = sign ++ `int`() ++ `frac`() ++ `exp`()
    ws()
    BigDecimal(chars.toArray)
  }

  private def `int`(): Seq[Char] = if (!ch('0')) oneOrMoreDigits() else Seq('0')
  private def `frac`(): Seq[Char] = if (ch('.')) '.' +: oneOrMoreDigits() else Seq.empty
  private def `exp`(): Seq[Char] = if (ch('e') || ch('E')) {
    val sign = if (ch('-')) {
      Some('-')
    } else if (ch('+')) {
      Some('+')
    } else {
      None
    }
    Seq('e') ++ sign ++ oneOrMoreDigits()
  } else {
    Seq.empty
  }

  private def oneOrMoreDigits(): Seq[Char] = if (DIGIT()) zeroOrMoreDigits() else fail("DIGIT")
  @tailrec private def zeroOrMoreDigits(soFar: Seq[Char] = Seq.empty): Seq[Char] = {
    if (!DIGIT()) {
      soFar
    } else {
      val current = cursorChar
      advance()
      zeroOrMoreDigits(soFar :+ current)
    }
  }

  private def DIGIT(): Boolean = cursorChar >= '0' && cursorChar <= '9'

  /** Skips all whitespace under the cursor. */
  protected def ws(): Unit =
    // fast test whether cursorChar is one of " \n\r\t"
    while (((1L << cursorChar) & ((cursorChar - 64) >> 31) & 0x100002600L) != 0L) { advance() }

  ////////////////////////// PRIVATE HELPERS ////////////////////////////
  private def appendSB(c: Char): Boolean = { sb.append(c); true }

  ////////////////////////// PROTECTED HELPERS //////////////////////////

  protected def ch(c: Char): Boolean = if (cursorChar == c) { advance(); true } else false
  protected def ws(c: Char): Boolean = if (ch(c)) { ws(); true } else false
  protected def advance(): Boolean = { cursorChar = input.nextChar(); true }
  protected def require(c: Char): Unit = if (!ch(c)) fail(s"'$c'")

  protected def fail(target: String): Nothing = {
    val ParserInput.Line(lineNr, col, text) = input.currLine
    val summary = {
      val unexpected =
        if (cursorChar != EOI) {
          val c = if (Character.isISOControl(cursorChar)) "\\u%04x" format cursorChar.toInt else cursorChar.toString
          s"character '$c'"
        } else "end-of-input"
      s"Unexpected $unexpected at (line $lineNr, position $col), expected $target"
    }
    val detail = {
      val sanitizedText = text.map(c ⇒ if (Character.isISOControl(c)) '?' else c)
      s"\n$sanitizedText\n${" " * (col - 1)}^\n"
    }
    failWithException(summary, detail)
  }

  /** Throws an exception appropriate to the parser. */
  protected def failWithException(summary: String, detail: String): Nothing
}

trait ParserInput {
  /** Advance the cursor and get the next char.
    * Since the char is required to be a 7-Bit ASCII char no decoding is required.
    */
  def nextChar(): Char

  /** Advance the cursor and get the next char, which could potentially be outside
    * of the 7-Bit ASCII range. Therefore decoding might be required.
    */
  def nextUtf8Char(): Char

  def currLine: ParserInput.Line
}

object ParserInput {
  private final val EOI = '\uFFFF' // compile-time constant
  private final val ErrorChar = '\uFFFD' // compile-time constant, universal UTF-8 replacement character '�'

  def apply(string: String): StringBasedParserInput = new StringBasedParserInput(string)
  def apply(chars: Array[Char]): CharArrayBasedParserInput = new CharArrayBasedParserInput(chars)
  def apply(bytes: Array[Byte]): ByteArrayBasedParserInput = new ByteArrayBasedParserInput(bytes)

  case class Line(lineNr: Int, column: Int, text: String)

  /** Parent class for parser input with a numeric cursor. */
  abstract class DefaultParserInput extends ParserInput {
    protected var _cursor: Int = -1
    /** @return the line that encloses the given cursor index */
    def currLine: Line = {
      val index = _cursor
      val sb = new java.lang.StringBuilder
      @tailrec def rec(ix: Int, lineStartIx: Int, lineNr: Int): Line =
        nextUtf8Char() match {
          case '\n' if index > ix =>
            sb.setLength(0); rec(ix + 1, ix + 1, lineNr + 1)
          case '\n' | EOI => Line(lineNr, index - lineStartIx + 1, sb.toString)
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
    override def currLine: Line = {
      // TODO IMPLEMENT
      Line(0, 0, "")
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
