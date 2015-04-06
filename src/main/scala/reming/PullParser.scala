/*
 * Copyright (C) 2015 by Jesse Kinkead
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

import scala.annotation.switch
import scala.collection.mutable
import scala.io.Source

object PullParser {
  def read[T](input: String)(implicit reader: JsonStreamReader[T]): T = {
    reader.read(withString(input))
  }

  def read[T](source: Source)(implicit reader: JsonStreamReader[T]): T = {
    reader.read(withSource(source))
  }

  /** @return an initialized pull parser for the given input */
  def withSource(source: Source): PullParser = {
    val parser = new PullParser(new ParserInput.CharIteratorBasedParserInput(source))
    parser.start()
    parser
  }

  /** @return an initialized pull parser for the given string */
  def withString(string: String): PullParser = {
    val parser = new PullParser(new ParserInput.StringBasedParserInput(string))
    parser.start()
    parser
  }

  /** @return an initialized pull parser for the given input */
  def withInput(input: ParserInput): PullParser = {
    val parser = new PullParser(input)
    parser.start()
    parser
  }
}

/** JSON parser with pull semantics. Borrows heavily from the main JsonParser by Mathias Doenitz.
  * Public methods will throw DeserializationException if the expected value isn't next in the
  * input.
  */
class PullParser(input: ParserInput) extends ParserBase(input) {

  // End-of-input sigil, forced to a compile-time constant.
  private final val EOI = '\uFFFF'

  /** The mapping of object keys to handler functions for the current object. */
  private var fieldValueHolders: mutable.Map[String, ObjectValue[_]] = _

  /** Throw a DeserializationException instead of ParsingException, since parsing is synonymous with
    * deserialization.
    */
  override def failWithException(summary: String, detail: String): Nothing = {
    if (detail.isEmpty) {
      deserializationError(s"$summary")
    } else {
      deserializationError(s"$summary:$detail")
    }
  }

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

  /** Reads a number from the stream. */
  def readNumber(): BigDecimal = {
    (cursorChar: @switch) match {
      case '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' | '-' => number()
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
  def read[T]()(implicit handler: JsonStreamReader[T]): T = handler.read(this)

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
  def startArray[T]()(implicit handler: JsonStreamReader[T]): T = {
    startArrayInternal()
    handler.read(this)
  }

  /** Read an item from an array. Behavior is undefined if not inside of an array when called. */
  def readArrayItem[T]()(implicit handler: JsonStreamReader[T]): T = {
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
  def readArray[T]()(implicit handler: JsonStreamReader[T]): Iterator[T] = {
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
  def readField[T](key: String)(implicit fieldReader: JsonStreamReader[T]): ObjectValue[T] = {
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
  def readObject[T]()(implicit handler: JsonStreamReader[T]): Iterator[(String, T)] = {
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
