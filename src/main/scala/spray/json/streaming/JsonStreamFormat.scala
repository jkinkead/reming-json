/*
 * Original implementation (C) 2009-2011 Debasish Ghosh
 * Adapted and extended in 2011 by Mathias Doenitz
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

import annotation.implicitNotFound

/** Provides the streaming JSON deserialization for type T, using the PullParser. */
@implicitNotFound(msg = "Cannot find JsonStreamReader or JsonStreamFormat type class for ${T}")
trait JsonStreamReader[T] {
  /** Reads an instance of the given type out of the given parser. */
  def read(parser: PullParser): T
}
object JsonStreamReader {
  /** Conversion to let any function be a JsonStreamReader. */
  implicit def func2Reader[T](f: PullParser => T): JsonStreamReader[T] = new JsonStreamReader[T] {
    override def read(parser: PullParser) = f(parser)
  }
}

/** Provides the streaming JSON serialization for type T. */
@implicitNotFound(msg = "Cannot find JsonStreamWriter or JsonStreamFormat type class for ${T}")
trait JsonStreamWriter[T] {
  /** Write the given instance of T to the given writer. */
  def write(obj: T, printer: JsonStreamPrinter): Unit
}
object JsonStreamWriter {
  /** Conversion to let any function be a JsonStreamWriter. */
  implicit def func2Writer[T](f: (T, JsonStreamPrinter) => Unit): JsonStreamWriter[T] = {
    new JsonStreamWriter[T] {
      override def write(obj: T, printer: JsonStreamPrinter) = f(obj, printer)
    }
  }
}

/** Provides the streaming JSON deserialization and serialization for type T. */
trait JsonStreamFormat[T] extends JsonStreamReader[T] with JsonStreamWriter[T]
