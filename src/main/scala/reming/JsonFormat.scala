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

package reming

import annotation.implicitNotFound

/** Provides the streaming JSON deserialization for type T, using the JsonParser. */
@implicitNotFound(msg = "Cannot find JsonReader or JsonFormat type class for ${T}")
trait JsonReader[T] {
  /** Reads an instance of the given type out of the given parser. */
  def read(parser: JsonParser): T

  def deserializationError(msg: String, cause: Throwable = null) =
    throw new DeserializationException(msg, cause)
}
object JsonReader {
  /** Conversion to let any function be a JsonReader. */
  implicit def func2Reader[T](f: JsonParser => T): JsonReader[T] = new JsonReader[T] {
    override def read(parser: JsonParser) = f(parser)
  }
}

/** Provides the streaming JSON serialization for type T. */
@implicitNotFound(msg = "Cannot find JsonWriter or JsonFormat type class for ${T}")
trait JsonWriter[T] {
  /** Write the given instance of T to the given writer. */
  def write(obj: T, printer: JsonPrinter): Unit

  def serializationError(msg: String) = throw new SerializationException(msg)
}
object JsonWriter {
  /** Conversion to let any function be a JsonWriter. */
  implicit def func2Writer[T](f: (T, JsonPrinter) => Unit): JsonWriter[T] = {
    new JsonWriter[T] {
      override def write(obj: T, printer: JsonPrinter) = f(obj, printer)
    }
  }
}

/** Provides the streaming JSON deserialization and serialization for type T. */
trait JsonFormat[T] extends JsonReader[T] with JsonWriter[T]

/** Lazy wrapper around serialization, for serializing recursive class hierarchies. This is because
  * in a recursive structure, need the format to exist before you can construct it.
  */
trait LazyFormat[T] extends JsonFormat[T] {
  /** @return the wrapped format to delegate to */
  def delegate: JsonFormat[T]

  final override def write(obj: T, printer: JsonPrinter): Unit = delegate.write(obj, printer)

  final override def read(parser: JsonParser): T = delegate.read(parser)
}
