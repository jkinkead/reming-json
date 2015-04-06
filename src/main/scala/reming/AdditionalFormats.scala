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

import scala.collection.mutable
import scala.reflect.ClassTag
import scala.util.Try

/** Provides formats and helpers. */
trait AdditionalFormats { self: BasicFormats =>
  // TODO(jkinkead): Add in format for spray JsValue.
  // TODO(jkinkead): Add in format for spray.JsonFormat.
  // TODO(jkinkead): Add in lift functions to turn Json{Writer,Reader} to JsonFormat?

  /** Builds a ChildFormat for class P that wraps an existing format for child class C. */
  def childFormat[C <: P : ClassTag : JsonFormat, P]: ChildFormat[C, P] = {
    new ChildFormat(implicitly[JsonFormat[C]])
  }

  /** Builds a ParentFormat for class P serializing any number of child classes of P. */
  def parentFormat[P](childFormats: ChildFormat[_ <: P, P]*): JsonFormat[P] = {
    new ParentFormat[P](childFormats)
  }

  /** Wraps a format for a child class C in a format for P. This adds type-checking to the
    * serialization, and exposes the runtime class of C, for use in ParentFormat.
    */
  class ChildFormat[C <: P : ClassTag, P](
      childFormat: JsonFormat[C]
  ) extends JsonFormat[P] {
    val childClass: Class[_] = implicitly[ClassTag[C]].runtimeClass

    override def write(value: P, printer: JsonPrinter): Unit = value match {
      case value: C => childFormat.write(value, printer)
      case _ => serializationError("Incompatible type: " + value)
    }
    override def read(parser: JsonParser): P = childFormat.read(parser)
  }

  /** A format for a class P with multiple child implementations. */
  class ParentFormat[P](formats: Iterable[ChildFormat[_ <: P, P]]) extends JsonFormat[P] {
    private val formatsByClass: Map[Class[_], ChildFormat[_ <: P, P]] = {
      val result: Map[Class[_], ChildFormat[_ <: P, P]] =
        (formats map { f => f.childClass -> f }).toMap
      require(
        result.size == formats.size,
        "ParentFormat created for ChildFormats with same runtime class!"
      )
      result
    }
    private val formatsByName: Map[String, JsonFormat[P]] =
      (formats map { f => f.childClass.getSimpleName -> f }).toMap

    override def write(value: P, printer: JsonPrinter): Unit = {
      val clazz = value.getClass
      formatsByClass.get(clazz) map { format =>
        printer.startArray()
        printer.printArrayItem(clazz.getSimpleName)
        printer.printArrayItem(value)(format)
        printer.endArray()
      } getOrElse {
        serializationError("No format found for class " + value.getClass)
      }
    }
    override def read(parser: JsonParser): P = {
      val name = parser.startArray[String]()
      val value = formatsByName.get(name) map { format =>
        parser.readArrayItem()(format)
      } getOrElse {
        deserializationError("No format for name " + name)
      }
      parser.endArray()
      value
    }
  }

  /** Lazy wrapper around serialization. Useful when you want to serialize (mutually) recursive
    * structures.
    */
  def lazyFormat[T](format: => JsonFormat[T]) = new JsonFormat[T] {
    lazy val delegate = format;
    override def write(value: T, printer: JsonPrinter): Unit = delegate.write(value, printer)
    override def read(parser: JsonParser): T = delegate.read(parser)
  }
}
