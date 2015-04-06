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

import java.lang.reflect.Modifier

import scala.reflect.ClassTag
import scala.util.control.NonFatal

object ProductFormats {
  private val operators = Map(
    "$eq" -> "=",
    "$greater" -> ">",
    "$less" -> "<",
    "$plus" -> "+",
    "$minus" -> "-",
    "$times" -> "*",
    "$div" -> "/",
    "$bang" -> "!",
    "$at" -> "@",
    "$hash" -> "#",
    "$percent" -> "%",
    "$up" -> "^",
    "$amp" -> "&",
    "$tilde" -> "~",
    "$qmark" -> "?",
    "$bar" -> "|"
  )

  private def unmangle(name: String) = operators.foldLeft(name) {
    case (n, (mangled, unmangled)) =>
      if (n.indexOf(mangled) >= 0) n.replace(mangled, unmangled) else n
  }

  private[reming] def extractFieldNames(classManifest: ClassTag[_]): Array[String] = {
    val clazz = classManifest.runtimeClass
    try {
      // copy methods have the form copy$default$N(), we need to sort them in order, but must account for the fact
      // that lexical sorting of ...8(), ...9(), ...10() is not correct, so we extract N and sort by N.toInt
      val copyDefaultMethods = clazz.getMethods.filter(_.getName.startsWith("copy$default$")).sortBy(
        _.getName.drop("copy$default$".length).takeWhile(_ != '(').toInt
      )
      val fields = clazz.getDeclaredFields.filterNot { f =>
        f.getName.startsWith("$") || Modifier.isTransient(f.getModifiers) || Modifier.isStatic(f.getModifiers)
      }
      if (copyDefaultMethods.length != fields.length)
        sys.error("Case class " + clazz.getName + " declares additional fields")
      if (fields.zip(copyDefaultMethods).exists { case (f, m) => f.getType != m.getReturnType })
        sys.error("Cannot determine field order of case class " + clazz.getName)
      fields.map(f => ProductFormats.unmangle(f.getName))
    } catch {
      case NonFatal(ex) => throw new RuntimeException("Cannot automatically determine case class field names and order " +
        "for '" + clazz.getName + "', please use the 'jsonFormat' overload with explicit field name specification", ex)
    }
  }
}

trait ProductStreamFormats extends ProductStreamFormatsInstances { this: StandardStreamFormats =>
  def jsonStreamFormat0[T](construct: () => T): JsonStreamFormat[T] = {
    new JsonStreamFormat[T] {
      def write(p: T, printer: JsonStreamPrinter): Unit = {
        printer.startObject()
        printer.endObject()
      }
      def read(parser: PullParser): T = {
        parser.startObject()
        parser.endObject()
        construct()
      }
    }
  }

  // helpers

  /** Writes the given field of a product. This special-cases None values using the default
    * serialization, omitting them from the serialization entirely. This is always called from
    * within an object serialization.
    * @param fieldName the name to serialize the field as
    * @param ix the index of the field to serialize in the product
    */
  protected def writeProductElement[T](
    fieldName: String,
    p: Product,
    ix: Int,
    printer: JsonStreamPrinter
  )(implicit writer: JsonStreamWriter[T]): Unit = {
    val value = p.productElement(ix).asInstanceOf[T]
    writer match {
      case _: OptionFormat[_] if (value == None) => // No-op
      case _ => printer.printField(fieldName, value)
    }
  }

  /** Reads a value from an object, treating a missing value as None if the format is for an Option.
    */
  protected def readObjectValue[T](
    value: ObjectValue[T]
  )(implicit reader: JsonStreamReader[T]): T = {
    if (reader.isInstanceOf[OptionFormat[_]] && value.optionalValue.isEmpty) {
      None.asInstanceOf[T]
    } else {
      value.value
    }
  }
}

/** This trait supplies an alternative rendering mode for optional case class members.
  * Normally optional members that are undefined (`None`) are not rendered at all.
  * By mixing in this trait into your custom JsonProtocol you can enforce the rendering of undefined members as `null`.
  * (Note that this only affect JSON writing, spray-json will always read missing optional members as well as `null`
  * optional members as `None`.)
  */
trait NullOptions extends ProductStreamFormats {
  this: StandardStreamFormats =>

  override protected def writeProductElement[T](
    fieldName: String,
    p: Product,
    ix: Int,
    printer: JsonStreamPrinter
  )(implicit writer: JsonStreamWriter[T]): Unit = {
    printer.printField(fieldName, p.productElement(ix).asInstanceOf[T])
  }
}
