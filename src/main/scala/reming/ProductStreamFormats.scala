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

import scala.reflect.{ classTag, ClassTag }
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

trait ProductStreamFormats { this: StandardStreamFormats =>
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

  // Case classes with 1 parameters

  def jsonStreamFormat1[P1 :JSF, T <: Product :ClassTag](construct: (P1) => T): JsonStreamFormat[T] = {
    val Array(p1) = ProductFormats.extractFieldNames(classTag[T])
    jsonStreamFormat(construct, p1)
  }
  def jsonStreamFormat[P1 :JSF, T <: Product](construct: (P1) => T, fieldName1: String): JsonStreamFormat[T] = new JsonStreamFormat[T]{
    def write(p: T, printer: JsonStreamPrinter): Unit = {
      printer.startObject()
      writeProductElement[P1](fieldName1, p, 0, printer)
      printer.endObject()
    }
    def read(parser: PullParser): T = {
      parser.startObject()
      val p1V = parser.readField[P1](fieldName1)
      parser.endObject()
      construct(readObjectValue(p1V))
    }
  }
  // Case classes with 2 parameters

  def jsonStreamFormat2[P1 :JSF, P2 :JSF, T <: Product :ClassTag](construct: (P1, P2) => T): JsonStreamFormat[T] = {
    val Array(p1, p2) = ProductFormats.extractFieldNames(classTag[T])
    jsonStreamFormat(construct, p1, p2)
  }
  def jsonStreamFormat[P1 :JSF, P2 :JSF, T <: Product](construct: (P1, P2) => T, fieldName1: String, fieldName2: String): JsonStreamFormat[T] = new JsonStreamFormat[T]{
    def write(p: T, printer: JsonStreamPrinter): Unit = {
      printer.startObject()
      writeProductElement[P1](fieldName1, p, 0, printer)
      writeProductElement[P2](fieldName2, p, 1, printer)
      printer.endObject()
    }
    def read(parser: PullParser): T = {
      parser.startObject()
      val p1V = parser.readField[P1](fieldName1)
      val p2V = parser.readField[P2](fieldName2)
      parser.endObject()
      construct(readObjectValue(p1V), readObjectValue(p2V))
    }
  }
  // Case classes with 3 parameters

  def jsonStreamFormat3[P1 :JSF, P2 :JSF, P3 :JSF, T <: Product :ClassTag](construct: (P1, P2, P3) => T): JsonStreamFormat[T] = {
    val Array(p1, p2, p3) = ProductFormats.extractFieldNames(classTag[T])
    jsonStreamFormat(construct, p1, p2, p3)
  }
  def jsonStreamFormat[P1 :JSF, P2 :JSF, P3 :JSF, T <: Product](construct: (P1, P2, P3) => T, fieldName1: String, fieldName2: String, fieldName3: String): JsonStreamFormat[T] = new JsonStreamFormat[T]{
    def write(p: T, printer: JsonStreamPrinter): Unit = {
      printer.startObject()
      writeProductElement[P1](fieldName1, p, 0, printer)
      writeProductElement[P2](fieldName2, p, 1, printer)
      writeProductElement[P3](fieldName3, p, 2, printer)
      printer.endObject()
    }
    def read(parser: PullParser): T = {
      parser.startObject()
      val p1V = parser.readField[P1](fieldName1)
      val p2V = parser.readField[P2](fieldName2)
      val p3V = parser.readField[P3](fieldName3)
      parser.endObject()
      construct(readObjectValue(p1V), readObjectValue(p2V), readObjectValue(p3V))
    }
  }
  // Case classes with 4 parameters

  def jsonStreamFormat4[P1 :JSF, P2 :JSF, P3 :JSF, P4 :JSF, T <: Product :ClassTag](construct: (P1, P2, P3, P4) => T): JsonStreamFormat[T] = {
    val Array(p1, p2, p3, p4) = ProductFormats.extractFieldNames(classTag[T])
    jsonStreamFormat(construct, p1, p2, p3, p4)
  }
  def jsonStreamFormat[P1 :JSF, P2 :JSF, P3 :JSF, P4 :JSF, T <: Product](construct: (P1, P2, P3, P4) => T, fieldName1: String, fieldName2: String, fieldName3: String, fieldName4: String): JsonStreamFormat[T] = new JsonStreamFormat[T]{
    def write(p: T, printer: JsonStreamPrinter): Unit = {
      printer.startObject()
      writeProductElement[P1](fieldName1, p, 0, printer)
      writeProductElement[P2](fieldName2, p, 1, printer)
      writeProductElement[P3](fieldName3, p, 2, printer)
      writeProductElement[P4](fieldName4, p, 3, printer)
      printer.endObject()
    }
    def read(parser: PullParser): T = {
      parser.startObject()
      val p1V = parser.readField[P1](fieldName1)
      val p2V = parser.readField[P2](fieldName2)
      val p3V = parser.readField[P3](fieldName3)
      val p4V = parser.readField[P4](fieldName4)
      parser.endObject()
      construct(readObjectValue(p1V), readObjectValue(p2V), readObjectValue(p3V), readObjectValue(p4V))
    }
  }
  // Case classes with 5 parameters

  def jsonStreamFormat5[P1 :JSF, P2 :JSF, P3 :JSF, P4 :JSF, P5 :JSF, T <: Product :ClassTag](construct: (P1, P2, P3, P4, P5) => T): JsonStreamFormat[T] = {
    val Array(p1, p2, p3, p4, p5) = ProductFormats.extractFieldNames(classTag[T])
    jsonStreamFormat(construct, p1, p2, p3, p4, p5)
  }
  def jsonStreamFormat[P1 :JSF, P2 :JSF, P3 :JSF, P4 :JSF, P5 :JSF, T <: Product](construct: (P1, P2, P3, P4, P5) => T, fieldName1: String, fieldName2: String, fieldName3: String, fieldName4: String, fieldName5: String): JsonStreamFormat[T] = new JsonStreamFormat[T]{
    def write(p: T, printer: JsonStreamPrinter): Unit = {
      printer.startObject()
      writeProductElement[P1](fieldName1, p, 0, printer)
      writeProductElement[P2](fieldName2, p, 1, printer)
      writeProductElement[P3](fieldName3, p, 2, printer)
      writeProductElement[P4](fieldName4, p, 3, printer)
      writeProductElement[P5](fieldName5, p, 4, printer)
      printer.endObject()
    }
    def read(parser: PullParser): T = {
      parser.startObject()
      val p1V = parser.readField[P1](fieldName1)
      val p2V = parser.readField[P2](fieldName2)
      val p3V = parser.readField[P3](fieldName3)
      val p4V = parser.readField[P4](fieldName4)
      val p5V = parser.readField[P5](fieldName5)
      parser.endObject()
      construct(readObjectValue(p1V), readObjectValue(p2V), readObjectValue(p3V), readObjectValue(p4V), readObjectValue(p5V))
    }
  }
  // Case classes with 6 parameters

  def jsonStreamFormat6[P1 :JSF, P2 :JSF, P3 :JSF, P4 :JSF, P5 :JSF, P6 :JSF, T <: Product :ClassTag](construct: (P1, P2, P3, P4, P5, P6) => T): JsonStreamFormat[T] = {
    val Array(p1, p2, p3, p4, p5, p6) = ProductFormats.extractFieldNames(classTag[T])
    jsonStreamFormat(construct, p1, p2, p3, p4, p5, p6)
  }
  def jsonStreamFormat[P1 :JSF, P2 :JSF, P3 :JSF, P4 :JSF, P5 :JSF, P6 :JSF, T <: Product](construct: (P1, P2, P3, P4, P5, P6) => T, fieldName1: String, fieldName2: String, fieldName3: String, fieldName4: String, fieldName5: String, fieldName6: String): JsonStreamFormat[T] = new JsonStreamFormat[T]{
    def write(p: T, printer: JsonStreamPrinter): Unit = {
      printer.startObject()
      writeProductElement[P1](fieldName1, p, 0, printer)
      writeProductElement[P2](fieldName2, p, 1, printer)
      writeProductElement[P3](fieldName3, p, 2, printer)
      writeProductElement[P4](fieldName4, p, 3, printer)
      writeProductElement[P5](fieldName5, p, 4, printer)
      writeProductElement[P6](fieldName6, p, 5, printer)
      printer.endObject()
    }
    def read(parser: PullParser): T = {
      parser.startObject()
      val p1V = parser.readField[P1](fieldName1)
      val p2V = parser.readField[P2](fieldName2)
      val p3V = parser.readField[P3](fieldName3)
      val p4V = parser.readField[P4](fieldName4)
      val p5V = parser.readField[P5](fieldName5)
      val p6V = parser.readField[P6](fieldName6)
      parser.endObject()
      construct(readObjectValue(p1V), readObjectValue(p2V), readObjectValue(p3V), readObjectValue(p4V), readObjectValue(p5V), readObjectValue(p6V))
    }
  }
  // Case classes with 7 parameters

  def jsonStreamFormat7[P1 :JSF, P2 :JSF, P3 :JSF, P4 :JSF, P5 :JSF, P6 :JSF, P7 :JSF, T <: Product :ClassTag](construct: (P1, P2, P3, P4, P5, P6, P7) => T): JsonStreamFormat[T] = {
    val Array(p1, p2, p3, p4, p5, p6, p7) = ProductFormats.extractFieldNames(classTag[T])
    jsonStreamFormat(construct, p1, p2, p3, p4, p5, p6, p7)
  }
  def jsonStreamFormat[P1 :JSF, P2 :JSF, P3 :JSF, P4 :JSF, P5 :JSF, P6 :JSF, P7 :JSF, T <: Product](construct: (P1, P2, P3, P4, P5, P6, P7) => T, fieldName1: String, fieldName2: String, fieldName3: String, fieldName4: String, fieldName5: String, fieldName6: String, fieldName7: String): JsonStreamFormat[T] = new JsonStreamFormat[T]{
    def write(p: T, printer: JsonStreamPrinter): Unit = {
      printer.startObject()
      writeProductElement[P1](fieldName1, p, 0, printer)
      writeProductElement[P2](fieldName2, p, 1, printer)
      writeProductElement[P3](fieldName3, p, 2, printer)
      writeProductElement[P4](fieldName4, p, 3, printer)
      writeProductElement[P5](fieldName5, p, 4, printer)
      writeProductElement[P6](fieldName6, p, 5, printer)
      writeProductElement[P7](fieldName7, p, 6, printer)
      printer.endObject()
    }
    def read(parser: PullParser): T = {
      parser.startObject()
      val p1V = parser.readField[P1](fieldName1)
      val p2V = parser.readField[P2](fieldName2)
      val p3V = parser.readField[P3](fieldName3)
      val p4V = parser.readField[P4](fieldName4)
      val p5V = parser.readField[P5](fieldName5)
      val p6V = parser.readField[P6](fieldName6)
      val p7V = parser.readField[P7](fieldName7)
      parser.endObject()
      construct(readObjectValue(p1V), readObjectValue(p2V), readObjectValue(p3V), readObjectValue(p4V), readObjectValue(p5V), readObjectValue(p6V), readObjectValue(p7V))
    }
  }
  // Case classes with 8 parameters

  def jsonStreamFormat8[P1 :JSF, P2 :JSF, P3 :JSF, P4 :JSF, P5 :JSF, P6 :JSF, P7 :JSF, P8 :JSF, T <: Product :ClassTag](construct: (P1, P2, P3, P4, P5, P6, P7, P8) => T): JsonStreamFormat[T] = {
    val Array(p1, p2, p3, p4, p5, p6, p7, p8) = ProductFormats.extractFieldNames(classTag[T])
    jsonStreamFormat(construct, p1, p2, p3, p4, p5, p6, p7, p8)
  }
  def jsonStreamFormat[P1 :JSF, P2 :JSF, P3 :JSF, P4 :JSF, P5 :JSF, P6 :JSF, P7 :JSF, P8 :JSF, T <: Product](construct: (P1, P2, P3, P4, P5, P6, P7, P8) => T, fieldName1: String, fieldName2: String, fieldName3: String, fieldName4: String, fieldName5: String, fieldName6: String, fieldName7: String, fieldName8: String): JsonStreamFormat[T] = new JsonStreamFormat[T]{
    def write(p: T, printer: JsonStreamPrinter): Unit = {
      printer.startObject()
      writeProductElement[P1](fieldName1, p, 0, printer)
      writeProductElement[P2](fieldName2, p, 1, printer)
      writeProductElement[P3](fieldName3, p, 2, printer)
      writeProductElement[P4](fieldName4, p, 3, printer)
      writeProductElement[P5](fieldName5, p, 4, printer)
      writeProductElement[P6](fieldName6, p, 5, printer)
      writeProductElement[P7](fieldName7, p, 6, printer)
      writeProductElement[P8](fieldName8, p, 7, printer)
      printer.endObject()
    }
    def read(parser: PullParser): T = {
      parser.startObject()
      val p1V = parser.readField[P1](fieldName1)
      val p2V = parser.readField[P2](fieldName2)
      val p3V = parser.readField[P3](fieldName3)
      val p4V = parser.readField[P4](fieldName4)
      val p5V = parser.readField[P5](fieldName5)
      val p6V = parser.readField[P6](fieldName6)
      val p7V = parser.readField[P7](fieldName7)
      val p8V = parser.readField[P8](fieldName8)
      parser.endObject()
      construct(readObjectValue(p1V), readObjectValue(p2V), readObjectValue(p3V), readObjectValue(p4V), readObjectValue(p5V), readObjectValue(p6V), readObjectValue(p7V), readObjectValue(p8V))
    }
  }
  // Case classes with 9 parameters

  def jsonStreamFormat9[P1 :JSF, P2 :JSF, P3 :JSF, P4 :JSF, P5 :JSF, P6 :JSF, P7 :JSF, P8 :JSF, P9 :JSF, T <: Product :ClassTag](construct: (P1, P2, P3, P4, P5, P6, P7, P8, P9) => T): JsonStreamFormat[T] = {
    val Array(p1, p2, p3, p4, p5, p6, p7, p8, p9) = ProductFormats.extractFieldNames(classTag[T])
    jsonStreamFormat(construct, p1, p2, p3, p4, p5, p6, p7, p8, p9)
  }
  def jsonStreamFormat[P1 :JSF, P2 :JSF, P3 :JSF, P4 :JSF, P5 :JSF, P6 :JSF, P7 :JSF, P8 :JSF, P9 :JSF, T <: Product](construct: (P1, P2, P3, P4, P5, P6, P7, P8, P9) => T, fieldName1: String, fieldName2: String, fieldName3: String, fieldName4: String, fieldName5: String, fieldName6: String, fieldName7: String, fieldName8: String, fieldName9: String): JsonStreamFormat[T] = new JsonStreamFormat[T]{
    def write(p: T, printer: JsonStreamPrinter): Unit = {
      printer.startObject()
      writeProductElement[P1](fieldName1, p, 0, printer)
      writeProductElement[P2](fieldName2, p, 1, printer)
      writeProductElement[P3](fieldName3, p, 2, printer)
      writeProductElement[P4](fieldName4, p, 3, printer)
      writeProductElement[P5](fieldName5, p, 4, printer)
      writeProductElement[P6](fieldName6, p, 5, printer)
      writeProductElement[P7](fieldName7, p, 6, printer)
      writeProductElement[P8](fieldName8, p, 7, printer)
      writeProductElement[P9](fieldName9, p, 8, printer)
      printer.endObject()
    }
    def read(parser: PullParser): T = {
      parser.startObject()
      val p1V = parser.readField[P1](fieldName1)
      val p2V = parser.readField[P2](fieldName2)
      val p3V = parser.readField[P3](fieldName3)
      val p4V = parser.readField[P4](fieldName4)
      val p5V = parser.readField[P5](fieldName5)
      val p6V = parser.readField[P6](fieldName6)
      val p7V = parser.readField[P7](fieldName7)
      val p8V = parser.readField[P8](fieldName8)
      val p9V = parser.readField[P9](fieldName9)
      parser.endObject()
      construct(readObjectValue(p1V), readObjectValue(p2V), readObjectValue(p3V), readObjectValue(p4V), readObjectValue(p5V), readObjectValue(p6V), readObjectValue(p7V), readObjectValue(p8V), readObjectValue(p9V))
    }
  }
  // Case classes with 10 parameters

  def jsonStreamFormat10[P1 :JSF, P2 :JSF, P3 :JSF, P4 :JSF, P5 :JSF, P6 :JSF, P7 :JSF, P8 :JSF, P9 :JSF, P10 :JSF, T <: Product :ClassTag](construct: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10) => T): JsonStreamFormat[T] = {
    val Array(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10) = ProductFormats.extractFieldNames(classTag[T])
    jsonStreamFormat(construct, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10)
  }
  def jsonStreamFormat[P1 :JSF, P2 :JSF, P3 :JSF, P4 :JSF, P5 :JSF, P6 :JSF, P7 :JSF, P8 :JSF, P9 :JSF, P10 :JSF, T <: Product](construct: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10) => T, fieldName1: String, fieldName2: String, fieldName3: String, fieldName4: String, fieldName5: String, fieldName6: String, fieldName7: String, fieldName8: String, fieldName9: String, fieldName10: String): JsonStreamFormat[T] = new JsonStreamFormat[T]{
    def write(p: T, printer: JsonStreamPrinter): Unit = {
      printer.startObject()
      writeProductElement[P1](fieldName1, p, 0, printer)
      writeProductElement[P2](fieldName2, p, 1, printer)
      writeProductElement[P3](fieldName3, p, 2, printer)
      writeProductElement[P4](fieldName4, p, 3, printer)
      writeProductElement[P5](fieldName5, p, 4, printer)
      writeProductElement[P6](fieldName6, p, 5, printer)
      writeProductElement[P7](fieldName7, p, 6, printer)
      writeProductElement[P8](fieldName8, p, 7, printer)
      writeProductElement[P9](fieldName9, p, 8, printer)
      writeProductElement[P10](fieldName10, p, 9, printer)
      printer.endObject()
    }
    def read(parser: PullParser): T = {
      parser.startObject()
      val p1V = parser.readField[P1](fieldName1)
      val p2V = parser.readField[P2](fieldName2)
      val p3V = parser.readField[P3](fieldName3)
      val p4V = parser.readField[P4](fieldName4)
      val p5V = parser.readField[P5](fieldName5)
      val p6V = parser.readField[P6](fieldName6)
      val p7V = parser.readField[P7](fieldName7)
      val p8V = parser.readField[P8](fieldName8)
      val p9V = parser.readField[P9](fieldName9)
      val p10V = parser.readField[P10](fieldName10)
      parser.endObject()
      construct(readObjectValue(p1V), readObjectValue(p2V), readObjectValue(p3V), readObjectValue(p4V), readObjectValue(p5V), readObjectValue(p6V), readObjectValue(p7V), readObjectValue(p8V), readObjectValue(p9V), readObjectValue(p10V))
    }
  }
  // Case classes with 11 parameters

  def jsonStreamFormat11[P1 :JSF, P2 :JSF, P3 :JSF, P4 :JSF, P5 :JSF, P6 :JSF, P7 :JSF, P8 :JSF, P9 :JSF, P10 :JSF, P11 :JSF, T <: Product :ClassTag](construct: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11) => T): JsonStreamFormat[T] = {
    val Array(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11) = ProductFormats.extractFieldNames(classTag[T])
    jsonStreamFormat(construct, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11)
  }
  def jsonStreamFormat[P1 :JSF, P2 :JSF, P3 :JSF, P4 :JSF, P5 :JSF, P6 :JSF, P7 :JSF, P8 :JSF, P9 :JSF, P10 :JSF, P11 :JSF, T <: Product](construct: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11) => T, fieldName1: String, fieldName2: String, fieldName3: String, fieldName4: String, fieldName5: String, fieldName6: String, fieldName7: String, fieldName8: String, fieldName9: String, fieldName10: String, fieldName11: String): JsonStreamFormat[T] = new JsonStreamFormat[T]{
    def write(p: T, printer: JsonStreamPrinter): Unit = {
      printer.startObject()
      writeProductElement[P1](fieldName1, p, 0, printer)
      writeProductElement[P2](fieldName2, p, 1, printer)
      writeProductElement[P3](fieldName3, p, 2, printer)
      writeProductElement[P4](fieldName4, p, 3, printer)
      writeProductElement[P5](fieldName5, p, 4, printer)
      writeProductElement[P6](fieldName6, p, 5, printer)
      writeProductElement[P7](fieldName7, p, 6, printer)
      writeProductElement[P8](fieldName8, p, 7, printer)
      writeProductElement[P9](fieldName9, p, 8, printer)
      writeProductElement[P10](fieldName10, p, 9, printer)
      writeProductElement[P11](fieldName11, p, 10, printer)
      printer.endObject()
    }
    def read(parser: PullParser): T = {
      parser.startObject()
      val p1V = parser.readField[P1](fieldName1)
      val p2V = parser.readField[P2](fieldName2)
      val p3V = parser.readField[P3](fieldName3)
      val p4V = parser.readField[P4](fieldName4)
      val p5V = parser.readField[P5](fieldName5)
      val p6V = parser.readField[P6](fieldName6)
      val p7V = parser.readField[P7](fieldName7)
      val p8V = parser.readField[P8](fieldName8)
      val p9V = parser.readField[P9](fieldName9)
      val p10V = parser.readField[P10](fieldName10)
      val p11V = parser.readField[P11](fieldName11)
      parser.endObject()
      construct(readObjectValue(p1V), readObjectValue(p2V), readObjectValue(p3V), readObjectValue(p4V), readObjectValue(p5V), readObjectValue(p6V), readObjectValue(p7V), readObjectValue(p8V), readObjectValue(p9V), readObjectValue(p10V), readObjectValue(p11V))
    }
  }
  // Case classes with 12 parameters

  def jsonStreamFormat12[P1 :JSF, P2 :JSF, P3 :JSF, P4 :JSF, P5 :JSF, P6 :JSF, P7 :JSF, P8 :JSF, P9 :JSF, P10 :JSF, P11 :JSF, P12 :JSF, T <: Product :ClassTag](construct: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12) => T): JsonStreamFormat[T] = {
    val Array(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12) = ProductFormats.extractFieldNames(classTag[T])
    jsonStreamFormat(construct, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12)
  }
  def jsonStreamFormat[P1 :JSF, P2 :JSF, P3 :JSF, P4 :JSF, P5 :JSF, P6 :JSF, P7 :JSF, P8 :JSF, P9 :JSF, P10 :JSF, P11 :JSF, P12 :JSF, T <: Product](construct: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12) => T, fieldName1: String, fieldName2: String, fieldName3: String, fieldName4: String, fieldName5: String, fieldName6: String, fieldName7: String, fieldName8: String, fieldName9: String, fieldName10: String, fieldName11: String, fieldName12: String): JsonStreamFormat[T] = new JsonStreamFormat[T]{
    def write(p: T, printer: JsonStreamPrinter): Unit = {
      printer.startObject()
      writeProductElement[P1](fieldName1, p, 0, printer)
      writeProductElement[P2](fieldName2, p, 1, printer)
      writeProductElement[P3](fieldName3, p, 2, printer)
      writeProductElement[P4](fieldName4, p, 3, printer)
      writeProductElement[P5](fieldName5, p, 4, printer)
      writeProductElement[P6](fieldName6, p, 5, printer)
      writeProductElement[P7](fieldName7, p, 6, printer)
      writeProductElement[P8](fieldName8, p, 7, printer)
      writeProductElement[P9](fieldName9, p, 8, printer)
      writeProductElement[P10](fieldName10, p, 9, printer)
      writeProductElement[P11](fieldName11, p, 10, printer)
      writeProductElement[P12](fieldName12, p, 11, printer)
      printer.endObject()
    }
    def read(parser: PullParser): T = {
      parser.startObject()
      val p1V = parser.readField[P1](fieldName1)
      val p2V = parser.readField[P2](fieldName2)
      val p3V = parser.readField[P3](fieldName3)
      val p4V = parser.readField[P4](fieldName4)
      val p5V = parser.readField[P5](fieldName5)
      val p6V = parser.readField[P6](fieldName6)
      val p7V = parser.readField[P7](fieldName7)
      val p8V = parser.readField[P8](fieldName8)
      val p9V = parser.readField[P9](fieldName9)
      val p10V = parser.readField[P10](fieldName10)
      val p11V = parser.readField[P11](fieldName11)
      val p12V = parser.readField[P12](fieldName12)
      parser.endObject()
      construct(readObjectValue(p1V), readObjectValue(p2V), readObjectValue(p3V), readObjectValue(p4V), readObjectValue(p5V), readObjectValue(p6V), readObjectValue(p7V), readObjectValue(p8V), readObjectValue(p9V), readObjectValue(p10V), readObjectValue(p11V), readObjectValue(p12V))
    }
  }
  // Case classes with 13 parameters

  def jsonStreamFormat13[P1 :JSF, P2 :JSF, P3 :JSF, P4 :JSF, P5 :JSF, P6 :JSF, P7 :JSF, P8 :JSF, P9 :JSF, P10 :JSF, P11 :JSF, P12 :JSF, P13 :JSF, T <: Product :ClassTag](construct: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13) => T): JsonStreamFormat[T] = {
    val Array(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13) = ProductFormats.extractFieldNames(classTag[T])
    jsonStreamFormat(construct, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13)
  }
  def jsonStreamFormat[P1 :JSF, P2 :JSF, P3 :JSF, P4 :JSF, P5 :JSF, P6 :JSF, P7 :JSF, P8 :JSF, P9 :JSF, P10 :JSF, P11 :JSF, P12 :JSF, P13 :JSF, T <: Product](construct: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13) => T, fieldName1: String, fieldName2: String, fieldName3: String, fieldName4: String, fieldName5: String, fieldName6: String, fieldName7: String, fieldName8: String, fieldName9: String, fieldName10: String, fieldName11: String, fieldName12: String, fieldName13: String): JsonStreamFormat[T] = new JsonStreamFormat[T]{
    def write(p: T, printer: JsonStreamPrinter): Unit = {
      printer.startObject()
      writeProductElement[P1](fieldName1, p, 0, printer)
      writeProductElement[P2](fieldName2, p, 1, printer)
      writeProductElement[P3](fieldName3, p, 2, printer)
      writeProductElement[P4](fieldName4, p, 3, printer)
      writeProductElement[P5](fieldName5, p, 4, printer)
      writeProductElement[P6](fieldName6, p, 5, printer)
      writeProductElement[P7](fieldName7, p, 6, printer)
      writeProductElement[P8](fieldName8, p, 7, printer)
      writeProductElement[P9](fieldName9, p, 8, printer)
      writeProductElement[P10](fieldName10, p, 9, printer)
      writeProductElement[P11](fieldName11, p, 10, printer)
      writeProductElement[P12](fieldName12, p, 11, printer)
      writeProductElement[P13](fieldName13, p, 12, printer)
      printer.endObject()
    }
    def read(parser: PullParser): T = {
      parser.startObject()
      val p1V = parser.readField[P1](fieldName1)
      val p2V = parser.readField[P2](fieldName2)
      val p3V = parser.readField[P3](fieldName3)
      val p4V = parser.readField[P4](fieldName4)
      val p5V = parser.readField[P5](fieldName5)
      val p6V = parser.readField[P6](fieldName6)
      val p7V = parser.readField[P7](fieldName7)
      val p8V = parser.readField[P8](fieldName8)
      val p9V = parser.readField[P9](fieldName9)
      val p10V = parser.readField[P10](fieldName10)
      val p11V = parser.readField[P11](fieldName11)
      val p12V = parser.readField[P12](fieldName12)
      val p13V = parser.readField[P13](fieldName13)
      parser.endObject()
      construct(readObjectValue(p1V), readObjectValue(p2V), readObjectValue(p3V), readObjectValue(p4V), readObjectValue(p5V), readObjectValue(p6V), readObjectValue(p7V), readObjectValue(p8V), readObjectValue(p9V), readObjectValue(p10V), readObjectValue(p11V), readObjectValue(p12V), readObjectValue(p13V))
    }
  }
  // Case classes with 14 parameters

  def jsonStreamFormat14[P1 :JSF, P2 :JSF, P3 :JSF, P4 :JSF, P5 :JSF, P6 :JSF, P7 :JSF, P8 :JSF, P9 :JSF, P10 :JSF, P11 :JSF, P12 :JSF, P13 :JSF, P14 :JSF, T <: Product :ClassTag](construct: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14) => T): JsonStreamFormat[T] = {
    val Array(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14) = ProductFormats.extractFieldNames(classTag[T])
    jsonStreamFormat(construct, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14)
  }
  def jsonStreamFormat[P1 :JSF, P2 :JSF, P3 :JSF, P4 :JSF, P5 :JSF, P6 :JSF, P7 :JSF, P8 :JSF, P9 :JSF, P10 :JSF, P11 :JSF, P12 :JSF, P13 :JSF, P14 :JSF, T <: Product](construct: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14) => T, fieldName1: String, fieldName2: String, fieldName3: String, fieldName4: String, fieldName5: String, fieldName6: String, fieldName7: String, fieldName8: String, fieldName9: String, fieldName10: String, fieldName11: String, fieldName12: String, fieldName13: String, fieldName14: String): JsonStreamFormat[T] = new JsonStreamFormat[T]{
    def write(p: T, printer: JsonStreamPrinter): Unit = {
      printer.startObject()
      writeProductElement[P1](fieldName1, p, 0, printer)
      writeProductElement[P2](fieldName2, p, 1, printer)
      writeProductElement[P3](fieldName3, p, 2, printer)
      writeProductElement[P4](fieldName4, p, 3, printer)
      writeProductElement[P5](fieldName5, p, 4, printer)
      writeProductElement[P6](fieldName6, p, 5, printer)
      writeProductElement[P7](fieldName7, p, 6, printer)
      writeProductElement[P8](fieldName8, p, 7, printer)
      writeProductElement[P9](fieldName9, p, 8, printer)
      writeProductElement[P10](fieldName10, p, 9, printer)
      writeProductElement[P11](fieldName11, p, 10, printer)
      writeProductElement[P12](fieldName12, p, 11, printer)
      writeProductElement[P13](fieldName13, p, 12, printer)
      writeProductElement[P14](fieldName14, p, 13, printer)
      printer.endObject()
    }
    def read(parser: PullParser): T = {
      parser.startObject()
      val p1V = parser.readField[P1](fieldName1)
      val p2V = parser.readField[P2](fieldName2)
      val p3V = parser.readField[P3](fieldName3)
      val p4V = parser.readField[P4](fieldName4)
      val p5V = parser.readField[P5](fieldName5)
      val p6V = parser.readField[P6](fieldName6)
      val p7V = parser.readField[P7](fieldName7)
      val p8V = parser.readField[P8](fieldName8)
      val p9V = parser.readField[P9](fieldName9)
      val p10V = parser.readField[P10](fieldName10)
      val p11V = parser.readField[P11](fieldName11)
      val p12V = parser.readField[P12](fieldName12)
      val p13V = parser.readField[P13](fieldName13)
      val p14V = parser.readField[P14](fieldName14)
      parser.endObject()
      construct(readObjectValue(p1V), readObjectValue(p2V), readObjectValue(p3V), readObjectValue(p4V), readObjectValue(p5V), readObjectValue(p6V), readObjectValue(p7V), readObjectValue(p8V), readObjectValue(p9V), readObjectValue(p10V), readObjectValue(p11V), readObjectValue(p12V), readObjectValue(p13V), readObjectValue(p14V))
    }
  }
  // Case classes with 15 parameters

  def jsonStreamFormat15[P1 :JSF, P2 :JSF, P3 :JSF, P4 :JSF, P5 :JSF, P6 :JSF, P7 :JSF, P8 :JSF, P9 :JSF, P10 :JSF, P11 :JSF, P12 :JSF, P13 :JSF, P14 :JSF, P15 :JSF, T <: Product :ClassTag](construct: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15) => T): JsonStreamFormat[T] = {
    val Array(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15) = ProductFormats.extractFieldNames(classTag[T])
    jsonStreamFormat(construct, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15)
  }
  def jsonStreamFormat[P1 :JSF, P2 :JSF, P3 :JSF, P4 :JSF, P5 :JSF, P6 :JSF, P7 :JSF, P8 :JSF, P9 :JSF, P10 :JSF, P11 :JSF, P12 :JSF, P13 :JSF, P14 :JSF, P15 :JSF, T <: Product](construct: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15) => T, fieldName1: String, fieldName2: String, fieldName3: String, fieldName4: String, fieldName5: String, fieldName6: String, fieldName7: String, fieldName8: String, fieldName9: String, fieldName10: String, fieldName11: String, fieldName12: String, fieldName13: String, fieldName14: String, fieldName15: String): JsonStreamFormat[T] = new JsonStreamFormat[T]{
    def write(p: T, printer: JsonStreamPrinter): Unit = {
      printer.startObject()
      writeProductElement[P1](fieldName1, p, 0, printer)
      writeProductElement[P2](fieldName2, p, 1, printer)
      writeProductElement[P3](fieldName3, p, 2, printer)
      writeProductElement[P4](fieldName4, p, 3, printer)
      writeProductElement[P5](fieldName5, p, 4, printer)
      writeProductElement[P6](fieldName6, p, 5, printer)
      writeProductElement[P7](fieldName7, p, 6, printer)
      writeProductElement[P8](fieldName8, p, 7, printer)
      writeProductElement[P9](fieldName9, p, 8, printer)
      writeProductElement[P10](fieldName10, p, 9, printer)
      writeProductElement[P11](fieldName11, p, 10, printer)
      writeProductElement[P12](fieldName12, p, 11, printer)
      writeProductElement[P13](fieldName13, p, 12, printer)
      writeProductElement[P14](fieldName14, p, 13, printer)
      writeProductElement[P15](fieldName15, p, 14, printer)
      printer.endObject()
    }
    def read(parser: PullParser): T = {
      parser.startObject()
      val p1V = parser.readField[P1](fieldName1)
      val p2V = parser.readField[P2](fieldName2)
      val p3V = parser.readField[P3](fieldName3)
      val p4V = parser.readField[P4](fieldName4)
      val p5V = parser.readField[P5](fieldName5)
      val p6V = parser.readField[P6](fieldName6)
      val p7V = parser.readField[P7](fieldName7)
      val p8V = parser.readField[P8](fieldName8)
      val p9V = parser.readField[P9](fieldName9)
      val p10V = parser.readField[P10](fieldName10)
      val p11V = parser.readField[P11](fieldName11)
      val p12V = parser.readField[P12](fieldName12)
      val p13V = parser.readField[P13](fieldName13)
      val p14V = parser.readField[P14](fieldName14)
      val p15V = parser.readField[P15](fieldName15)
      parser.endObject()
      construct(readObjectValue(p1V), readObjectValue(p2V), readObjectValue(p3V), readObjectValue(p4V), readObjectValue(p5V), readObjectValue(p6V), readObjectValue(p7V), readObjectValue(p8V), readObjectValue(p9V), readObjectValue(p10V), readObjectValue(p11V), readObjectValue(p12V), readObjectValue(p13V), readObjectValue(p14V), readObjectValue(p15V))
    }
  }
  // Case classes with 16 parameters

  def jsonStreamFormat16[P1 :JSF, P2 :JSF, P3 :JSF, P4 :JSF, P5 :JSF, P6 :JSF, P7 :JSF, P8 :JSF, P9 :JSF, P10 :JSF, P11 :JSF, P12 :JSF, P13 :JSF, P14 :JSF, P15 :JSF, P16 :JSF, T <: Product :ClassTag](construct: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16) => T): JsonStreamFormat[T] = {
    val Array(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16) = ProductFormats.extractFieldNames(classTag[T])
    jsonStreamFormat(construct, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16)
  }
  def jsonStreamFormat[P1 :JSF, P2 :JSF, P3 :JSF, P4 :JSF, P5 :JSF, P6 :JSF, P7 :JSF, P8 :JSF, P9 :JSF, P10 :JSF, P11 :JSF, P12 :JSF, P13 :JSF, P14 :JSF, P15 :JSF, P16 :JSF, T <: Product](construct: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16) => T, fieldName1: String, fieldName2: String, fieldName3: String, fieldName4: String, fieldName5: String, fieldName6: String, fieldName7: String, fieldName8: String, fieldName9: String, fieldName10: String, fieldName11: String, fieldName12: String, fieldName13: String, fieldName14: String, fieldName15: String, fieldName16: String): JsonStreamFormat[T] = new JsonStreamFormat[T]{
    def write(p: T, printer: JsonStreamPrinter): Unit = {
      printer.startObject()
      writeProductElement[P1](fieldName1, p, 0, printer)
      writeProductElement[P2](fieldName2, p, 1, printer)
      writeProductElement[P3](fieldName3, p, 2, printer)
      writeProductElement[P4](fieldName4, p, 3, printer)
      writeProductElement[P5](fieldName5, p, 4, printer)
      writeProductElement[P6](fieldName6, p, 5, printer)
      writeProductElement[P7](fieldName7, p, 6, printer)
      writeProductElement[P8](fieldName8, p, 7, printer)
      writeProductElement[P9](fieldName9, p, 8, printer)
      writeProductElement[P10](fieldName10, p, 9, printer)
      writeProductElement[P11](fieldName11, p, 10, printer)
      writeProductElement[P12](fieldName12, p, 11, printer)
      writeProductElement[P13](fieldName13, p, 12, printer)
      writeProductElement[P14](fieldName14, p, 13, printer)
      writeProductElement[P15](fieldName15, p, 14, printer)
      writeProductElement[P16](fieldName16, p, 15, printer)
      printer.endObject()
    }
    def read(parser: PullParser): T = {
      parser.startObject()
      val p1V = parser.readField[P1](fieldName1)
      val p2V = parser.readField[P2](fieldName2)
      val p3V = parser.readField[P3](fieldName3)
      val p4V = parser.readField[P4](fieldName4)
      val p5V = parser.readField[P5](fieldName5)
      val p6V = parser.readField[P6](fieldName6)
      val p7V = parser.readField[P7](fieldName7)
      val p8V = parser.readField[P8](fieldName8)
      val p9V = parser.readField[P9](fieldName9)
      val p10V = parser.readField[P10](fieldName10)
      val p11V = parser.readField[P11](fieldName11)
      val p12V = parser.readField[P12](fieldName12)
      val p13V = parser.readField[P13](fieldName13)
      val p14V = parser.readField[P14](fieldName14)
      val p15V = parser.readField[P15](fieldName15)
      val p16V = parser.readField[P16](fieldName16)
      parser.endObject()
      construct(readObjectValue(p1V), readObjectValue(p2V), readObjectValue(p3V), readObjectValue(p4V), readObjectValue(p5V), readObjectValue(p6V), readObjectValue(p7V), readObjectValue(p8V), readObjectValue(p9V), readObjectValue(p10V), readObjectValue(p11V), readObjectValue(p12V), readObjectValue(p13V), readObjectValue(p14V), readObjectValue(p15V), readObjectValue(p16V))
    }
  }
  // Case classes with 17 parameters

  def jsonStreamFormat17[P1 :JSF, P2 :JSF, P3 :JSF, P4 :JSF, P5 :JSF, P6 :JSF, P7 :JSF, P8 :JSF, P9 :JSF, P10 :JSF, P11 :JSF, P12 :JSF, P13 :JSF, P14 :JSF, P15 :JSF, P16 :JSF, P17 :JSF, T <: Product :ClassTag](construct: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17) => T): JsonStreamFormat[T] = {
    val Array(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17) = ProductFormats.extractFieldNames(classTag[T])
    jsonStreamFormat(construct, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17)
  }
  def jsonStreamFormat[P1 :JSF, P2 :JSF, P3 :JSF, P4 :JSF, P5 :JSF, P6 :JSF, P7 :JSF, P8 :JSF, P9 :JSF, P10 :JSF, P11 :JSF, P12 :JSF, P13 :JSF, P14 :JSF, P15 :JSF, P16 :JSF, P17 :JSF, T <: Product](construct: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17) => T, fieldName1: String, fieldName2: String, fieldName3: String, fieldName4: String, fieldName5: String, fieldName6: String, fieldName7: String, fieldName8: String, fieldName9: String, fieldName10: String, fieldName11: String, fieldName12: String, fieldName13: String, fieldName14: String, fieldName15: String, fieldName16: String, fieldName17: String): JsonStreamFormat[T] = new JsonStreamFormat[T]{
    def write(p: T, printer: JsonStreamPrinter): Unit = {
      printer.startObject()
      writeProductElement[P1](fieldName1, p, 0, printer)
      writeProductElement[P2](fieldName2, p, 1, printer)
      writeProductElement[P3](fieldName3, p, 2, printer)
      writeProductElement[P4](fieldName4, p, 3, printer)
      writeProductElement[P5](fieldName5, p, 4, printer)
      writeProductElement[P6](fieldName6, p, 5, printer)
      writeProductElement[P7](fieldName7, p, 6, printer)
      writeProductElement[P8](fieldName8, p, 7, printer)
      writeProductElement[P9](fieldName9, p, 8, printer)
      writeProductElement[P10](fieldName10, p, 9, printer)
      writeProductElement[P11](fieldName11, p, 10, printer)
      writeProductElement[P12](fieldName12, p, 11, printer)
      writeProductElement[P13](fieldName13, p, 12, printer)
      writeProductElement[P14](fieldName14, p, 13, printer)
      writeProductElement[P15](fieldName15, p, 14, printer)
      writeProductElement[P16](fieldName16, p, 15, printer)
      writeProductElement[P17](fieldName17, p, 16, printer)
      printer.endObject()
    }
    def read(parser: PullParser): T = {
      parser.startObject()
      val p1V = parser.readField[P1](fieldName1)
      val p2V = parser.readField[P2](fieldName2)
      val p3V = parser.readField[P3](fieldName3)
      val p4V = parser.readField[P4](fieldName4)
      val p5V = parser.readField[P5](fieldName5)
      val p6V = parser.readField[P6](fieldName6)
      val p7V = parser.readField[P7](fieldName7)
      val p8V = parser.readField[P8](fieldName8)
      val p9V = parser.readField[P9](fieldName9)
      val p10V = parser.readField[P10](fieldName10)
      val p11V = parser.readField[P11](fieldName11)
      val p12V = parser.readField[P12](fieldName12)
      val p13V = parser.readField[P13](fieldName13)
      val p14V = parser.readField[P14](fieldName14)
      val p15V = parser.readField[P15](fieldName15)
      val p16V = parser.readField[P16](fieldName16)
      val p17V = parser.readField[P17](fieldName17)
      parser.endObject()
      construct(readObjectValue(p1V), readObjectValue(p2V), readObjectValue(p3V), readObjectValue(p4V), readObjectValue(p5V), readObjectValue(p6V), readObjectValue(p7V), readObjectValue(p8V), readObjectValue(p9V), readObjectValue(p10V), readObjectValue(p11V), readObjectValue(p12V), readObjectValue(p13V), readObjectValue(p14V), readObjectValue(p15V), readObjectValue(p16V), readObjectValue(p17V))
    }
  }
  // Case classes with 18 parameters

  def jsonStreamFormat18[P1 :JSF, P2 :JSF, P3 :JSF, P4 :JSF, P5 :JSF, P6 :JSF, P7 :JSF, P8 :JSF, P9 :JSF, P10 :JSF, P11 :JSF, P12 :JSF, P13 :JSF, P14 :JSF, P15 :JSF, P16 :JSF, P17 :JSF, P18 :JSF, T <: Product :ClassTag](construct: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18) => T): JsonStreamFormat[T] = {
    val Array(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18) = ProductFormats.extractFieldNames(classTag[T])
    jsonStreamFormat(construct, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18)
  }
  def jsonStreamFormat[P1 :JSF, P2 :JSF, P3 :JSF, P4 :JSF, P5 :JSF, P6 :JSF, P7 :JSF, P8 :JSF, P9 :JSF, P10 :JSF, P11 :JSF, P12 :JSF, P13 :JSF, P14 :JSF, P15 :JSF, P16 :JSF, P17 :JSF, P18 :JSF, T <: Product](construct: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18) => T, fieldName1: String, fieldName2: String, fieldName3: String, fieldName4: String, fieldName5: String, fieldName6: String, fieldName7: String, fieldName8: String, fieldName9: String, fieldName10: String, fieldName11: String, fieldName12: String, fieldName13: String, fieldName14: String, fieldName15: String, fieldName16: String, fieldName17: String, fieldName18: String): JsonStreamFormat[T] = new JsonStreamFormat[T]{
    def write(p: T, printer: JsonStreamPrinter): Unit = {
      printer.startObject()
      writeProductElement[P1](fieldName1, p, 0, printer)
      writeProductElement[P2](fieldName2, p, 1, printer)
      writeProductElement[P3](fieldName3, p, 2, printer)
      writeProductElement[P4](fieldName4, p, 3, printer)
      writeProductElement[P5](fieldName5, p, 4, printer)
      writeProductElement[P6](fieldName6, p, 5, printer)
      writeProductElement[P7](fieldName7, p, 6, printer)
      writeProductElement[P8](fieldName8, p, 7, printer)
      writeProductElement[P9](fieldName9, p, 8, printer)
      writeProductElement[P10](fieldName10, p, 9, printer)
      writeProductElement[P11](fieldName11, p, 10, printer)
      writeProductElement[P12](fieldName12, p, 11, printer)
      writeProductElement[P13](fieldName13, p, 12, printer)
      writeProductElement[P14](fieldName14, p, 13, printer)
      writeProductElement[P15](fieldName15, p, 14, printer)
      writeProductElement[P16](fieldName16, p, 15, printer)
      writeProductElement[P17](fieldName17, p, 16, printer)
      writeProductElement[P18](fieldName18, p, 17, printer)
      printer.endObject()
    }
    def read(parser: PullParser): T = {
      parser.startObject()
      val p1V = parser.readField[P1](fieldName1)
      val p2V = parser.readField[P2](fieldName2)
      val p3V = parser.readField[P3](fieldName3)
      val p4V = parser.readField[P4](fieldName4)
      val p5V = parser.readField[P5](fieldName5)
      val p6V = parser.readField[P6](fieldName6)
      val p7V = parser.readField[P7](fieldName7)
      val p8V = parser.readField[P8](fieldName8)
      val p9V = parser.readField[P9](fieldName9)
      val p10V = parser.readField[P10](fieldName10)
      val p11V = parser.readField[P11](fieldName11)
      val p12V = parser.readField[P12](fieldName12)
      val p13V = parser.readField[P13](fieldName13)
      val p14V = parser.readField[P14](fieldName14)
      val p15V = parser.readField[P15](fieldName15)
      val p16V = parser.readField[P16](fieldName16)
      val p17V = parser.readField[P17](fieldName17)
      val p18V = parser.readField[P18](fieldName18)
      parser.endObject()
      construct(readObjectValue(p1V), readObjectValue(p2V), readObjectValue(p3V), readObjectValue(p4V), readObjectValue(p5V), readObjectValue(p6V), readObjectValue(p7V), readObjectValue(p8V), readObjectValue(p9V), readObjectValue(p10V), readObjectValue(p11V), readObjectValue(p12V), readObjectValue(p13V), readObjectValue(p14V), readObjectValue(p15V), readObjectValue(p16V), readObjectValue(p17V), readObjectValue(p18V))
    }
  }
  // Case classes with 19 parameters

  def jsonStreamFormat19[P1 :JSF, P2 :JSF, P3 :JSF, P4 :JSF, P5 :JSF, P6 :JSF, P7 :JSF, P8 :JSF, P9 :JSF, P10 :JSF, P11 :JSF, P12 :JSF, P13 :JSF, P14 :JSF, P15 :JSF, P16 :JSF, P17 :JSF, P18 :JSF, P19 :JSF, T <: Product :ClassTag](construct: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19) => T): JsonStreamFormat[T] = {
    val Array(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19) = ProductFormats.extractFieldNames(classTag[T])
    jsonStreamFormat(construct, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19)
  }
  def jsonStreamFormat[P1 :JSF, P2 :JSF, P3 :JSF, P4 :JSF, P5 :JSF, P6 :JSF, P7 :JSF, P8 :JSF, P9 :JSF, P10 :JSF, P11 :JSF, P12 :JSF, P13 :JSF, P14 :JSF, P15 :JSF, P16 :JSF, P17 :JSF, P18 :JSF, P19 :JSF, T <: Product](construct: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19) => T, fieldName1: String, fieldName2: String, fieldName3: String, fieldName4: String, fieldName5: String, fieldName6: String, fieldName7: String, fieldName8: String, fieldName9: String, fieldName10: String, fieldName11: String, fieldName12: String, fieldName13: String, fieldName14: String, fieldName15: String, fieldName16: String, fieldName17: String, fieldName18: String, fieldName19: String): JsonStreamFormat[T] = new JsonStreamFormat[T]{
    def write(p: T, printer: JsonStreamPrinter): Unit = {
      printer.startObject()
      writeProductElement[P1](fieldName1, p, 0, printer)
      writeProductElement[P2](fieldName2, p, 1, printer)
      writeProductElement[P3](fieldName3, p, 2, printer)
      writeProductElement[P4](fieldName4, p, 3, printer)
      writeProductElement[P5](fieldName5, p, 4, printer)
      writeProductElement[P6](fieldName6, p, 5, printer)
      writeProductElement[P7](fieldName7, p, 6, printer)
      writeProductElement[P8](fieldName8, p, 7, printer)
      writeProductElement[P9](fieldName9, p, 8, printer)
      writeProductElement[P10](fieldName10, p, 9, printer)
      writeProductElement[P11](fieldName11, p, 10, printer)
      writeProductElement[P12](fieldName12, p, 11, printer)
      writeProductElement[P13](fieldName13, p, 12, printer)
      writeProductElement[P14](fieldName14, p, 13, printer)
      writeProductElement[P15](fieldName15, p, 14, printer)
      writeProductElement[P16](fieldName16, p, 15, printer)
      writeProductElement[P17](fieldName17, p, 16, printer)
      writeProductElement[P18](fieldName18, p, 17, printer)
      writeProductElement[P19](fieldName19, p, 18, printer)
      printer.endObject()
    }
    def read(parser: PullParser): T = {
      parser.startObject()
      val p1V = parser.readField[P1](fieldName1)
      val p2V = parser.readField[P2](fieldName2)
      val p3V = parser.readField[P3](fieldName3)
      val p4V = parser.readField[P4](fieldName4)
      val p5V = parser.readField[P5](fieldName5)
      val p6V = parser.readField[P6](fieldName6)
      val p7V = parser.readField[P7](fieldName7)
      val p8V = parser.readField[P8](fieldName8)
      val p9V = parser.readField[P9](fieldName9)
      val p10V = parser.readField[P10](fieldName10)
      val p11V = parser.readField[P11](fieldName11)
      val p12V = parser.readField[P12](fieldName12)
      val p13V = parser.readField[P13](fieldName13)
      val p14V = parser.readField[P14](fieldName14)
      val p15V = parser.readField[P15](fieldName15)
      val p16V = parser.readField[P16](fieldName16)
      val p17V = parser.readField[P17](fieldName17)
      val p18V = parser.readField[P18](fieldName18)
      val p19V = parser.readField[P19](fieldName19)
      parser.endObject()
      construct(readObjectValue(p1V), readObjectValue(p2V), readObjectValue(p3V), readObjectValue(p4V), readObjectValue(p5V), readObjectValue(p6V), readObjectValue(p7V), readObjectValue(p8V), readObjectValue(p9V), readObjectValue(p10V), readObjectValue(p11V), readObjectValue(p12V), readObjectValue(p13V), readObjectValue(p14V), readObjectValue(p15V), readObjectValue(p16V), readObjectValue(p17V), readObjectValue(p18V), readObjectValue(p19V))
    }
  }
  // Case classes with 20 parameters

  def jsonStreamFormat20[P1 :JSF, P2 :JSF, P3 :JSF, P4 :JSF, P5 :JSF, P6 :JSF, P7 :JSF, P8 :JSF, P9 :JSF, P10 :JSF, P11 :JSF, P12 :JSF, P13 :JSF, P14 :JSF, P15 :JSF, P16 :JSF, P17 :JSF, P18 :JSF, P19 :JSF, P20 :JSF, T <: Product :ClassTag](construct: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20) => T): JsonStreamFormat[T] = {
    val Array(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20) = ProductFormats.extractFieldNames(classTag[T])
    jsonStreamFormat(construct, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20)
  }
  def jsonStreamFormat[P1 :JSF, P2 :JSF, P3 :JSF, P4 :JSF, P5 :JSF, P6 :JSF, P7 :JSF, P8 :JSF, P9 :JSF, P10 :JSF, P11 :JSF, P12 :JSF, P13 :JSF, P14 :JSF, P15 :JSF, P16 :JSF, P17 :JSF, P18 :JSF, P19 :JSF, P20 :JSF, T <: Product](construct: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20) => T, fieldName1: String, fieldName2: String, fieldName3: String, fieldName4: String, fieldName5: String, fieldName6: String, fieldName7: String, fieldName8: String, fieldName9: String, fieldName10: String, fieldName11: String, fieldName12: String, fieldName13: String, fieldName14: String, fieldName15: String, fieldName16: String, fieldName17: String, fieldName18: String, fieldName19: String, fieldName20: String): JsonStreamFormat[T] = new JsonStreamFormat[T]{
    def write(p: T, printer: JsonStreamPrinter): Unit = {
      printer.startObject()
      writeProductElement[P1](fieldName1, p, 0, printer)
      writeProductElement[P2](fieldName2, p, 1, printer)
      writeProductElement[P3](fieldName3, p, 2, printer)
      writeProductElement[P4](fieldName4, p, 3, printer)
      writeProductElement[P5](fieldName5, p, 4, printer)
      writeProductElement[P6](fieldName6, p, 5, printer)
      writeProductElement[P7](fieldName7, p, 6, printer)
      writeProductElement[P8](fieldName8, p, 7, printer)
      writeProductElement[P9](fieldName9, p, 8, printer)
      writeProductElement[P10](fieldName10, p, 9, printer)
      writeProductElement[P11](fieldName11, p, 10, printer)
      writeProductElement[P12](fieldName12, p, 11, printer)
      writeProductElement[P13](fieldName13, p, 12, printer)
      writeProductElement[P14](fieldName14, p, 13, printer)
      writeProductElement[P15](fieldName15, p, 14, printer)
      writeProductElement[P16](fieldName16, p, 15, printer)
      writeProductElement[P17](fieldName17, p, 16, printer)
      writeProductElement[P18](fieldName18, p, 17, printer)
      writeProductElement[P19](fieldName19, p, 18, printer)
      writeProductElement[P20](fieldName20, p, 19, printer)
      printer.endObject()
    }
    def read(parser: PullParser): T = {
      parser.startObject()
      val p1V = parser.readField[P1](fieldName1)
      val p2V = parser.readField[P2](fieldName2)
      val p3V = parser.readField[P3](fieldName3)
      val p4V = parser.readField[P4](fieldName4)
      val p5V = parser.readField[P5](fieldName5)
      val p6V = parser.readField[P6](fieldName6)
      val p7V = parser.readField[P7](fieldName7)
      val p8V = parser.readField[P8](fieldName8)
      val p9V = parser.readField[P9](fieldName9)
      val p10V = parser.readField[P10](fieldName10)
      val p11V = parser.readField[P11](fieldName11)
      val p12V = parser.readField[P12](fieldName12)
      val p13V = parser.readField[P13](fieldName13)
      val p14V = parser.readField[P14](fieldName14)
      val p15V = parser.readField[P15](fieldName15)
      val p16V = parser.readField[P16](fieldName16)
      val p17V = parser.readField[P17](fieldName17)
      val p18V = parser.readField[P18](fieldName18)
      val p19V = parser.readField[P19](fieldName19)
      val p20V = parser.readField[P20](fieldName20)
      parser.endObject()
      construct(readObjectValue(p1V), readObjectValue(p2V), readObjectValue(p3V), readObjectValue(p4V), readObjectValue(p5V), readObjectValue(p6V), readObjectValue(p7V), readObjectValue(p8V), readObjectValue(p9V), readObjectValue(p10V), readObjectValue(p11V), readObjectValue(p12V), readObjectValue(p13V), readObjectValue(p14V), readObjectValue(p15V), readObjectValue(p16V), readObjectValue(p17V), readObjectValue(p18V), readObjectValue(p19V), readObjectValue(p20V))
    }
  }
  // Case classes with 21 parameters

  def jsonStreamFormat21[P1 :JSF, P2 :JSF, P3 :JSF, P4 :JSF, P5 :JSF, P6 :JSF, P7 :JSF, P8 :JSF, P9 :JSF, P10 :JSF, P11 :JSF, P12 :JSF, P13 :JSF, P14 :JSF, P15 :JSF, P16 :JSF, P17 :JSF, P18 :JSF, P19 :JSF, P20 :JSF, P21 :JSF, T <: Product :ClassTag](construct: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20, P21) => T): JsonStreamFormat[T] = {
    val Array(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20, p21) = ProductFormats.extractFieldNames(classTag[T])
    jsonStreamFormat(construct, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20, p21)
  }
  def jsonStreamFormat[P1 :JSF, P2 :JSF, P3 :JSF, P4 :JSF, P5 :JSF, P6 :JSF, P7 :JSF, P8 :JSF, P9 :JSF, P10 :JSF, P11 :JSF, P12 :JSF, P13 :JSF, P14 :JSF, P15 :JSF, P16 :JSF, P17 :JSF, P18 :JSF, P19 :JSF, P20 :JSF, P21 :JSF, T <: Product](construct: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20, P21) => T, fieldName1: String, fieldName2: String, fieldName3: String, fieldName4: String, fieldName5: String, fieldName6: String, fieldName7: String, fieldName8: String, fieldName9: String, fieldName10: String, fieldName11: String, fieldName12: String, fieldName13: String, fieldName14: String, fieldName15: String, fieldName16: String, fieldName17: String, fieldName18: String, fieldName19: String, fieldName20: String, fieldName21: String): JsonStreamFormat[T] = new JsonStreamFormat[T]{
    def write(p: T, printer: JsonStreamPrinter): Unit = {
      printer.startObject()
      writeProductElement[P1](fieldName1, p, 0, printer)
      writeProductElement[P2](fieldName2, p, 1, printer)
      writeProductElement[P3](fieldName3, p, 2, printer)
      writeProductElement[P4](fieldName4, p, 3, printer)
      writeProductElement[P5](fieldName5, p, 4, printer)
      writeProductElement[P6](fieldName6, p, 5, printer)
      writeProductElement[P7](fieldName7, p, 6, printer)
      writeProductElement[P8](fieldName8, p, 7, printer)
      writeProductElement[P9](fieldName9, p, 8, printer)
      writeProductElement[P10](fieldName10, p, 9, printer)
      writeProductElement[P11](fieldName11, p, 10, printer)
      writeProductElement[P12](fieldName12, p, 11, printer)
      writeProductElement[P13](fieldName13, p, 12, printer)
      writeProductElement[P14](fieldName14, p, 13, printer)
      writeProductElement[P15](fieldName15, p, 14, printer)
      writeProductElement[P16](fieldName16, p, 15, printer)
      writeProductElement[P17](fieldName17, p, 16, printer)
      writeProductElement[P18](fieldName18, p, 17, printer)
      writeProductElement[P19](fieldName19, p, 18, printer)
      writeProductElement[P20](fieldName20, p, 19, printer)
      writeProductElement[P21](fieldName21, p, 20, printer)
      printer.endObject()
    }
    def read(parser: PullParser): T = {
      parser.startObject()
      val p1V = parser.readField[P1](fieldName1)
      val p2V = parser.readField[P2](fieldName2)
      val p3V = parser.readField[P3](fieldName3)
      val p4V = parser.readField[P4](fieldName4)
      val p5V = parser.readField[P5](fieldName5)
      val p6V = parser.readField[P6](fieldName6)
      val p7V = parser.readField[P7](fieldName7)
      val p8V = parser.readField[P8](fieldName8)
      val p9V = parser.readField[P9](fieldName9)
      val p10V = parser.readField[P10](fieldName10)
      val p11V = parser.readField[P11](fieldName11)
      val p12V = parser.readField[P12](fieldName12)
      val p13V = parser.readField[P13](fieldName13)
      val p14V = parser.readField[P14](fieldName14)
      val p15V = parser.readField[P15](fieldName15)
      val p16V = parser.readField[P16](fieldName16)
      val p17V = parser.readField[P17](fieldName17)
      val p18V = parser.readField[P18](fieldName18)
      val p19V = parser.readField[P19](fieldName19)
      val p20V = parser.readField[P20](fieldName20)
      val p21V = parser.readField[P21](fieldName21)
      parser.endObject()
      construct(readObjectValue(p1V), readObjectValue(p2V), readObjectValue(p3V), readObjectValue(p4V), readObjectValue(p5V), readObjectValue(p6V), readObjectValue(p7V), readObjectValue(p8V), readObjectValue(p9V), readObjectValue(p10V), readObjectValue(p11V), readObjectValue(p12V), readObjectValue(p13V), readObjectValue(p14V), readObjectValue(p15V), readObjectValue(p16V), readObjectValue(p17V), readObjectValue(p18V), readObjectValue(p19V), readObjectValue(p20V), readObjectValue(p21V))
    }
  }
  // Case classes with 22 parameters

  def jsonStreamFormat22[P1 :JSF, P2 :JSF, P3 :JSF, P4 :JSF, P5 :JSF, P6 :JSF, P7 :JSF, P8 :JSF, P9 :JSF, P10 :JSF, P11 :JSF, P12 :JSF, P13 :JSF, P14 :JSF, P15 :JSF, P16 :JSF, P17 :JSF, P18 :JSF, P19 :JSF, P20 :JSF, P21 :JSF, P22 :JSF, T <: Product :ClassTag](construct: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20, P21, P22) => T): JsonStreamFormat[T] = {
    val Array(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20, p21, p22) = ProductFormats.extractFieldNames(classTag[T])
    jsonStreamFormat(construct, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20, p21, p22)
  }
  def jsonStreamFormat[P1 :JSF, P2 :JSF, P3 :JSF, P4 :JSF, P5 :JSF, P6 :JSF, P7 :JSF, P8 :JSF, P9 :JSF, P10 :JSF, P11 :JSF, P12 :JSF, P13 :JSF, P14 :JSF, P15 :JSF, P16 :JSF, P17 :JSF, P18 :JSF, P19 :JSF, P20 :JSF, P21 :JSF, P22 :JSF, T <: Product](construct: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20, P21, P22) => T, fieldName1: String, fieldName2: String, fieldName3: String, fieldName4: String, fieldName5: String, fieldName6: String, fieldName7: String, fieldName8: String, fieldName9: String, fieldName10: String, fieldName11: String, fieldName12: String, fieldName13: String, fieldName14: String, fieldName15: String, fieldName16: String, fieldName17: String, fieldName18: String, fieldName19: String, fieldName20: String, fieldName21: String, fieldName22: String): JsonStreamFormat[T] = new JsonStreamFormat[T]{
    def write(p: T, printer: JsonStreamPrinter): Unit = {
      printer.startObject()
      writeProductElement[P1](fieldName1, p, 0, printer)
      writeProductElement[P2](fieldName2, p, 1, printer)
      writeProductElement[P3](fieldName3, p, 2, printer)
      writeProductElement[P4](fieldName4, p, 3, printer)
      writeProductElement[P5](fieldName5, p, 4, printer)
      writeProductElement[P6](fieldName6, p, 5, printer)
      writeProductElement[P7](fieldName7, p, 6, printer)
      writeProductElement[P8](fieldName8, p, 7, printer)
      writeProductElement[P9](fieldName9, p, 8, printer)
      writeProductElement[P10](fieldName10, p, 9, printer)
      writeProductElement[P11](fieldName11, p, 10, printer)
      writeProductElement[P12](fieldName12, p, 11, printer)
      writeProductElement[P13](fieldName13, p, 12, printer)
      writeProductElement[P14](fieldName14, p, 13, printer)
      writeProductElement[P15](fieldName15, p, 14, printer)
      writeProductElement[P16](fieldName16, p, 15, printer)
      writeProductElement[P17](fieldName17, p, 16, printer)
      writeProductElement[P18](fieldName18, p, 17, printer)
      writeProductElement[P19](fieldName19, p, 18, printer)
      writeProductElement[P20](fieldName20, p, 19, printer)
      writeProductElement[P21](fieldName21, p, 20, printer)
      writeProductElement[P22](fieldName22, p, 21, printer)
      printer.endObject()
    }
    def read(parser: PullParser): T = {
      parser.startObject()
      val p1V = parser.readField[P1](fieldName1)
      val p2V = parser.readField[P2](fieldName2)
      val p3V = parser.readField[P3](fieldName3)
      val p4V = parser.readField[P4](fieldName4)
      val p5V = parser.readField[P5](fieldName5)
      val p6V = parser.readField[P6](fieldName6)
      val p7V = parser.readField[P7](fieldName7)
      val p8V = parser.readField[P8](fieldName8)
      val p9V = parser.readField[P9](fieldName9)
      val p10V = parser.readField[P10](fieldName10)
      val p11V = parser.readField[P11](fieldName11)
      val p12V = parser.readField[P12](fieldName12)
      val p13V = parser.readField[P13](fieldName13)
      val p14V = parser.readField[P14](fieldName14)
      val p15V = parser.readField[P15](fieldName15)
      val p16V = parser.readField[P16](fieldName16)
      val p17V = parser.readField[P17](fieldName17)
      val p18V = parser.readField[P18](fieldName18)
      val p19V = parser.readField[P19](fieldName19)
      val p20V = parser.readField[P20](fieldName20)
      val p21V = parser.readField[P21](fieldName21)
      val p22V = parser.readField[P22](fieldName22)
      parser.endObject()
      construct(readObjectValue(p1V), readObjectValue(p2V), readObjectValue(p3V), readObjectValue(p4V), readObjectValue(p5V), readObjectValue(p6V), readObjectValue(p7V), readObjectValue(p8V), readObjectValue(p9V), readObjectValue(p10V), readObjectValue(p11V), readObjectValue(p12V), readObjectValue(p13V), readObjectValue(p14V), readObjectValue(p15V), readObjectValue(p16V), readObjectValue(p17V), readObjectValue(p18V), readObjectValue(p19V), readObjectValue(p20V), readObjectValue(p21V), readObjectValue(p22V))
    }
  }
}

/** This trait supplies an alternative rendering mode for optional case class members.  Normally
  * optional members that are undefined (`None`) are not rendered at all.  By mixing in this trait
  * into your custom JsonProtocol you can enforce the rendering of undefined members as `null`.
  * (Note that this only affect JSON writing, spray-json will always read missing optional members
  * as well as `null` optional members as `None`.)
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
