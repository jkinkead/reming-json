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

import scala.reflect.ClassTag

trait CollectionFormats { self: StandardFormats =>
  implicit def arrayFormat[T : JsonFormat : ClassTag] = new JsonFormat[Array[T]] {
    override def write(value: Array[T], printer: JsonPrinter): Unit = {
      printer.startArray()
      value foreach printer.printArrayItem[T]
      printer.endArray()
    }
    override def read(parser: JsonParser): Array[T] = parser.readArray.toArray
  }

  /** Serializes any map with string keys as a JS object. */
  implicit def stringMapFormat[T : JsonFormat] = new JsonFormat[Map[String, T]] {
    override def write(map: Map[String, T], printer: JsonPrinter): Unit = {
      printer.startObject()
      for ((key, value) <- map) printer.printField(key, value)
      printer.endObject()
    }
    override def read(parser: JsonParser): Map[String, T] = Map(parser.readObject.toSeq: _*)
  }

  /** Serializes any other map a JS array. */
  implicit def anyMapFormat[K : JsonFormat, V : JsonFormat] =
    viaSeq[Map[K, V], (K, V)](seq => Map(seq: _*))

  implicit def listFormat[T : JsonFormat] = viaSeq[List[T], T](seq => List(seq: _*))

  // Immutable collection iterables.
  import collection.{ immutable => imm }

  implicit def immIterableFormat[T : JsonFormat] =
    viaSeq[imm.Iterable[T], T](seq => imm.Iterable(seq :_*))
  implicit def immSeqFormat[T : JsonFormat] = viaSeq[imm.Seq[T], T](seq => imm.Seq(seq :_*))
  implicit def immIndexedSeqFormat[T : JsonFormat] =
    viaSeq[imm.IndexedSeq[T], T](seq => imm.IndexedSeq(seq :_*))
  implicit def immLinearSeqFormat[T : JsonFormat] =
    viaSeq[imm.LinearSeq[T], T](seq => imm.LinearSeq(seq :_*))
  implicit def immSetFormat[T : JsonFormat] = viaSeq[imm.Set[T], T](seq => imm.Set(seq :_*))

  implicit def vectorFormat[T : JsonFormat] = viaSeq[Vector[T], T](seq => Vector(seq: _*))

  import collection._

  // Base collection iterables.
  implicit def iterableFormat[T : JsonFormat] = viaSeq[Iterable[T], T](seq => Iterable(seq: _*))
  implicit def seqFormat[T : JsonFormat] = viaSeq[Seq[T], T](seq => Seq(seq: _*))
  implicit def indexedSeqFormat[T : JsonFormat] =
    viaSeq[IndexedSeq[T], T](seq => IndexedSeq(seq: _*))
  implicit def linearSeqFormat[T : JsonFormat]  = viaSeq[LinearSeq[T], T](seq => LinearSeq(seq :_*))
  implicit def setFormat[T : JsonFormat] = viaSeq[Set[T], T](seq => Set(seq :_*))
  implicit def sortedMapFormat[K : JsonFormat : Ordering, V : JsonFormat] =
    viaSeq[SortedMap[K, V], (K, V)](seq => SortedMap(seq: _*))

  /** Writes any iterable I of T as a JS array.
    * @param builder factory method to build an I from Seq[T]
    */
  def viaSeq[I <: Iterable[T], T : JsonFormat](builder: Seq[T] => I): JsonFormat[I] = {
    new JsonFormat[I] {
      override def write(value: I, printer: JsonPrinter): Unit = {
        printer.startArray()
        value foreach printer.printArrayItem[T]
        printer.endArray()
      }
      override def read(parser: JsonParser): I = builder(parser.readArray[T].toSeq)
    }
  }
}
