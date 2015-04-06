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

import scala.reflect.ClassTag

trait CollectionStreamFormats {
  implicit def arrayStreamFormat[T : JsonStreamFormat : ClassTag] = new JsonStreamFormat[Array[T]] {
    override def write(value: Array[T], printer: JsonStreamPrinter): Unit = {
      printer.startArray()
      value foreach printer.printArrayItem[T]
      printer.endArray()
    }
    override def read(parser: PullParser): Array[T] = {
      parser.readArray.toArray
    }
  }

  /** Serializes any map with string keys as as JS object. */
  implicit def mapStreamFormat[T : JsonStreamFormat] = new JsonStreamFormat[Map[String, T]] {
    override def write(map: Map[String, T], printer: JsonStreamPrinter): Unit = {
      printer.startObject()
      for ((key, value) <- map) printer.printField(key, value)
      printer.endObject()
    }
    override def read(parser: PullParser): Map[String, T] = Map(parser.readObject.toSeq: _*)
  }
  implicit def listStreamFormat[T : JsonStreamFormat] = viaSeq[List[T], T](seq => List(seq: _*))

  // Immutable collection iterables.
  import collection.{ immutable => imm }

  implicit def immIterableStreamFormat[T : JsonStreamFormat] =
    viaSeq[imm.Iterable[T], T](seq => imm.Iterable(seq :_*))
  implicit def immSeqStreamFormat[T : JsonStreamFormat] =
    viaSeq[imm.Seq[T], T](seq => imm.Seq(seq :_*))
  implicit def immIndexedSeqStreamFormat[T : JsonStreamFormat] =
    viaSeq[imm.IndexedSeq[T], T](seq => imm.IndexedSeq(seq :_*))
  implicit def immLinearSeqStreamFormat[T : JsonStreamFormat] =
    viaSeq[imm.LinearSeq[T], T](seq => imm.LinearSeq(seq :_*))
  implicit def immSetStreamFormat[T : JsonStreamFormat] =
    viaSeq[imm.Set[T], T](seq => imm.Set(seq :_*))

  implicit def vectorStreamFormat[T : JsonStreamFormat] =
    viaSeq[Vector[T], T](seq => Vector(seq: _*))

  import collection._

  // Base collection iterables.
  implicit def iterableStreamFormat[T : JsonStreamFormat] =
    viaSeq[Iterable[T], T](seq => Iterable(seq: _*))
  implicit def seqStreamFormat[T : JsonStreamFormat] =
    viaSeq[Seq[T], T](seq => Seq(seq: _*))
  implicit def indexedSeqStreamFormat[T : JsonStreamFormat] =
    viaSeq[IndexedSeq[T], T](seq => IndexedSeq(seq: _*))
  implicit def linearSeqStreamFormat[T : JsonStreamFormat]  =
    viaSeq[LinearSeq[T], T](seq => LinearSeq(seq :_*))
  implicit def setStreamFormat[T : JsonStreamFormat] =
    viaSeq[Set[T], T](seq => Set(seq :_*))

  /** Writes any iterable I of T as a JS array.
    * @param builder factory method to build an I from Seq[T]
    */
  def viaSeq[I <: Iterable[T], T : JsonStreamFormat](builder: Seq[T] => I): JsonStreamFormat[I] = {
    new JsonStreamFormat[I] {
      override def write(value: I, printer: JsonStreamPrinter): Unit = {
        printer.startArray()
        value foreach printer.printArrayItem[T]
        printer.endArray()
      }
      override def read(parser: PullParser): I = builder(parser.readArray[T].toSeq)
    }
  }
}
