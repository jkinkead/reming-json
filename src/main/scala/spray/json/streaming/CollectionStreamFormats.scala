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
