package spray.json.streaming

import scala.reflect.ClassTag

trait CollectionStreamFormats {
  implicit def arrayFormat[T : JsonStreamFormat : ClassTag] = new JsonStreamFormat[Array[T]] {
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
  implicit def mapFormat[T : JsonStreamFormat] = new JsonStreamFormat[Map[String, T]] {
    override def write(map: Map[String, T], printer: JsonStreamPrinter): Unit = {
      printer.startObject()
      for ((key, value) <- map) printer.printField(key, value)
      printer.endObject()
    }
    override def read(parser: PullParser): Map[String, T] = Map(parser.readObject.toSeq: _*)
  }

  implicit def listFormat[T : JsonStreamFormat] = viaSeq[List[T], T](seq => List(seq: _*))
  implicit def vectorFormat[T : JsonStreamFormat] = viaSeq[Vector[T], T](seq => Vector(seq: _*))

  // Base collection iterables.
  implicit def iterableFormat[T : JsonStreamFormat] =
    viaSeq[collection.Iterable[T], T](seq => Iterable(seq: _*))
  implicit def seqFormat[T : JsonStreamFormat] =
    viaSeq[collection.Seq[T], T](seq => Seq(seq: _*))
  implicit def indexedSeqFormat[T : JsonStreamFormat] = 
    viaSeq[collection.IndexedSeq[T], T](seq => IndexedSeq(seq: _*))
  implicit def linearSeqFormat[T : JsonStreamFormat]  =
    viaSeq[collection.LinearSeq[T], T](seq => collection.LinearSeq(seq :_*))
  implicit def setFormat[T : JsonStreamFormat] =
    viaSeq[collection.Set[T], T](seq => collection.Set(seq :_*))

  // Immutable collection iterables.
  import collection.{immutable => imm}

  implicit def immIterableFormat[T : JsonStreamFormat] =
    viaSeq[imm.Iterable[T], T](seq => imm.Iterable(seq :_*))
  implicit def immSeqFormat[T : JsonStreamFormat] = viaSeq[imm.Seq[T], T](seq => imm.Seq(seq :_*))
  implicit def immIndexedSeqFormat[T : JsonStreamFormat] =
    viaSeq[imm.IndexedSeq[T], T](seq => imm.IndexedSeq(seq :_*))
  implicit def immLinearSeqFormat[T : JsonStreamFormat] =
    viaSeq[imm.LinearSeq[T], T](seq => imm.LinearSeq(seq :_*))
  implicit def immSetFormat[T : JsonStreamFormat] = viaSeq[imm.Set[T], T](seq => imm.Set(seq :_*))

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
