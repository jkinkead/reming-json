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
