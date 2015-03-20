package spray.json.streaming

/** A JsonStreamReader interacts with a PullParser to construct an instance of a type. */
trait JsonStreamReader[T] {
  def read(parser: PullParser): T
}
object JsonStreamReader {
  /** Conversion to let any function be a JsonStreamReader. */
  implicit def func2Reader[T](f: PullParser => T): JsonStreamReader[T] = new JsonStreamReader[T] {
    override def read(parser: PullParser) = f(parser)
  }
}
