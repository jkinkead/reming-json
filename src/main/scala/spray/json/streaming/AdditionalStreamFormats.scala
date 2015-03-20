package spray.json.streaming

import scala.util.Try

/** Provides formats and helpers. */
object AdditionalStreamFormats {
  // TODO: Add in lazy; lift and other formats, too?

  /** Wraps an existing JsonStreamReader with Exception protection. */
  def safeReader[A : JsonStreamReader] = new JsonStreamReader[Try[A]] {
    def read(parser: PullParser): Try[A] = Try(implicitly[JsonStreamReader[A]].read(parser))
  }
}
