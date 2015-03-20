package spray.json.streaming

/** Provides all the predefined JsonStreamFormats. */
trait DefaultStreamProtocol extends BasicStreamFormats with StandardStreamFormats

object DefaultStreamProtocol extends DefaultStreamProtocol
