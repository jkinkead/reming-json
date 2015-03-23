package spray.json.streaming

/** Provides all the predefined JsonStreamFormats. */
trait DefaultStreamProtocol
  extends BasicStreamFormats
  with StandardStreamFormats
  with CollectionStreamFormats
  with ProductStreamFormats

object DefaultStreamProtocol extends DefaultStreamProtocol
