package spray.json.streaming

import spray.json.deserializationError

/** Class to hold values that were requested from an object. Uninitialized until the parser advances
  * past the specified obeject key.
  * @param key the object key this was stored in
  */
private[streaming] class ObjectValue[T](val key: String, handler: JsonStreamReader[T]) {
  /** Set when the parse completes. Will be Some(Some(T)) if the key was present in the object,
    * Some(None) if it was missing.
    */
  private var parsedValue: Option[Option[T]] = None

  /** @throws DeserializationException if called before parsedValue is set */
  private def getParsedValue: Option[T] = parsedValue getOrElse {
    deserializationError("StreamFormat usage error: ParsedVal dereferenced before endObject")
  }

  /** Called when the parser sees the key this was registered for. */
  private[streaming] def readValue(parser: PullParser): Unit = {
    parsedValue = Some(Some(handler.read(parser)))
  }

  /** Called at the start of parsing. This indicates that the parser read the object this was
    * registered for, but didn't (yet) see the key.
    */
  private[streaming] def setDefault(): Unit = {
    parsedValue = Some(None)
  }

  /** Returns the wrapped value, throwing an exception if it's missing.
    * @throws DeserializationException if called before `endObject` was called on the parent parser,
    *     or if the value was missing from the object
    */
  def value: T = getParsedValue getOrElse {
    deserializationError(s"Object missing required field '$key'")
  }

  /** Returns the wrapped value, if it was in the object, or None otherwise.
    * @throws DeserializationException if called before `endObject` was called on the parent parser
    */
  def optionalValue: Option[T] = getParsedValue
}
