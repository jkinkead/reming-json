/*
 * Copyright (C) 2015 by Jesse Kinkead
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

/** Class to hold values that were requested from an object. Uninitialized until the parser advances
  * past the specified obeject key.
  * @param key the object key this was stored in
  */
private[reming] class ObjectValue[T](val key: String, handler: JsonReader[T]) {
  /** Set when the parse completes. Will be Some(Some(T)) if the key was present in the object,
    * Some(None) if it was missing.
    */
  private var parsedValue: Option[Option[T]] = None

  /** @throws DeserializationException if called before parsedValue is set */
  private def getParsedValue: Option[T] = parsedValue getOrElse {
    deserializationError("Usage error: ParsedVal dereferenced before endObject")
  }

  /** Called when the parser sees the key this was registered for. */
  private[reming] def readValue(parser: JsonParser): Unit = {
    parsedValue = Some(Some(handler.read(parser)))
  }

  /** Called at the start of parsing. This indicates that the parser read the object this was
    * registered for, but didn't (yet) see the key.
    */
  private[reming] def setDefault(): Unit = {
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
