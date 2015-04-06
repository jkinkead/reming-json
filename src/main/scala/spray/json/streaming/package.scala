/*
 * Original implementation (C) 2009-2011 Mathias Doenitz
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

package spray.json

package object streaming {
  def jsonStreamReader[T](implicit reader: JsonStreamReader[T]) = reader
  def jsonStreamWriter[T](implicit writer: JsonStreamWriter[T]) = writer
  def jsonStreamFormat[T](implicit format: JsonStreamFormat[T]) = format

  def deserializationError(msg: String, cause: Throwable = null) = throw new DeserializationException(msg, cause)
  def serializationError(msg: String) = throw new SerializationException(msg)
}
package streaming {
  class DeserializationException(msg: String, cause: Throwable = null) extends RuntimeException(msg, cause)
  class SerializationException(msg: String) extends RuntimeException(msg)
}
